%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@jims-mbp.jhs.local>
%%% @copyright (C) 2016, Jim Rosenblum
%%% @doc Library module that stores, retrieves and refreshes iam credentials
%%% which are used by erldyn/dynamoDB.
%%% @end
%%% Created :  20 Apr 2016 by Jim Rosenblum <jrosenblum@jims-mbp.jhs.local>
%%% ----------------------------------------------------------------------------
-module(nook_creds).


-export([get_credentials/0]).

% definition of the credential record.
-include("../include/records.hrl").



%%% ============================================================================
%%%                         API
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% Return the encrypted, temporary AWS credentials, else error.
%%
-spec get_credentials() -> #creds{} | error | {error, {term(), term()}}.

get_credentials() ->
    case using_iam() of
        true ->
            get_iam_credentials();
        false ->
            error
end.            


get_iam_credentials() ->
    case retrieve_credentials() of
        {ok, Credentials} ->
            case is_current(Credentials) of
                true -> 
                    Credentials;
                false -> 
                    fetch_and_store_credentials()
            end;
        _ ->
            % missing or ets access error of some sort
            fetch_and_store_credentials()
    end.


retrieve_credentials() ->
    try ets:lookup(nook_cred, creds) of
        [#creds{} = Credentials] ->
            {ok, Credentials};
        [] ->
            missing
    catch
        _:_  ->
            error
    end.


is_current(#creds{expiration = Exp}) ->
    Now = calendar:universal_time(),
    Expiration = remove_milliseconds(ec_date:parse(Exp)),
    Expiration > Now.


remove_milliseconds({_YMD, {_, _, _}} = DateTime) ->
    DateTime;
remove_milliseconds({YMD, {H, M, S, _MS}}) ->
    {YMD, {H, M, S}}.


using_iam() ->
    application:get_env(nook, iam, false).


% ------------------------------------------------------------------------------
% Hit the AIM URL, convert result into structure, persist into ets.
%
fetch_and_store_credentials() ->
    case pull_from_aws() of
        error ->
            error;
        Map ->
            Credentials = structure_from_map(Map),
            lager:notice("~p: fetched new credentials that expire ~p.",
                         [?MODULE, Credentials#creds.expiration]),
            store_credentials(Credentials)
    end.


pull_from_aws() ->
    EndPoint = get_endpoint(),
    case httpc:request(EndPoint) of
        {ok, {{_, 200,"OK"}, _, Result}} ->
            jsone:decode(list_to_binary(Result));
        _ ->
            error
end.


store_credentials(Credentials) ->
    try 
        ets:insert(nook_cred, Credentials),
        Credentials
    catch
        error:badarg ->
            create_cred_table(),
            store_credentials(Credentials);
        T:E ->
            {error, {T,E}}
    end.


% ------------------------------------------------------------------------------
% Instantiate a #creds{} with everything but the expiration encrypted.
%
structure_from_map(Map) ->
    #creds{expiration = binary_to_list(maps:get(<<"Expiration">>, Map)),
           access_key = encrypt(binary_to_list(maps:get(<<"AccessKeyId">>, Map))),
           secret_key = encrypt(binary_to_list(maps:get(<<"SecretAccessKey">>, Map))),
           token = encrypt(binary_to_list(maps:get(<<"Token">>, Map)))}.


encrypt(PlainText) ->
    X = get_key(),
    State = crypto:stream_init(aes_ctr, X, X),
    {_, CipherText} = crypto:stream_encrypt(State, PlainText),
    CipherText.


% ------------------------------------------------------------------------------
% AIM endpoint from environment + associated role.
%
get_endpoint() -> 
    {ok, AIMEp} = application:get_env(nook, metadata),
    AIMEp ++ "NookRole".


% ------------------------------------------------------------------------------
% get / set the encryption key.
%
get_key() ->
    case application:get_env(nook, left) of
        undefined ->
            Key = uuid:get_v4(),
            application:set_env(nook, left, Key),
            Key;
        {ok, Value} ->
            Value
end.


% ------------------------------------------------------------------------------
% The ets table is public, so encrypt whats in there. But this allows many
% processes to appropriately share credentials.
%
create_cred_table() ->
    Tid = ets:new(nook_cred, [set, 
                        named_table, 
                        public, 
                        {read_concurrency, true}, 
                        {write_concurrency, false}]),
    ets:give_away(Tid, whereis(nook_sup), []).




