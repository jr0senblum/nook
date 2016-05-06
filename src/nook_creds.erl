%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@jims-mbp.jhs.local>
%%% @copyright (C) 2016, Jim Rosenblum
%%% @doc Library module that stores, retrieves and refreshes iam credentials.
%%% @end
%%% Created :  20 Apr 2016 by Jim Rosenblum <jrosenblum@jims-mbp.jhs.local>
%%% ----------------------------------------------------------------------------
-module(nook_creds).

-export([get_credentials/0]).

-record (creds, 
         {expiration, 
          access_key,
          secret_key,
          token}).


%% -----------------------------------------------------------------------------
%% Return the Encrypted, temporary AWS credentials if it can, else error.
%%
-spec get_credentials() -> #creds{} | error.

get_credentials() ->
    case retrieve_credentials() of
        {ok, Credentials} ->
            case current(Credentials) of
                true -> 
                    Credentials;
                false -> 
                    construct_credentials()
            end;

        _ ->
            % missing or ets access error of some sort
            construct_credentials()
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


construct_credentials() ->
    {ok, IamEp} = application:get_env(nook, metadata),
    case httpc:request(IamEp ++ "NookRole") of
        {ok, {{_, 200,"OK"}, _, Result}} ->
            RMap = jsone:decode(list_to_binary(Result)),
            Credentials = #creds{expiration = binary_to_list(maps:get(<<"Expiration">>, RMap)),
                                 access_key = binary_to_list(maps:get(<<"AccessKeyId">>, RMap)),
                                 secret_key = binary_to_list(maps:get(<<"SecretAccessKey">>, RMap)),
                                 token = binary_to_list(maps:get(<<"Token">>, RMap))},
            lager:notice("~p: fetched new credentials that expire ~p.",
                         [?MODULE, Credentials#creds.expiration]),
            Encoded = encode(Credentials),
            insert_credentials(Encoded),
            Encoded;
        _ ->
            error
    end.


insert_credentials(Credentials) ->
    try 
        ets:insert(nook_cred, Credentials)
    catch
        error:badarg ->
            create_cred_table(),
            ets:insert(nook_cred, Credentials);
        T:E ->
            {error, {T, E}}
    end.


encode(#creds{expiration = E, access_key = AK, secret_key = SK, token = T}) ->
    #creds{expiration = E,
           access_key = encrypt(AK),
           secret_key = encrypt(SK),
           token = encrypt(T)}.

encrypt(PlainText) ->
    X = get_key(),
    State = crypto:stream_init(aes_ctr, X, X),
    {_, CipherText} = crypto:stream_encrypt(State, PlainText),
    CipherText.

get_key() ->
    case application:get_env(nook, left) of
        undefined ->
            Key = uuid:get_v4(),
            application:set_env(nook, left, Key),
            Key;
        {ok, Value} ->
            Value
end.


create_cred_table() ->
    Tid = ets:new(nook_cred, [set, 
                        named_table, 
                        public, 
                        {read_concurrency, true}, 
                        {write_concurrency, false}]),
    ets:give_away(Tid, whereis(nook_sup), []).


current(#creds{expiration = Exp}) ->
    Now = calendar:universal_time(),
    Expiration = remove_milliseconds(ec_date:parse(Exp)),
    Expiration > Now.


remove_milliseconds({_YMD, {_, _, _}} = DateTime) ->
    DateTime;

remove_milliseconds({YMD, {H, M, S, _MS}}) ->
    {YMD, {H, M, S}}.


