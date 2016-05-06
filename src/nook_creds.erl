

-module(nook_creds).

-export([get_credentials/0]).

-record (creds, 
         {expiration, 
          access_key,
          secret_key,
          token}).



-spec get_credentials() -> #creds{}.
get_credentials() ->
    try ets:lookup(nook_cred, creds) of
        [#creds{expiration = Exp} = Credentials] ->
            Now = calendar:universal_time(),
            case (ec_date:parse(Exp) > Now) of
                true ->
                    Credentials;
                false ->
                    refresh_credentials()
            end;
        _ ->
            refresh_credentials()
    catch
        _:_  ->
            refresh_credentials()
    end.

            
refresh_credentials() ->
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

create_cred_table() ->
    Tid = ets:new(nook_cred, [set, 
                        named_table, 
                        public, 
                        {read_concurrency, true}, 
                        {write_concurrency, false}]),
    ets:give_away(Tid, whereis(nook_sup), []).




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
