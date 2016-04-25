%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@jims-mbp.jhs.local>
%%% @copyright (C) 2016, Jim Rosenblum
%%% @doc Library module that interacts with DynamoDb to persist, update and
%%% retrieve note.
%%% @end
%%% Created :  20 Apr 2016 by Jim Rosenblum <jrosenblum@jims-mbp.jhs.local>
%%% ----------------------------------------------------------------------------
-module(nook_store).


% Public API
-export([delete/1,
         delete_old/0,
         get/1,
         update/1,
         put/4]).


-type store_errors() :: {error, missing_note} | {error, {storage, term()}}.
-export_type([store_errors/0]).



%%% ============================================================================
%%%                               API
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% Create the dybamoDb entry.
%%
-spec put(Key, Content, TTL, Gets) ->  {ok, #{}} | store_errors() when
      Key     :: nook:key(),
      Content :: nook:contents(),
      TTL     :: nook:ttl(),
      Gets    :: nook:gets().
                             
put(Key, Content, TTL, Gets) ->
    set_endpoint(),
    HashedKey = hash_key(Key),
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    Item = 
        #{<<"TableName">> => <<"Notes">>,
          <<"Item">> => 
              #{<<"HKey">> => #{<<"S">> => HashedKey},
                <<"Contents">> => #{<<"B">> => encrypt(Key, Content)},
                <<"Gets">> => #{<<"N">> => convert(Gets)},
                <<"TimeTL">> => #{<<"N">> => convert(TTL)},
                <<"Expiration">> => #{<<"N">> => convert(TTL + Now)},
                <<"Created">> => #{<<"N">> => convert(Now)}
               }
         },
    case erldyn:put_item(jsone:encode(Item)) of
        {ok, _} = Result ->
            Result;
        {error, #{<<"message">> := Message}} ->
            {error, {storage_error, Message}}
    end.


%% -----------------------------------------------------------------------------
%% Get the item from dynamoDb, convert result into expected map.
%%
-spec get(nook:key()) -> {ok, #{}} | store_errors().

get(Key) ->
    set_endpoint(),
    HashedKey = hash_key(Key),

    Select = #{<<"TableName">>  => <<"Notes">>,
             <<"Key">> => 
                   #{<<"HKey">> => #{<<"S">> => HashedKey}}},
    case erldyn:get_item(jsone:encode(Select)) of
        {ok, #{<<"Item">> := 
                   #{<<"Contents">> := #{<<"B">> := Encrypted},
                     <<"Gets">> := #{<<"N">> := Gets},
                     <<"TimeTL">> := #{<<"N">> := TTL},
                     <<"Created">> := #{<<"N">> := Created}}}} ->
            {ok, #{contents => decrypt(Key, Encrypted),
                   gets => convert(Gets),
                   ttl => convert(TTL),
                   created => convert(Created)}};
        {ok, #{}} ->
            {error, missing_note};
        {error, #{<<"message">> := Message}} ->
            {error, {storage_error, Message}}
        end.


%% -----------------------------------------------------------------------------
%% Delete the item from dynamoDb.
%%
-spec delete(nook:key()) -> ok.

delete(Key) ->
    set_endpoint(),
    HashedKey = hash_key(Key),

    Select = #{<<"TableName">>  => <<"Notes">>,
             <<"Key">> => 
                   #{<<"HKey">> => #{<<"S">> => HashedKey}}},
    erldyn:delete_item(jsone:encode(Select)),
    ok.


%% -----------------------------------------------------------------------------
%% Delete items from dynamoDb whose TTL has expired.
%%
-spec delete_old() -> ok.

delete_old() ->
    set_endpoint(),
    Now = integer_to_binary(calendar:datetime_to_gregorian_seconds(calendar:universal_time())),


    Select = #{<<"TableName">> => <<"Notes">>,
               <<"FilterExpression">> => <<"Expiration < :now">>,
               <<"ExpressionAttributeValues">> => #{<<":now">> => #{<<"N">> => Now}}
              },
 
    case erldyn:scan(jsone:encode(Select)) of
        {ok, Candidates} -> delete_found(Candidates);
        _ ->
            ok
    end.


delete_found(Candidates) ->
    case jwalk:get({"Items","HKey","S"}, Candidates) of
        undefined -> 
            ok;
        Keys ->
            [delete_(K) || K <- Keys],
            ok
    end,
    case jwalk:get({"LastEvaluatedKey"}, Candidates) of
        undefined ->
            ok;
        _  ->
            delete_old()
    end.


delete_(Key) ->

    Select = #{<<"TableName">>  => <<"Notes">>,
             <<"Key">> => 
                   #{<<"HKey">> => #{<<"S">> => Key}}},
    erldyn:delete_item(jsone:encode(Select)),
    ok.



%% -----------------------------------------------------------------------------
%% Decriment the Gets count if it is greater than 0. -1 indicates infinite and
%% it shouldn't ever get below 0 because at 0 it should be deleted.
%%
-spec update(nook:key()) -> {ok, #{}} | store_errors().

update(Key) ->
    set_endpoint(),
    HashedKey = hash_key(Key),

    Select = 
        #{<<"TableName">> => <<"Notes">>,
          <<"Key">> => 
                   #{<<"HKey">> => #{<<"S">> => HashedKey}},
          <<"UpdateExpression">> => <<"set Gets = Gets - :num">>,
          <<"ConditionExpression">> => <<"Gets > :zero">>,
          <<"ExpressionAttributeValues">> => #{<<":num">> => #{<<"N">> => <<"1">>},
                                               <<":zero">> => #{<<"N">> => <<"0">>}

                                              },
          <<"ReturnValues">> => <<"UPDATED_NEW">>
         },
    case erldyn:update_item(jsone:encode(Select)) of
        {ok, #{<<"Attributes">> := #{<<"Gets">> := #{<<"N">> := N}}}} ->
            {ok, #{gets => convert(N)}};
        {error, #{<<"message">> := <<"The conditional request failed">> = M}} ->
            {ok, #{<<"message">> => M}};
        {error, #{<<"message">> := _}} = Error ->
            {error, Error}
    end.



%%% ============================================================================
%%%                             Internal functions
%%% ============================================================================


% Hash the key, return base64.
hash_key(Key) ->
    base64:encode(hash(Key)).


hash(Thing) ->
    Digest = crypto:hash(sha512, Thing),
    string:to_lower(lists:flatten([[integer_to_list(N, 16) || 
                                       <<N:4>> <= Digest]])).
    

% Encrypt and Decrypt using AES stream encryption
encrypt(Key, Content) ->
    K = uuid:string_to_uuid(Key),
    State = crypto:stream_init(aes_ctr, <<K/binary, K/binary>>, K),
    {_NewState, Encrypted} = crypto:stream_encrypt(State, Content),
    base64:encode(Encrypted).


decrypt(Key, Encrypted) ->
    K = uuid:string_to_uuid(Key),
    State = crypto:stream_init(aes_ctr, <<K/binary, K/binary>>, K),
    {_NewState, Decrypted} =
        crypto:stream_decrypt(State, base64:decode(Encrypted)),
    Decrypted.



% infinite <--> <<"-1">>, N <--> <<"N">>. 
convert(infinite) ->
    <<"-1">>;
convert(<<"-1">>) ->
    infinite;
convert(N) when is_binary(N) ->
    binary_to_integer(N);
convert(N) ->
    integer_to_binary(N).

set_endpoint()->
    {ok, Ep} = application:get_env(nook, endpoint),
    erldyn:config(#{endpoint => Ep}).
