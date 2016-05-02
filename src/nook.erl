%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@jims-mbp.jhs.local>
%%% @copyright (C) 2016, Jim Rosenblum
%%% @doc Library module that acts as the client interface to Nook. Notes are
%%% binary messages, currently stored in DynamoDB ,with some meta data defining
%%% their life-span.
%%%
%%% Once a message is created, it is retrieved via unique key., Don't have the
%%% key, don't get the message.
%%% @end
%%% Created :  1 Apr 2016 by Jim Rosenblum <jrosenblum@jims-mbp.jhs.local>
%%% ----------------------------------------------------------------------------
-module(nook).


%% API
-export([decriment/1,
         destroy/1,
         expiration/1,
         exists/1,
         get/1, 
         gets/1,
         new/3]).


-type key()        :: string().
-type contents()   :: binary().
-type ttl()        :: non_neg_integer() | infinite.
-type gets()       :: non_neg_integer() | infinite.
-type errors()     :: {error, badarg} | nook_store:store_errors().
-export_type([key/0, contents/0, ttl/0, gets/0]).


% Notes must not live forever: one of TTL or Retrievals must be a 
% a positive integers.
-define (VALID_P(N), (N == infinite) or (is_integer(N) andalso N > 0)).
-define (VALID(T,R), ?VALID_P(T) andalso 
                     ?VALID_P(R) andalso 
                     (T /= infinite orelse R /= infinite)).



%%% ============================================================================
%%% API
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% @doc Destroy the note associated with the key. Return ok or {error, badarg}.
%%
-spec destroy(key()) -> ok | {error, badarg}.

destroy(Key) when is_list(Key) -> 
    nook_store:delete(Key),
    ok;

destroy(_Key) -> 
    {error, badarg}.


%% -----------------------------------------------------------------------------
%% @doc Return the expiration datetime or undefined if the TTL is infinite.
%%
-spec expiration(key()) -> {ok, undefined | erlang:datetime()} | errors().

expiration(Key) when is_list(Key) ->

    case nook_store:n_get(Key) of
        {ok, #{ttl := infinite}} ->
            {ok, undefined};
        {ok, #{ttl := TTL, created := Created}} ->
            {ok, calendar:gregorian_seconds_to_datetime(Created + TTL)};
        {error, _} = Error ->
            Error
    end;

expiration(_) ->
    {error, badarg}.


%% -----------------------------------------------------------------------------
%% @doc Return true if a note exists under the given key, else false.
%%
-spec exists(key()) -> {ok, true | fase} | errors().

exists(Key) when is_list(Key) ->
    case nook_store:n_get(Key) of 
        {ok, _} ->
            true;
        {error, missing_note} -> 
            false;
        {error, _} = Error ->
            Error
    end;

exists(_) ->
    {error, badarg}.


%% ------------------------------------------------------------------------------
%% @doc Return the contents of the note specificed by the Key.
%% 
-spec get(key()) -> {ok, map()} | errors().

get(Key) when is_list(Key) ->
    nook_store:n_get(Key);

get(_Key) ->
    {error, badarg}.


%% ------------------------------------------------------------------------------
%% @doc Decriment the count of allowable gets, delete if 0.
%% 
-spec decriment(key()) -> {ok, destroyed|non_neg_integer()|infinite} | errors().

decriment(Key) when is_list(Key) ->
    case nook_store:update(Key) of
        {ok, #{gets := 0}} ->
            destroy(Key),
            {ok, destroyed};
        {ok, #{gets := N}} ->
            {ok, N};
        {ok, _} ->
            {ok, infinite};
        {error, _} = Result ->
            Result
    end;

decriment(_Key) ->
    {error, badarg}.



%% -----------------------------------------------------------------------------
%% @doc Return the number of retrievals allowed or infinite.
%%
-spec gets(key()) -> {ok, infinite | pos_integer()} | errors().

gets(Key) when is_list(Key) ->
    case nook_store:n_get(Key) of
        {ok, #{gets := infinite}} ->
            {ok, infinite};
        {ok, #{gets := Gets}} ->
            {ok, Gets};
        {error, _} = Error ->
            Error
    end;

gets(_Key) ->
    {error, badarg}.



%% -----------------------------------------------------------------------------
%% @doc Create a Note with a supplied time to live, and retrieval limit on
%% each node.
%%
-spec new(contents(), ttl(), gets()) -> key() | {error, badarg | term()}.

new(<<_/binary>> = Note, TTL, Gets) when ?VALID(TTL, Gets) ->
    % User's Key
    Key = uuid:uuid_to_string(uuid:get_v4()),
    case nook_store:put(Key, Note, TTL, Gets) of
        {ok, #{}} ->
            Key;
        {error, #{<<"message">> := Message}} ->
            {error, Message}
    end;

new(_N, _T, _R) ->
    {error, badarg}.
