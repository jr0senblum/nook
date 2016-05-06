%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@jims-mbp.jhs.local>
%%% @copyright (C) 2016, Jim Rosenblum
%%% @doc Nook top level, simple_one_for_one supervisor. Spins up a nook_note 
%%% process on demand. 
%%% @end
%%% Created :  1 Apr 2016 by Jim Rosenblum <jrosenblum@jims-mbp.jhs.local>
%%% ----------------------------------------------------------------------------
-module(nook_sup).
-behaviour(supervisor).


% API
-export([start_link/0,
         start_nook/1,
         get_credentials/0]).



% Supervisor callbacks
-export([init/1]).


-define(SERVER, ?MODULE).

-record (creds, 
         {expiration, 
          access_key,
          secret_key,
          token}).

%% =============================================================================
%% API 
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc Start a nook process per the parameters in the map.
%% The process is not part of a supervision tree, because I cannot find any
%% value in doing so.
%%
-spec start_nook(NookDescription::#{}) -> {ok, pid()}.

start_nook(NookDescription) ->
     nook_note:start(NookDescription).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).



%% =============================================================================
%% Supervisor callbacks
%% =============================================================================


init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 60,
                 period => 300},

    ChildSpec = #{id => nook_cleanup,
                  start => {nook_cleanup, start_link, []}, 
                  restart => permanent, 
                  shutdown => 1000, 
                  type => worker, 
                  modules => [nook_cleanup]},

    {ok, { SupFlags, [ChildSpec]} }.



-spec get_credentials() -> #creds{}.
get_credentials() ->
    try ets:lookup(nook_cred, cred) of
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
                         [?MODULE, get(Credentials#creds.expiration)]),
            insert_credentials(Credentials),
            Credentials;
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
    ets:give_away(Tid, whereis(?MODULE), []).
