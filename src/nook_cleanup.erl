%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@Jims-MBP.attlocal.net>
%%% @copyright (C) 2016, Jim Rosenblum
%%% @doc Every interval, remove items who have exceeded their
%%% TTL. Interval is defined in the config.sys as cleanup_sec.
%%% @end
%%% Created : 24 Apr 2016 by Jim Rosenblum <jrosenblum@Jims-MBP.attlocal.net>
%%% ----------------------------------------------------------------------------
-module(nook_cleanup).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

-define(SERVER, ?MODULE).
-define(CLEANUP_SEC, 60).

-record(state, {cleanup_ms:: non_neg_integer()}).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->
    Cleanup_sec = application:get_env(nook, cleanup_sec, ?CLEANUP_SEC),
    Cleanup_ms = Cleanup_sec * 1000,
    erlang:send_after(Cleanup_ms, ?MODULE, delete_old),
    lager:info("~p: starting cleanup process with a ~p interval.", 
               [?MODULE, Cleanup_sec]),
    {ok, #state{cleanup_ms=Cleanup_ms}}.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(delete_old, #state{cleanup_ms=Int} = State) ->
    lager:notice("~p: deleting expired messages.",[?MODULE]),
    nook_store:delete_old(),
    erlang:send_after(Int, ?MODULE, delete_old),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

