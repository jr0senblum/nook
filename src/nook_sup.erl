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
         start_nook/1]).

% Supervisor callbacks
-export([init/1]).


-define(SERVER, ?MODULE).



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
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},

    ChildSpec = #{id => nook_sup,
                  start => {nook_note, start_link, []}, 
                  restart => temporary, 
                  shutdown => 1000, 
                  type => worker, 
                  modules => [nook_note]},

    {ok, { SupFlags, [ChildSpec]} }.

