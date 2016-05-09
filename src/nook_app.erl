%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@jims-mbp.jhs.local>
%%% @copyright (C) 2016, Jim Rosenblum
%%% @doc Nook application file - fires up the supervisor.
%%% @end
%%% Created :  1 Apr 2016 by Jim Rosenblum <jrosenblum@jims-mbp.jhs.local>
%%% ----------------------------------------------------------------------------
-module(nook_app).
-behaviour(application).


%% Application callbacks.
-export([start/2,
         stop/1]).



%% =============================================================================
%% API
%% =============================================================================


%% -----------------------------------------------------------------------------
-spec start(atom(), application:restart_tupe()) -> ok | {error, term()}.

start(_StartType, _StartArgs) ->
    lager:start(),

    erldyn:config(),
    nook_sup:start_link().


%% -----------------------------------------------------------------------------
-spec stop(atom()) -> ok.

stop(_State) ->
    ok.

