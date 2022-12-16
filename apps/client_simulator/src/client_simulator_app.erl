%%%-------------------------------------------------------------------
%% @doc tweeter client simulator public API
%% @end
%%%-------------------------------------------------------------------

-module(client_simulator_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, [N]) ->
    c_store:init(),
    client_simulator_sup:start_link(N).

stop(_State) ->
    c_store:cleanup(),
    ok.

%% internal functions
