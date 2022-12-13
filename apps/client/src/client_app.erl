%%%-------------------------------------------------------------------
%% @doc tweeter client public API
%% @end
%%%-------------------------------------------------------------------

-module(client_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, [N]) ->
    client_sup:start_link(N).

stop(_State) ->
    ok.

%% internal functions
