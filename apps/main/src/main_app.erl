%%%-------------------------------------------------------------------
%% @doc tweeter client public API
%% @end
%%%-------------------------------------------------------------------

-module(main_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, []) ->
    main:start().

stop(_State) ->
    ok.

%% internal functions