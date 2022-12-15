%%%-------------------------------------------------------------------
%% @doc tweeter server public API
%% @end
%%%-------------------------------------------------------------------

-module(server_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(WS_LISTENER, websocket_listener).

start(_StartType, _StartArgs) ->
    % initialize DB
    store:init(),
    % start server
    Dispatch = cowboy_router:compile([
        {'_', [{"/ws", tweeter, #{}}]}
    ]),
    cowboy:start_clear(
        ?WS_LISTENER,
        [{port, 5000}],
        #{env => #{dispatch => Dispatch}}
    ),
    server_sup:start_link().

stop(_State) ->
    store:cleanup(),
    ok = cowboy:stop_listener(?WS_LISTENER).

%% internal functions
