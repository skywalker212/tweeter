-module(tcs_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, [N]) ->
    % start the backend
    ok = application:start(tweeter),
    % start the simulator
    case tcs_sup:start_link(N) of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.
