-module(tcs_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, [N]) ->
    case tcs_sup:start_link(N) of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.
