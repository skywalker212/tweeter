-module(c_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, []) ->
    {ok, N} = application:get_env(total_clients),
    case c_sup:start_link(N) of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.
