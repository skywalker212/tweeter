-module(c_app).
-behaviour(application).
-export([
    start/2, 
    stop/1
]).

start(_StartType, _StartArgs) ->
    c_store:init(),
    case c_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.