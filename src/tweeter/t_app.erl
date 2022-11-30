-module(t_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    t_store:init(),
    % start server
    case tweeter:start_link() of
        {ok, Pid} ->
            %% temporary line below TODO: remove it
            % start client simulator.
            {ok, _} = tcs_sup:start_link(10),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.
