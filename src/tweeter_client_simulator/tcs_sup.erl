-module(tcs_sup).
-behaviour(supervisor).
-export([
    start_link/1
]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link(N) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [N]).

init([N]) ->
    Children = lists:map(
        fun(X) ->
            ID = integer_to_list(X),
            {
                "tweeter_client" ++ ID,
                {tweeter_client, start_link, [ID, N]},
                transient,
                brutal_kill,
                worker,
                [
                    tweeter_client
                ]
            }
        end,
        lists:seq(1, N)
    ),
    RestartStrategy = #{},
    {ok, {RestartStrategy, Children}}.
