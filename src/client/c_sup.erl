-module(c_sup).
-behaviour(supervisor).
-export([
    start_link/1
]).

-export([init/1]).
-define(SERVER, ?MODULE).

%% API
start_link(N) ->
    % store words to be used in tweets in an ETS table
    generator:save_words(),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [N]).

%% supervisor implementation
init([N]) ->
    Children = lists:map(
        fun(X) ->
            ID = integer_to_list(X),
            #{
                id => "client" ++ ID,
                start => {client, start_link, [ID, N]},
                restart => transient
            }
        end,
        lists:seq(1, N)
    ),
    RestartStrategy = #{},
    {ok, {RestartStrategy, Children}}.
