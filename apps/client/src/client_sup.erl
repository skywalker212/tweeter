%%%-------------------------------------------------------------------
%% @doc tweeter client top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(client_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(N) ->
    supervisor:start_link(?MODULE, [N]).

init([N]) ->
    ChildSpecs = lists:map(
        fun(X) ->
            ID = integer_to_list(X),
            #{
                id => "client" ++ ID,
                start => {client, start_link, [ID, N]}
            }
        end,
        lists:seq(1, N)
    ),
    SupFlags = #{intensity => N},
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
