%%%-------------------------------------------------------------------
%% @doc tweeter client public API
%% @end
%%%-------------------------------------------------------------------

-module(client_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, [N]) ->
    % create table to store client pids
    ets:new(client_pid, [
        public,
        named_table,
        {write_concurrency, auto},
        {read_concurrency, true}
    ]),
    % store words to be used in tweets in an ETS table
    generator:save_words(),
    client_sup:start_link(N).

stop(_State) ->
    ets:delete(client_pid),
    ok.

%% internal functions
