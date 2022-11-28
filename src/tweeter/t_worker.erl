-module(t_worker).
-behaviour(gen_server).
-export([start_link/0]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    terminate/2,
    code_change/3
]).
-record(state, {}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call(_Msg, _From, _State) ->
    ok.

handle_cast({test, From}, State) ->
    gen_server:reply(From, {ok, test}),
    {stop, normal, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, _State, _Extra) ->
    ok.