-module(tweeter).
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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call(_Msg, From, State) ->
    {ok, Handler} = t_worker:start_link(),
    gen_server:cast(Handler, {test, From}),
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, _State, _Extra) ->
    ok.
