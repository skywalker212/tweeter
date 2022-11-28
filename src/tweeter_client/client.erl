-module(client).
-behaviour(gen_server).

-export([start_link/0]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).
-record(state, {user_id = util:generate_string()}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    State = #state{},
    gen_server:call(tweeter, {register, State#state.user_id}),
    {ok, State}.

handle_call(Msg, _From, State) ->
    gen_server:call(tweeter, Msg),
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({ok, register_account, _}, State) ->
    io:format("account registered"),
    {noreply, State};
handle_info(Msg, State) ->
    io:format("[~p] ~p~n", [self(), Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, _State, _Extra) ->
    ok.
