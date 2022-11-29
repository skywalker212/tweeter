-module(tweeter_client).
-behaviour(gen_server).

-export([start_link/1]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).
-record(state, {user_id = 0}).

start_link(UserID) ->
    gen_server:start_link(?MODULE, [UserID], []).

init([UserID]) ->
    State = #state{user_id = UserID},
    gen_server:call(tweeter, {register, State#state.user_id}),
    {ok, State}.

handle_call(Msg, _From, State) ->
    gen_server:call(tweeter, Msg),
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({ok, register_account, _}, #state{user_id = UserID} = State) ->
    io:format("[~p] account registered~n", [UserID]),
    {noreply, State};
handle_info(Msg, State) ->
    io:format("[~p] ~p~n", [self(), Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, _State, _Extra) ->
    ok.
