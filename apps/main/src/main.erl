-module(main).
-behaviour(gen_server).

-export([
    start/0,
    start/1,
    start/2
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

%% number of milliseconds for which to run the simulation
-define(DEFAULT_RUN_TIME, 5000).

-define(DEFAULT_NO_OF_CLIENTS, 10).

-define(SERVER_APP_NAME, server).

-define(CLIENT_APP_NAME, client).

-record(state, {
    request_times = dict:new(),
    total_clients = ?DEFAULT_NO_OF_CLIENTS,
    terminated_clients = []
}).

%% API
start() ->
    start(?DEFAULT_NO_OF_CLIENTS, ?DEFAULT_RUN_TIME).
start(NoOfClients) ->
    start(NoOfClients, ?DEFAULT_RUN_TIME).
start(NoOfClients, RunTime) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [NoOfClients, RunTime], []).

%% GEN SERVER IMPLEMENTATION
init([NoOfClients, RunTime]) ->
    application:start(?SERVER_APP_NAME),
    io:format("Started server application~n"),
    application:start(?CLIENT_APP_NAME),
    io:format("Started client simulator with ~B clients, simulator will stop after ~B seconds~n", [
        NoOfClients, erlang:convert_time_unit(RunTime, millisecond, second)
    ]),
    erlang:send_after(RunTime, self(), {'$gen_cast', stop}),
    {ok, #state{total_clients = NoOfClients}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(
    {disconnect, _UserID, _StartTime, IncomingRequestTimesDict},
    #state{
        request_times = RequestTimesDict
    } = State
) ->
    UpdatedRequestTimesDict = dict:merge(
        fun(_, List1, List2) -> List1 ++ List2 end, IncomingRequestTimesDict, RequestTimesDict
    ),
    {noreply, State#state{request_times = UpdatedRequestTimesDict}};
handle_cast(
    {shutdown, UserID, _StartTime, IncomingRequestTimesDict},
    #state{
        request_times = RequestTimesDict,
        total_clients = TotalClients,
        terminated_clients = TerminatedClients
    } = State
) ->
    UpdatedRequestTimesDict = dict:merge(
        fun(_, List1, List2) -> List1 ++ List2 end, IncomingRequestTimesDict, RequestTimesDict
    ),
    case length(TerminatedClients) + 1 of
        TotalClients ->
            %% Report the statistics and exit
            AverageTimes = dict:fold(
                fun(K, List, OutputList) -> [{K, lists:sum(List) / length(List)} | OutputList] end,
                [],
                UpdatedRequestTimesDict
            ),
            io:format("----------------- Average Operation Times ------------------~n"),
            lists:foreach(
                fun({K, V}) -> io:format("~-20s~-15B nanoseconds~n", [K, trunc(V)]) end,
                AverageTimes
            ),
            io:format("------------------------------------------------------------~n"),
            {stop, normal, State};
        _ ->
            {noreply, State#state{
                request_times = UpdatedRequestTimesDict,
                terminated_clients = [UserID | TerminatedClients]
            }}
    end;
handle_cast(stop, State) ->
    io:format("~n~nEnding simulation, sending signal to stop client and server app...~n~n"),
    application:stop(?CLIENT_APP_NAME),
    application:stop(?SERVER_APP_NAME),
    {noreply, State}.

handle_info(timeout, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {stop, normal, State}.
code_change(_OldVsn, State, _Extra) ->
    {stop, normal, State}.
