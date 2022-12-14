-module(client).
-behaviour(gen_server).

-export([start_link/2]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% startup time in miliseconds to wait other clients to register themselves before performing actions
-define(START_TIME, 500).
%% Interval at which to publish tweet/retweet
-define(TWEET_INTERVAL, 100).
%% Interval at which to query
-define(QUERY_INTERVAL, 500).

-ifdef(prod).
-define(PRINT(S, A), ok).
-else.
-define(PRINT(S, A), io:format(S, A)).
-endif.

-record(state, {
    n = 1,
    total_followers = 0,
    followers = [],
    user_id,
    start_time = erlang:system_time(),
    pending_requests = [],
    request_times = [],
    conn_pid,
    stream_ref
}).

%% API
start_link(UserID, N) ->
    gen_server:start_link(?MODULE, [UserID, N], []).

%% GEN SERVER IMPLEMENTATION
init([UserID, N]) ->
    process_flag(trap_exit, true),
    IntID = list_to_integer(UserID),
    {ok, ConnPid} = gun:open("localhost", 5000),
    State = #state{
        n = N,
        user_id = UserID,
        total_followers = trunc(math:floor((N * 2) / (IntID * 3))),
        conn_pid = ConnPid
    },
    {ok, State}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(
    {follow, FollowerID}, #state{conn_pid = ConnPid, stream_ref = StreamRef, pending_requests = PendingRequests} = State
) ->
    RequestID = send_ws_message(ConnPid, StreamRef, #{follow => #{follow_id => FollowerID}}),
    {noreply, State#state{pending_requests = [RequestID | PendingRequests]}};
handle_cast(
    {schedule_tweet},
    #state{
        n = N,
        total_followers = TotalFollowers,
        pending_requests = PendingRequests,
        conn_pid = ConnPid,
        stream_ref = StreamRef
    } = State
) ->
    % code to make the users with more followers tweet more
    case rand:uniform(N) - 1 of
        X when X =< TotalFollowers ->
            % make the tweet a retweet with 25% chance
            case rand:uniform(4) of
                4 ->
                    % retweet the last tweet, may happen that we retweet someone's retweet but it's fine for our simulation purposes
                    case store:get_last_tweet_id() of
                        null -> Request = [];
                        ID -> Request = [send_ws_message(ConnPid, StreamRef, #{re_tweet => #{ tweet_id => ID }})]
                    end;
                _ ->
                    Request = [send_ws_message(ConnPid, StreamRef, #{tweet => #{content => generator:generate_tweet(N)}})]
            end;
        _ ->
            Request = []
    end,
    % schedule next tweet
    schedule_tweet(),
    {noreply, State#state{pending_requests = PendingRequests ++ Request}};
handle_cast(
    {tweet, TweetContent}, #state{conn_pid = ConnPid, stream_ref = StreamRef, pending_requests = PendingRequests} = State
) ->
    TweetRequest = send_ws_message(ConnPid, StreamRef, #{tweet => #{content => TweetContent}}),
    {noreply, State#state{pending_requests = [TweetRequest | PendingRequests]}};
handle_cast(
    {re_tweet, TweetID}, #state{conn_pid = ConnPid, stream_ref = StreamRef, pending_requests = PendingRequests} = State
) ->
    RetweetRequest = send_ws_message(ConnPid, StreamRef, #{re_tweet => #{ tweet_id => TweetID }}),
    {noreply, State#state{pending_requests = [RetweetRequest | PendingRequests]}};
handle_cast(
    {schedule_mention_query}, #state{conn_pid = ConnPid, stream_ref = StreamRef, pending_requests = PendingRequests} = State
) ->
    % make a mention query with 10% chance
    case rand:uniform(10) of
        1 -> Request = [send_ws_message(ConnPid, StreamRef, #{mentions => true})];
        _ -> Request = []
    end,
    % schedule next query
    schedule_mention_query(),
    {noreply, State#state{pending_requests = PendingRequests ++ Request}};
handle_cast({mentions}, #state{conn_pid = ConnPid, stream_ref = StreamRef, pending_requests = PendingRequests} = State) ->
    MentionRequest = send_ws_message(ConnPid, StreamRef, #{mentions => true}),
    {noreply, State#state{pending_requests = [MentionRequest | PendingRequests]}};
handle_cast({query, Query}, #state{conn_pid = ConnPid, stream_ref = StreamRef, pending_requests = PendingRequests} = State) ->
    QueryRequest = send_ws_message(ConnPid, StreamRef, #{query => Query}),
    {noreply, State#state{pending_requests = [QueryRequest | PendingRequests]}}.

%% web socket connection establishment
handle_info(
    {gun_up, ConnPid, _},
    #state{conn_pid = ConnPid} = State
) ->
    StreamRef = gun:ws_upgrade(ConnPid, "/ws"),
    {noreply, State#state{stream_ref = StreamRef}};
handle_info(
    {gun_upgrade, _, _, [<<"websocket">>], _},
    #state{user_id = _UserID} = State
) ->
    {noreply, State};
%% user registration after connection establishment
handle_info(
    {gun_ws, ConnPid, StreamRef, {text, <<"ack">>}},
    #state{
        user_id = UserID,
        conn_pid = ConnPid,
        stream_ref = StreamRef,
        pending_requests = PendingRequests
    } = State
) ->
    RequestID = send_ws_message(ConnPid, StreamRef, #{register => #{user_id => UserID}}),
    {noreply, State#state{pending_requests = [RequestID | PendingRequests]}};
%% acknowledgement messages
handle_info(
    {gun_ws, ConnPid, StreamRef, {text, SerializedJSON}},
    #state{
        user_id = UserID,
        conn_pid = ConnPid,
        stream_ref = StreamRef,
        n = N,
        total_followers = TotalFollowers,
        request_times = RequestTimes,
        pending_requests = PendingRequests
    } = State
) ->
    RequestMap = util:decode_json(SerializedJSON),
    case RequestMap of
        #{<<"error">> := Data} ->
            io:format("Error in request ~p ~n", [Data]),
            {noreply, State};
        #{<<"tweet">> := #{<<"poster_id">> := PosterID, <<"tweet_type">> := TweetType, <<"content">> := TweetContent}} ->
            ?PRINT("[~s] ~s received from ~s: ~s~n", [UserID, TweetType, PosterID, TweetContent]),
            {noreply, State};
        #{<<"request_id">> := RequestID, <<"success">> := #{<<"register">> := NewUser}} ->
            {UpdatedRequestTimes, UpdatedPendingRequests} = update_request_times(
                register_account, RequestID, RequestTimes, PendingRequests
            ),
            ?PRINT("[~s, ~p] registered successfully~n", [UserID, RequestID]),
            % TODO: Don't do this for reconnecting client, do this only for newly registered clients
            Followers = case NewUser of
                true ->
                    IntID = list_to_integer(UserID),
                    generate_follow_users(
                        TotalFollowers, lists:delete(IntID, lists:seq(1, N)), []
                    );
                false ->
                    []
            end,
            % store the self pid in table for others to read
            ets:insert(client_pid, {UserID, self()}),
            % wait for START_TIME miliseconds before making users follow current user so that other users have enough time to start up and register themselves
            {noreply,
                State#state{
                    followers = Followers,
                    request_times = UpdatedRequestTimes,
                    pending_requests = UpdatedPendingRequests
                },
                ?START_TIME};
        #{<<"request_id">> := RequestID, <<"success">> := #{<<"follow">> := FollowUserID}} ->
            {UpdatedRequestTimes, UpdatedPendingRequests} = update_request_times(
                follow_user, RequestID, RequestTimes, PendingRequests
            ),
            ?PRINT("[~s, ~p] Followed ~s~n", [UserID, RequestID, FollowUserID]),
            {noreply, State#state{
                request_times = UpdatedRequestTimes, pending_requests = UpdatedPendingRequests
            }};
        #{<<"request_id">> := RequestID, <<"success">> := #{<<"tweet">> := #{<<"type">> := TweetType, <<"id">> := TweetID}}} ->
            {UpdatedRequestTimes, UpdatedPendingRequests} = update_request_times(
                publish_tweet, RequestID, RequestTimes, PendingRequests
            ),
            ?PRINT("[~s, ~p] ~p published with ID ~s~n", [UserID, RequestID, TweetType, TweetID]),
            {noreply, State#state{
                request_times = UpdatedRequestTimes, pending_requests = UpdatedPendingRequests
            }};
        #{<<"request_id">> := RequestID, <<"success">> := #{<<"query">> := Query, <<"result">> := QueryResult}} ->
            {UpdatedRequestTimes, UpdatedPendingRequests} = update_request_times(
                query, RequestID, RequestTimes, PendingRequests
            ),
            ?PRINT("[~s, ~p] Query ~s result ~s~n", [
                UserID,
                RequestID,
                Query,
                lists:map(
                    fun(#{<<"poster_id">> := PosterID, <<"content">> := Content}) -> " {[" ++ PosterID ++ "]: " ++ Content ++ "} " end,
                    QueryResult
                )
            ]),
            {noreply, State#state{
                request_times = UpdatedRequestTimes, pending_requests = UpdatedPendingRequests
            }};
        _ ->
            {noreply, State}
    end;
%% handle initial cooldown timeout before starting the simulation
handle_info(timeout, #state{n = N, user_id = UserID, followers = Followers} = State) ->
    case ets:info(client_pid, size) of
        N ->
            lists:foreach(
                fun(Follower) ->
                    [{_, PID}] = ets:lookup(client_pid, Follower),
                    % Make the Follower follow the current user
                    gen_server:cast(PID, {follow, UserID})
                end,
                Followers
            ),
            schedule_tweet(),
            schedule_mention_query(),
            {noreply, State};
        _ ->
            {noreply, State, ?START_TIME}
    end.

terminate(
    Reason,
    #state{
        user_id = UserID, start_time = StartTime, request_times = RequestTimes, conn_pid = ConnPid
    } = State
) ->
    ets:delete(client_pid, UserID),
    ok = gun:shutdown(ConnPid),
    RequestTimesDict = lists:foldr(
        fun({K, V}, D) -> dict:append(K, V, D) end, dict:new(), RequestTimes
    ),
    TerminationReason =
        case Reason of
            normal -> disconnect;
            Other -> Other
        end,
    gen_server:cast(main, {TerminationReason, UserID, StartTime, RequestTimesDict}),
    {stop, Reason, State}.
code_change(_OldVsn, _State, _Extra) ->
    ok.

%% Utility functions not to be exposed

send_ws_message(ConnPid, StreamRef, Message) ->
    RequestID = erlang:system_time(nanosecond),
    gun:ws_send(
        ConnPid,
        StreamRef,
        {text, util:encode_json(maps:merge(#{request_id => RequestID}, Message))}
    ),
    RequestID.

% first parameter calculates the approx no. of users required for rank "ID" to get a zipf distribution with s = 1
generate_follow_users(0, _, List) ->
    List;
generate_follow_users(N, UsersList, _) when N =:= length(UsersList) ->
    UsersList;
generate_follow_users(N, UsersList, List) ->
    {Left, [Follower | Right]} = lists:split(rand:uniform(length(UsersList)) - 1, UsersList),
    generate_follow_users(N - 1, Left ++ Right, [integer_to_list(Follower) | List]).

schedule_tweet() ->
    erlang:send_after(?TWEET_INTERVAL, self(), {'$gen_cast', {schedule_tweet}}).

schedule_mention_query() ->
    erlang:send_after(?QUERY_INTERVAL, self(), {'$gen_cast', {schedule_mention_query}}).

update_request_times(Operation, RequestID, RequestTimes, PendingRequests) ->
    UpdatedPendingRequests = lists:delete(RequestID, PendingRequests),
    UpdatedRequestTimes = [{Operation, erlang:system_time(nanosecond) - RequestID} | RequestTimes],
    {UpdatedRequestTimes, UpdatedPendingRequests}.
