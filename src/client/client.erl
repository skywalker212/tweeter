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

-ifdef(PROD).
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
    request_times = []
}).

%% API
start_link(UserID, N) ->
    gen_server:start_link(?MODULE, [UserID, N], []).

%% GEN SERVER IMPLEMENTATION
init([UserID, N]) ->
    process_flag(trap_exit, true),
    IntID = list_to_integer(UserID),
    RegisterRequest = tweeter:register_user(UserID),
    State = #state{
        n = N,
        user_id = UserID,
        total_followers = trunc(math:floor((N * 2) / (IntID * 3))),
        pending_requests = [RegisterRequest]
    },
    {ok, State}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(
    {follow, FollowerID}, #state{user_id = UserID, pending_requests = PendingRequests} = State
) ->
    FollowRequest = tweeter:follow_user(UserID, FollowerID),
    {noreply, State#state{pending_requests = [FollowRequest | PendingRequests]}};
handle_cast(
    {schedule_tweet},
    #state{
        n = N,
        total_followers = TotalFollowers,
        user_id = UserID,
        pending_requests = PendingRequests
    } = State
) ->
    % code to make the users with more followers tweet more
    case rand:uniform(N) - 1 of
        X when X =< TotalFollowers ->
            % make the tweet a retweet with 25% chance
            case rand:uniform(4) of
                4 ->
                    % retweet the last tweet, may happen that we retweet someone's retweet but it's fine for our simulation purposes
                    case t_store:get_last_tweet_id() of
                        null -> Request = [];
                        ID -> Request = [tweeter:re_tweet(UserID, ID)]
                    end;
                _ ->
                    Request = [tweeter:tweet(UserID, generator:generate_tweet(N))]
            end;
        _ ->
            Request = []
    end,
    % schedule next tweet
    schedule_tweet(),
    case rand:uniform(1000) of
        1 ->
            {stop, normal, State#state{pending_requests = PendingRequests ++ Request}};
        _ ->
            {noreply, State#state{pending_requests = PendingRequests ++ Request}}
    end;
handle_cast(
    {tweet, TweetContent}, #state{user_id = UserID, pending_requests = PendingRequests} = State
) ->
    TweetRequest = tweeter:tweet(UserID, TweetContent),
    {noreply, State#state{pending_requests = [TweetRequest | PendingRequests]}};
handle_cast(
    {re_tweet, TweetID}, #state{user_id = UserID, pending_requests = PendingRequests} = State
) ->
    RetweetRequest = tweeter:re_tweet(UserID, TweetID),
    {noreply, State#state{pending_requests = [RetweetRequest | PendingRequests]}};
handle_cast(
    {schedule_mention_query}, #state{user_id = UserID, pending_requests = PendingRequests} = State
) ->
    % make a mention query with 10% chance
    case rand:uniform(10) of
        1 -> Request = [tweeter:user_mentions(UserID)];
        _ -> Request = []
    end,
    % schedule next query
    schedule_mention_query(),
    {noreply, State#state{pending_requests = PendingRequests ++ Request}};
handle_cast({mentions}, #state{user_id = UserID, pending_requests = PendingRequests} = State) ->
    MentionRequest = tweeter:user_mentions(UserID),
    {noreply, State#state{pending_requests = [MentionRequest | PendingRequests]}};
handle_cast({query, Query}, #state{user_id = UserID, pending_requests = PendingRequests} = State) ->
    QueryRequest = tweeter:query(UserID, Query),
    {noreply, State#state{pending_requests = [QueryRequest | PendingRequests]}}.

%% Incoming tweets
handle_info(
    {tweet, TweetType, {PosterID, TweetContent}},
    #state{user_id = UserID} = State
) ->
    ?PRINT("[~s] ~s received from ~s: ~s~n", [UserID, TweetType, PosterID, TweetContent]),
    {noreply, State};
%% Acknowledgement messages
handle_info(
    {ok, RequestID, register_account, _},
    #state{
        n = N,
        total_followers = TotalFollowers,
        user_id = UserID,
        request_times = RequestTimes,
        pending_requests = PendingRequests
    } = State
) ->
    {UpdatedRequestTimes, UpdatedPendingRequests} = update_request_times(
        register_account, RequestID, RequestTimes, PendingRequests
    ),
    ?PRINT("[~s, ~s] registered successfully~n", [UserID, RequestID]),
    % TODO: Don't do this for reconnecting client, do this only for newly registered clients
    IntID = list_to_integer(UserID),
    Followers = generate_follow_users(
        TotalFollowers, lists:delete(IntID, lists:seq(1, N)), []
    ),
    % wait for START_TIME miliseconds before making users follow current user so that other users have enough time to start up and register themselves
    {noreply,
        State#state{
            followers = Followers,
            request_times = UpdatedRequestTimes,
            pending_requests = UpdatedPendingRequests
        },
        ?START_TIME};
handle_info(
    {ok, RequestID, follow_user, FollowUserID},
    #state{user_id = UserID, request_times = RequestTimes, pending_requests = PendingRequests} =
        State
) ->
    {UpdatedRequestTimes, UpdatedPendingRequests} = update_request_times(
        follow_user, RequestID, RequestTimes, PendingRequests
    ),
    ?PRINT("[~s, ~s] Followed ~s~n", [UserID, RequestID, FollowUserID]),
    {noreply, State#state{
        request_times = UpdatedRequestTimes, pending_requests = UpdatedPendingRequests
    }};
handle_info(
    {ok, RequestID, publish_tweet, TweetID, TweetType},
    #state{user_id = UserID, request_times = RequestTimes, pending_requests = PendingRequests} =
        State
) ->
    {UpdatedRequestTimes, UpdatedPendingRequests} = update_request_times(
        publish_tweet, RequestID, RequestTimes, PendingRequests
    ),
    ?PRINT("[~s, ~s] ~p published with ID ~s~n", [UserID, RequestID, TweetType, TweetID]),
    {noreply, State#state{
        request_times = UpdatedRequestTimes, pending_requests = UpdatedPendingRequests
    }};
handle_info(
    {ok, RequestID, result, Query, QueryResult},
    #state{user_id = UserID, request_times = RequestTimes, pending_requests = PendingRequests} =
        State
) ->
    {UpdatedRequestTimes, UpdatedPendingRequests} = update_request_times(
        query, RequestID, RequestTimes, PendingRequests
    ),
    ?PRINT("[~s, ~s] Query ~s result ~s~n", [
        UserID,
        RequestID,
        Query,
        lists:map(
            fun({PosterID, Content}) -> " {[" ++ PosterID ++ "]: " ++ Content ++ "} " end,
            QueryResult
        )
    ]),
    {noreply, State#state{
        request_times = UpdatedRequestTimes, pending_requests = UpdatedPendingRequests
    }};
%% handle initial cooldown timeout before starting the simulation
handle_info(timeout, #state{n = N, user_id = UserID, followers = Followers} = State) ->
    case t_store:total_pids() of
        N ->
            lists:foreach(
                fun(Follower) ->
                    % Make the Follower follow the current user
                    gen_server:cast(
                        t_store:get_user_pid(integer_to_list(Follower)), {follow, UserID}
                    )
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
    Reason, #state{user_id = UserID, start_time = StartTime, request_times = RequestTimes} = State
) ->
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

% first parameter calculates the approx no. of users required for rank "ID" to get a zipf distribution with s = 1
generate_follow_users(0, _, List) ->
    List;
generate_follow_users(N, UsersList, _) when N =:= length(UsersList) ->
    UsersList;
generate_follow_users(N, UsersList, List) ->
    {Left, [Follower | Right]} = lists:split(rand:uniform(length(UsersList)) - 1, UsersList),
    generate_follow_users(N - 1, Left ++ Right, [Follower | List]).

schedule_tweet() ->
    erlang:send_after(?TWEET_INTERVAL, self(), {'$gen_cast', {schedule_tweet}}).

schedule_mention_query() ->
    erlang:send_after(?QUERY_INTERVAL, self(), {'$gen_cast', {schedule_mention_query}}).

update_request_times(Operation, RequestID, RequestTimes, PendingRequests) ->
    case lists:keytake(RequestID, 1, PendingRequests) of
        false ->
            {RequestTimes, PendingRequests};
        {value, {_, StartTime}, UpdatedPendingRequests} ->
            {
                [{Operation, erlang:system_time(nanosecond) - StartTime} | RequestTimes],
                UpdatedPendingRequests
            }
    end.
