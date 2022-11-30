-module(tweeter_client).
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

-record(state, {
    n = 1,
    total_followers = 0,
    followers = [],
    user_id,
    start_time = util:get_utc_seconds()
}).

%% API
start_link(UserID, N) ->
    gen_server:start_link(?MODULE, [UserID, N], []).

%% GEN SERVER IMPLEMENTATION
init([UserID, N]) ->
    IntID = list_to_integer(UserID),
    State = #state{n = N, user_id = UserID, total_followers = trunc(math:floor((N - 1) / IntID))},
    tweeter:register_user(State#state.user_id),
    {ok, State}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({follow, FollowerID}, #state{user_id = UserID} = State) ->
    tweeter:follow_user(UserID, FollowerID),
    {noreply, State};
handle_cast({tweet, TweetContent}, #state{user_id = UserID} = State) ->
    tweeter:tweet(UserID, TweetContent),
    {noreply, State};
handle_cast({re_tweet, TweetID}, #state{user_id = UserID} = State) ->
    tweeter:re_tweet(UserID, TweetID),
    {noreply, State};
handle_cast({mentions}, #state{user_id = UserID} = State) ->
    tweeter:user_mentions(UserID),
    {noreply, State};
handle_cast({query, Query}, State) ->
    tweeter:query(Query),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(
    {ok, register_account, _},
    #state{n = N, total_followers = TotalFollowers, user_id = UserID} = State
) ->
    IntID = list_to_integer(UserID),
    Followers = generate_follow_users(
        TotalFollowers, lists:delete(IntID, lists:seq(1, N)), []
    ),
    {noreply, State#state{followers = Followers}, ?START_TIME};
handle_info(
    {ok, follow_user, FollowUserID},
    #state{user_id = UserID} = State
) ->
    io:format("[~p] Followed ~p~n", [UserID, FollowUserID]),
    {noreply, State};
handle_info(
    {tweet, TweetType, Tweet},
    #state{user_id = UserID} = State
) ->
    io:format("[~p] ~p received ~p~n", [UserID, TweetType, Tweet]),
    {noreply, State};
handle_info(
    {ok, publish_tweet, TweetID, TweetType},
    #state{user_id = UserID} = State
) ->
    io:format("[~p] ~p published with ID ~p~n", [UserID, TweetType, TweetID]),
    {noreply, State};
handle_info(
    {ok, result, Query, QueryResult},
    #state{user_id = UserID} = State
) ->
    io:format("[~p] Query ~p result ~p~n", [UserID, Query, QueryResult]),
    {noreply, State};
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
            {noreply, State};
        _ ->
            {noreply, State, ?START_TIME}
    end.

terminate(_Reason, #state{user_id = UserID}) ->
    io:format("[~p] terminating~n", [UserID]),
    ok.
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
