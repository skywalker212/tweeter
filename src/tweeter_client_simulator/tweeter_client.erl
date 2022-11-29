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

%% the default number of seconds that a client would run before terminating itself
-define(DEFAULT_LEASE_TIME, infinity).

-record(state, {
    user_id, lease_time = ?DEFAULT_LEASE_TIME, start_time = util:get_utc_seconds()
}).

%% API
start_link(UserID) ->
    gen_server:start_link({local, list_to_atom(UserID)}, ?MODULE, [UserID], []).

%% GEN SERVER IMPLEMENTATION
init([UserID]) ->
    State = #state{user_id = UserID},
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
    {tweet, TweetType, Tweet},
    #state{user_id = UserID, start_time = StartTime, lease_time = LeaseTime} = State
) ->
    io:format("[~p] ~p received ~p~n", [UserID, TweetType, Tweet]),
    {noreply, State, time_left(StartTime, LeaseTime)};
handle_info(
    {ok, register_account, _},
    #state{user_id = UserID, start_time = StartTime, lease_time = LeaseTime} = State
) ->
    io:format("[~p] account registered~n", [UserID]),
    {noreply, State, time_left(StartTime, LeaseTime)};
handle_info(
    {ok, follow_user, FollowUserID},
    #state{user_id = UserID, start_time = StartTime, lease_time = LeaseTime} = State
) ->
    io:format("[~p] Followed ~p~n", [UserID, FollowUserID]),
    {noreply, State, time_left(StartTime, LeaseTime)};
handle_info(
    {ok, publish_tweet, TweetID, TweetType},
    #state{user_id = UserID, start_time = StartTime, lease_time = LeaseTime} = State
) ->
    io:format("[~p] ~p published with ID ~p~n", [UserID, TweetType, TweetID]),
    {noreply, State, time_left(StartTime, LeaseTime)};
handle_info(
    {ok, result, Query, QueryResult},
    #state{user_id = UserID, start_time = StartTime, lease_time = LeaseTime} = State
) ->
    io:format("[~p] Query ~p result ~p~n", [UserID, Query, QueryResult]),
    {noreply, State, time_left(StartTime, LeaseTime)};
handle_info(timeout, State) ->
    {stop, normal, State}.

terminate(_Reason, #state{user_id = UserID}) ->
    io:format("[~p] terminating~n", [UserID]),
    ok.
code_change(_OldVsn, _State, _Extra) ->
    ok.

%% Utility functions not to be exposed
time_left(_StartTime, infinity) ->
    infinity;
time_left(StartTime, LeaseTime) ->
    CurrentTime = util:get_utc_seconds(),
    TimeElapsed = CurrentTime - StartTime,
    case LeaseTime - TimeElapsed of
        Time when Time =< 0 -> 0;
        Time -> Time * 1000
    end.
