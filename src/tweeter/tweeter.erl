-module(tweeter).
-behaviour(gen_server).

-export([
    start_link/0,
    register_user/1,
    follow_user/2,
    tweet/2,
    re_tweet/2,
    user_mentions/1,
    query/2
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    terminate/2,
    code_change/3
]).
-record(state, {}).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
register_user(UserID) ->
    Now = util:get_timestamp(),
    RequestID = UserID ++ integer_to_list(Now),
    % assuming the client would be calling these functions so using self() pid
    gen_server:cast(?MODULE, {self(), RequestID, register, UserID}),
    {RequestID, Now}.
follow_user(UserID, FollowerID) ->
    Now = util:get_timestamp(),
    RequestID = UserID ++ integer_to_list(Now),
    gen_server:cast(?MODULE, {self(), RequestID, follow, UserID, FollowerID}),
    {RequestID, Now}.
tweet(UserID, Content) ->
    Now = util:get_timestamp(),
    RequestID = UserID ++ integer_to_list(Now),
    gen_server:cast(?MODULE, {self(), RequestID, tweet, UserID, Content}),
    {RequestID, Now}.
re_tweet(UserID, TweetID) ->
    Now = util:get_timestamp(),
    RequestID = UserID ++ integer_to_list(Now),
    gen_server:cast(?MODULE, {self(), RequestID, re_tweet, UserID, TweetID}),
    {RequestID, Now}.
user_mentions(UserID) ->
    Now = util:get_timestamp(),
    RequestID = UserID ++ integer_to_list(Now),
    gen_server:cast(?MODULE, {self(), RequestID, mentions, UserID}),
    {RequestID, Now}.
query(UserID, Query) ->
    Now = util:get_timestamp(),
    RequestID = UserID ++ integer_to_list(Now),
    gen_server:cast(?MODULE, {self(), RequestID, query, Query}),
    {RequestID, Now}.

%% gen server impelemtation
init([]) ->
    {ok, #state{}}.

%% not handling any synchronous request now
handle_call(_Message, _From, _State) ->
    ok.

%% asynchronously fork another process to reply to the request
%% RequestID is used on client side to measure the time taken to finish the request by the server
handle_cast({PID, RequestID, register, UserID}, State) ->
    spawn(t_worker, register_account, [PID, RequestID, UserID]),
    {noreply, State};
handle_cast({PID, RequestID, tweet, UserID, TweetContent}, State) ->
    spawn(t_worker, publish_tweet, [PID, RequestID, UserID, TweetContent]),
    {noreply, State};
handle_cast({PID, RequestID, follow, UserID, FollowerID}, State) ->
    spawn(t_worker, follow_user, [PID, RequestID, UserID, FollowerID]),
    {noreply, State};
handle_cast({PID, RequestID, re_tweet, UserID, TweetID}, State) ->
    spawn(t_worker, re_tweet, [PID, RequestID, UserID, TweetID]),
    {noreply, State};
handle_cast({PID, RequestID, mentions, UserID}, State) ->
    spawn(t_worker, mentions, [PID, RequestID, UserID]),
    {noreply, State};
handle_cast({PID, RequestID, query, Query}, State) ->
    spawn(t_worker, query, [PID, RequestID, Query]),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, _State, _Extra) ->
    ok.
