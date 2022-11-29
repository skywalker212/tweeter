-module(tweeter).
-behaviour(gen_server).

-export([
    start_link/0,
    register_user/1,
    follow_user/2,
    tweet/2,
    re_tweet/2,
    user_mentions/1,
    query/1
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
    % assuming the client would be calling these functions so using self() pid
    gen_server:cast(?MODULE, {self(), register, UserID}).
follow_user(UserID, FollowerID) ->
    gen_server:cast(?MODULE, {self(), follow, UserID, FollowerID}).
tweet(UserID, Content) ->
    gen_server:cast(?MODULE, {self(), tweet, UserID, Content}).
re_tweet(UserID, TweetID) ->
    gen_server:cast(?MODULE, {self(), re_tweet, UserID, TweetID}).
user_mentions(UserID) ->
    gen_server:cast(?MODULE, {self(), mentions, UserID}).
query(Query) ->
    gen_server:cast(?MODULE, {self(), query, Query}).

%% gen server impelemtation
init([]) ->
    {ok, #state{}}.

%% not handling any synchronous request now
handle_call(_Message, _From, _State) ->
    ok.

%% asynchronously fork another process to reply to the request
handle_cast({PID, register, UserID}, State) ->
    spawn(t_worker, register_account, [PID, UserID]),
    {noreply, State};
handle_cast({PID, tweet, UserID, TweetContent}, State) ->
    spawn(t_worker, publish_tweet, [PID, UserID, TweetContent]),
    {noreply, State};
handle_cast({PID, follow, UserID, FollowerID}, State) ->
    spawn(t_worker, follow_user, [PID, UserID, FollowerID]),
    {noreply, State};
handle_cast({PID, re_tweet, UserID, TweetID}, State) ->
    spawn(t_worker, re_tweet, [PID, UserID, TweetID]),
    {noreply, State};
handle_cast({PID, mentions, UserID}, State) ->
    spawn(t_worker, mentions, [PID, UserID]),
    {noreply, State};
handle_cast({PID, query, Query}, State) ->
    spawn(t_worker, query, [PID, Query]),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, _State, _Extra) ->
    ok.
