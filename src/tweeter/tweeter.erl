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

handle_call({register, UserID}, From, State) ->
    spawn(t_worker, register_account, [From, UserID]),
    {reply, ok, State};
handle_call({tweet, UserID, TweetContent}, From, State) ->
    spawn(t_worker, publish_tweet, [From, UserID, TweetContent]),
    {reply, ok, State};
handle_call({follow, UserID, FollowerID}, From, State) ->
    spawn(t_worker, follow_user, [From, UserID, FollowerID]),
    {reply, ok, State};
handle_call({re_tweet, UserID, TweetID}, From, State) ->
    spawn(t_worker, re_tweet, [From, UserID, TweetID]),
    {reply, ok, State};
handle_call({mentions, UserID}, From, State) ->
    spawn(t_worker, mentions, [From, UserID]),
    {reply, ok, State};
handle_call({query, Query}, From, State) ->
    spawn(t_worker, query, [From, Query]),
    {reply, ok, State}.

%% would be useful when implementing websockets
% handle_cast(Msg, State) ->
%     {ok, Handler} = t_worker:start_link(),
%     gen_server:cast(Handler, Msg),
%     {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, _State, _Extra) ->
    ok.
