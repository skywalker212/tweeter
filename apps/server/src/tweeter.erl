-module(tweeter).

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

-record(state, {
    user_id
}).

% %% API
% register_user(UserID) ->
%     Now = erlang:system_time(nanosecond),
%     RequestID = UserID ++ integer_to_list(Now),
%     % assuming the client would be calling these functions so using self() pid
%     gen_server:cast(?MODULE, {self(), RequestID, register, UserID}),
%     {RequestID, Now}.
% follow_user(UserID, FollowerID) ->
%     Now = erlang:system_time(nanosecond),
%     RequestID = UserID ++ integer_to_list(Now),
%     gen_server:cast(?MODULE, {self(), RequestID, follow, UserID, FollowerID}),
%     {RequestID, Now}.
% tweet(UserID, Content) ->
%     Now = erlang:system_time(nanosecond),
%     RequestID = UserID ++ integer_to_list(Now),
%     gen_server:cast(?MODULE, {self(), RequestID, tweet, UserID, Content}),
%     {RequestID, Now}.
% re_tweet(UserID, TweetID) ->
%     Now = erlang:system_time(nanosecond),
%     RequestID = UserID ++ integer_to_list(Now),
%     gen_server:cast(?MODULE, {self(), RequestID, re_tweet, UserID, TweetID}),
%     {RequestID, Now}.
% user_mentions(UserID) ->
%     Now = erlang:system_time(nanosecond),
%     RequestID = UserID ++ integer_to_list(Now),
%     gen_server:cast(?MODULE, {self(), RequestID, mentions, UserID}),
%     {RequestID, Now}.
% query(UserID, Query) ->
%     Now = erlang:system_time(nanosecond),
%     RequestID = UserID ++ integer_to_list(Now),
%     gen_server:cast(?MODULE, {self(), RequestID, query, Query}),
%     {RequestID, Now}.

%% cowboy websocket handler impelemtation
init(Req, _State) ->
    {cowboy_websocket, Req, #state{}}.

websocket_init(State) ->
    {[{text, <<"Web socket connection acknowledged!">>}], State}.

websocket_handle({text, SerializedJSON}, #state{user_id = UserID} = State) ->
    RequestMap = util:decode_json(SerializedJSON),
    case RequestMap of
        #{<<"request_id">> := RequestID, <<"register">> := Data} ->
            #{<<"user_id">> := ID} = Data,
            store:save_user(ID, self()),
            {[{text, util:encode_json(#{request_id => RequestID, success => #{register => ID}})}], State#state{user_id = ID}};
        #{<<"request_id">> := RequestID, <<"tweet">> := Data} ->
            #{<<"tweet">> := TweetContent} = Data,
            TweetID = UserID ++ integer_to_list(util:get_utc_seconds()),
            store:save_tweet(TweetID, UserID, TweetContent),
            notify_followers(UserID, {tweet, tweet, {UserID, TweetContent}}),
            {[{text, util:encode_json(#{request_id => RequestID, success => #{tweet => TweetID}})}], State};
        #{<<"request_id">> := RequestID, <<"follow">> := Data} ->
            #{<<"follow_id">> := FollowID} = Data,
            store:follow_user(UserID, FollowID),
            {[{text, util:encode_json(#{request_id => RequestID, success => #{follow => FollowID}})}], State};
        #{<<"request_id">> := RequestID, <<"re_tweet">> := Data} ->
            #{<<"tweet_id">> := TweetID} = Data,
            RetweetID = UserID ++ integer_to_list(util:get_utc_seconds()),
            store:re_tweet(RetweetID, TweetID, UserID),
            Content = store:get_retweet_content(TweetID),
            notify_followers(UserID, {tweet, re_tweet, {UserID, Content}}),
            {[{text, util:encode_json(#{request_id => RequestID, success => #{re_tweet => RetweetID}})}], State};
        #{<<"request_id">> := RequestID, <<"mentions">> := _Data} ->
            query(RequestID, "@" ++ UserID, State);
        #{<<"request_id">> := RequestID, <<"query">> := Data} ->
            #{<<"query">> := Query} = Data,
            query(RequestID, Query, State);
        _ ->
            {[{text, util:encode_json(#{error => <<"Unknown Request">>})}], State}
    end;
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({tweet, TweetType, {UserID, TweetContent}}, State) ->
    {[{text, util:encode_json(#{tweet => #{user_id => UserID, tweet_type => TweetType, tweet => list_to_binary(TweetContent)}})}], State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _PartialReq, _State) ->
    ok.

%% Utility Functions
query(RequestID, Query, State) ->
    QueryResult = lists:map(fun ({PosterID, Content}) -> #{poster_id => PosterID, tweet => list_to_binary(Content)} end, store:query_tweets(Query)),
    {[{text, util:encode_json(#{request_id => RequestID, success => #{query => QueryResult}})}], State}.

notify_followers(UserID, Message) ->
    lists:foreach(
        fun(FollowerID) ->
            store:get_handler_pid_from_user_id(FollowerID) ! Message
        end,
        store:get_followers(UserID)
    ).