-module(t_worker).
-export([
    register_account/3,
    publish_tweet/4,
    follow_user/4,
    re_tweet/4,
    mentions/3,
    query/3
]).

register_account(PID, RequestID, UserID) ->
    store:save_user(UserID, PID),
    PID ! {ok, RequestID, register_account, UserID}.

publish_tweet(PID, RequestID, UserID, Content) ->
    TweetID = UserID ++ integer_to_list(util:get_utc_seconds()),
    store:save_tweet(TweetID, UserID, Content),
    PID ! {ok, RequestID, publish_tweet, TweetID, tweet},
    notify_followers(UserID, {tweet, tweet, {UserID, Content}}).

follow_user(PID, RequestID, UserID, FollowUserID) ->
    store:follow_user(UserID, FollowUserID),
    PID ! {ok, RequestID, follow_user, FollowUserID}.

re_tweet(PID, RequestID, UserID, TweetID) ->
    RetweetID = UserID ++ integer_to_list(util:get_utc_seconds()),
    store:re_tweet(RetweetID, TweetID, UserID),
    Content = store:get_retweet_content(TweetID),
    PID ! {ok, RequestID, publish_tweet, RetweetID, re_tweet},
    notify_followers(UserID, {tweet, re_tweet, {UserID, Content}}).

mentions(PID, RequestID, UserID) ->
    query(PID, RequestID, "@" ++ UserID).

query(PID, RequestID, Query) ->
    QueryResult = store:query_tweets(Query),
    PID ! {ok, RequestID, result, Query, QueryResult}.

notify_followers(UserID, Message) ->
    lists:foreach(
        fun(FollowerID) ->
            store:get_user_pid(FollowerID) ! Message
        end,
        store:get_followers(UserID)
    ).
