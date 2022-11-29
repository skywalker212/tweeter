-module(t_worker).
-export([
    register_account/2,
    publish_tweet/3,
    follow_user/3,
    re_tweet/3,
    mentions/2,
    query/2
]).

register_account(PID, UserID) ->
    t_store:save_user(UserID, PID),
    PID ! {ok, register_account, UserID}.

publish_tweet(PID, UserID, Content) ->
    TweetID = UserID ++ integer_to_list(util:get_utc_seconds()),
    t_store:save_tweet(TweetID, UserID, Content),
    PID ! {ok, publish_tweet, TweetID, tweet},
    notify_followers(UserID, {tweet, tweet, {UserID, Content}}).

follow_user(PID, UserID, FollowUserID) ->
    t_store:follow_user(UserID, FollowUserID),
    PID ! {ok, follow_user, FollowUserID}.

re_tweet(PID, UserID, TweetID) ->
    RetweetID = UserID ++ integer_to_list(util:get_utc_seconds()),
    t_store:re_tweet(RetweetID, TweetID, UserID),
    Content = t_store:get_retweet_content(TweetID),
    PID ! {ok, publish_tweet, RetweetID, re_tweet},
    notify_followers(UserID, {tweet, re_tweet, {UserID, Content}}).

mentions(PID, UserID) ->
    query(PID, "@" ++ UserID).

query(PID, Query) ->
    QueryResult = t_store:query_tweets(Query),
    PID ! {ok, result, Query, QueryResult}.

notify_followers(UserID, Message) ->
    lists:foreach(
        fun(FollowerID) ->
            {_, FollowerPID} = t_store:get_user_pid(FollowerID),
            FollowerPID ! Message
        end,
        t_store:get_followers(UserID)
    ).
