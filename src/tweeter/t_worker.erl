-module(t_worker).
-export([
    register_account/2,
    publish_tweet/3,
    follow_user/3,
    re_tweet/3,
    mentions/2,
    query/2
]).

register_account({PID, _}, UserID) ->
    t_store:save_user(UserID),
    PID ! {ok, register_account, UserID}.

publish_tweet({PID, _}, UserID, Content) ->
    TweetID = util:generate_string(),
    t_store:save_tweet(TweetID, UserID, Content),
    PID ! {ok, publish_tweet, TweetID}.

follow_user({PID, _}, UserID, FollowUserID) ->
    t_store:follow_user(UserID, FollowUserID),
    PID ! {ok, follow_user}.

re_tweet({PID, _}, UserID, TweetID) ->
    RetweetID = util:generate_string(),
    t_store:re_tweet(RetweetID, TweetID, UserID),
    PID ! {ok, re_tweet, RetweetID}.

mentions(Client, UserID) ->
    query(Client, "@" ++ UserID).

query({PID, _}, Query) ->
    QueryResult = t_store:query_tweets(Query),
    PID ! {ok, result, QueryResult}.
