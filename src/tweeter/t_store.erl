-module(t_store).
-export([
    init/0
]).

%% API
-export([
    save_user/2,
    get_user/1,
    follow_user/2,
    get_followers/1,
    save_tweet/3,
    save_tweet/5,
    get_tweet/1,
    get_retweet_content/1,
    re_tweet/3,
    quote_tweet/4,
    reply_tweet/4,
    like_tweet/2,
    get_user_tweets/1,
    get_user_tweets/2,
    get_last_tweet_id/0,
    query_tweets/1,
    get_user_pid/1,
    total_pids/0
]).

-define(USER_TABLE_NAME, user).
-define(TWEET_TABLE_NAME, tweet).
-define(MENTION_TABLE_NAME, mention).
-define(HASHTAG_TABLE_NAME, hashtag).
-define(PID_TABLE_NAME, pid).

-record(user, {
    id,
    following = sets:new([{version, 2}]),
    followers = sets:new([{version, 2}]),
    created_at = util:get_utc_seconds()
}).
-record(tweet, {
    id,
    parent_id,
    content,
    poster_id,
    type = tweet,
    likes = sets:new([{version, 2}]),
    created_at = util:get_utc_seconds()
}).

init() ->
    TableOptions = [
        public,
        named_table,
        {write_concurrency, auto},
        {read_concurrency, true}
    ],
    ets:new(?USER_TABLE_NAME, [{keypos, #user.id} | TableOptions]),
    ets:new(?TWEET_TABLE_NAME, [{keypos, #tweet.id} | TableOptions]),
    ets:new(?MENTION_TABLE_NAME, TableOptions),
    ets:new(?HASHTAG_TABLE_NAME, TableOptions),
    ets:new(?PID_TABLE_NAME, TableOptions),
    ok.

get_value(TableName, TableKey) ->
    case ets:lookup(TableName, TableKey) of
        [Match] -> Match;
        [] -> null
    end.

save_user(ID, PID) ->
    %% dont overwrite existing user
    case get_user(ID) of
        null -> ets:insert(?USER_TABLE_NAME, #user{id = ID});
        _ -> ok
    end,
    ets:insert(?PID_TABLE_NAME, {ID, PID}),
    ok.

get_user(ID) ->
    get_value(?USER_TABLE_NAME, ID).

follow_user(FollowerID, FollowingID) ->
    % assuming current and following users exist
    [FollowerUser] = ets:lookup(?USER_TABLE_NAME, FollowerID),
    ets:insert(?USER_TABLE_NAME, FollowerUser#user{
        following = sets:add_element(FollowingID, FollowerUser#user.following)
    }),
    [FollowedUser] = ets:lookup(?USER_TABLE_NAME, FollowingID),
    ets:insert(?USER_TABLE_NAME, FollowedUser#user{
        followers = sets:add_element(FollowerID, FollowedUser#user.followers)
    }),
    ok.

get_followers(UserID) ->
    #user{
        followers = Followers
    } = get_user(UserID),
    sets:to_list(Followers).

save_tweet(ID, PosterID, Content) ->
    save_tweet(ID, undefined, tweet, PosterID, Content).
save_tweet(ID, ParentId, Type, PosterID, Content) ->
    RegexOptions = [global, multiline, {capture, all, list}],
    case re:run(Content, "@(\\S+)", RegexOptions) of
        {match, MentionList} ->
            lists:foreach(
                fun([_, Match]) ->
                    case ets:lookup(?MENTION_TABLE_NAME, Match) of
                        [{_, MentionSet}] ->
                            ets:insert(
                                ?MENTION_TABLE_NAME, {Match, sets:add_element(ID, MentionSet)}
                            );
                        [] ->
                            ets:insert(
                                ?MENTION_TABLE_NAME, {Match, sets:from_list([ID], [{version, 2}])}
                            )
                    end
                end,
                MentionList
            );
        nomatch ->
            ok
    end,
    case re:run(Content, "#(\\S+)", RegexOptions) of
        {match, HashtagList} ->
            lists:foreach(
                fun([_, Match]) ->
                    case ets:lookup(?HASHTAG_TABLE_NAME, Match) of
                        [{_, HashtagSet}] ->
                            ets:insert(
                                ?HASHTAG_TABLE_NAME, {Match, sets:add_element(ID, HashtagSet)}
                            );
                        [] ->
                            ets:insert(
                                ?HASHTAG_TABLE_NAME, {Match, sets:from_list([ID], [{version, 2}])}
                            )
                    end
                end,
                HashtagList
            );
        nomatch ->
            ok
    end,
    Tweet = #tweet{
        id = ID,
        parent_id = ParentId,
        type = Type,
        poster_id = PosterID,
        content = Content
    },
    ets:insert(?TWEET_TABLE_NAME, Tweet),
    Tweet.

re_tweet(ID, RetweetID, PosterID) ->
    save_tweet(ID, RetweetID, re_tweet, PosterID, "").

quote_tweet(ID, QuotedTweetID, PosterID, Quote) ->
    save_tweet(ID, QuotedTweetID, quote, PosterID, Quote).

reply_tweet(ID, ReplyTweetID, PosterID, ReplyContent) ->
    save_tweet(ID, ReplyTweetID, reply, PosterID, ReplyContent).

like_tweet(TweetID, UserID) ->
    [Tweet] = ets:lookup(?TWEET_TABLE_NAME, TweetID),
    #tweet{
        likes = Likes
    } = Tweet,
    ets:insert(?TWEET_TABLE_NAME, Tweet#tweet{likes = sets:add_element(UserID, Likes)}),
    ok.

get_user_tweets(UserID) ->
    ets:match_object(?TWEET_TABLE_NAME, #tweet{poster_id = UserID, _ = '_'}).
get_user_tweets(UserID, Seconds) ->
    UTCBeforeSeconds = util:get_utc_seconds(Seconds),
    ets:select(?TWEET_TABLE_NAME, [
        {#tweet{poster_id = UserID, created_at = '$1', _ = '_'}, [{'>', '$1', UTCBeforeSeconds}], [
            '$_'
        ]}
    ]).

%% naming of the function couldve been better
get_tweets(TableName, Key) ->
    case ets:lookup(TableName, Key) of
        [{_, Set}] ->
            lists:map(
                fun(ID) ->
                    [Tweet] = ets:lookup(?TWEET_TABLE_NAME, ID),
                    Tweet
                end,
                sets:to_list(Set)
            );
        [] ->
            []
    end.
query_tweets(Query) ->
    case
        re:run(util:strip_whitespace(Query), "^(#(\\S+))$|^(@(\\S+))$|^(.+)$", [
            {capture, all, list}
        ])
    of
        {match, [_, _, Hashtag]} ->
            Tweets = get_tweets(?HASHTAG_TABLE_NAME, Hashtag);
        {match, [_, _, _, _, MentionID]} ->
            Tweets = get_tweets(?MENTION_TABLE_NAME, MentionID);
        {match, [_, _, _, _, _, QueryString]} ->
            %% no choice but to iterate through all the tweets and get the tweets with the given string since we dont have an index on tweet content
            Tweets = ets:foldl(
                fun(Tweet, List) ->
                    case string:str(Tweet#tweet.content, QueryString) of
                        0 -> List;
                        _ -> [Tweet | List]
                    end
                end,
                [],
                ?TWEET_TABLE_NAME
            );
        nomatch ->
            Tweets = []
    end,
    lists:map(fun(Tweet) -> {Tweet#tweet.poster_id, Tweet#tweet.content} end, Tweets).

get_tweet(ID) ->
    get_value(?TWEET_TABLE_NAME, ID).
get_retweet_content(TweetID) ->
    Tweet = get_tweet(TweetID),
    (Tweet#tweet.content).
get_last_tweet_id() ->
    case ets:last(?TWEET_TABLE_NAME) of
        '$end_of_table' -> null;
        Key -> Key
    end.

get_user_pid(ID) ->
    {_, PID} = get_value(?PID_TABLE_NAME, ID),
    PID.

total_pids() ->
    ets:info(?PID_TABLE_NAME, size).
