-module(t_store).
-export([
    init/0
]).

%% API
-export([
    save_user/1,
    get_user/1,
    follow_user/2,
    save_tweet/3,
    save_tweet/5,
    get_tweet/1,
    re_tweet/3,
    quote_tweet/4,
    reply_tweet/4,
    like_tweet/2,
    get_user_tweets/1,
    get_user_tweets/2,
    query_tweets/1
]).

-define(USER_TABLE_NAME, user).
-define(TWEET_TABLE_NAME, tweet).
-define(MENTION_TABLE_NAME, mention).
-define(HASHTAG_TABLE_NAME, hashtag).

-record(user, {
    id,
    following = sets:new([{version, 2}]),
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
    ok.

get_value(TableName, TableKey) ->
    case ets:lookup(TableName, TableKey) of
        [Match] -> Match;
        [] -> null
    end.

save_user(ID) ->
    ets:insert(?USER_TABLE_NAME, #user{id = ID}),
    ok.
get_user(ID) ->
    get_value(?USER_TABLE_NAME, ID).

follow_user(FollowerID, FollowingID) ->
    [FollowerUser] = ets:lookup(?USER_TABLE_NAME, FollowerID),
    #user{
        following = Following
    } = FollowerUser,
    ets:insert(?USER_TABLE_NAME, FollowerUser#user{
        following = sets:add_element(FollowingID, Following)
    }),
    ok.

save_tweet(ID, PosterID, Content) ->
    save_tweet(ID, undefined, tweet, PosterID, Content),
    ok.
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
    ets:insert(?TWEET_TABLE_NAME, #tweet{
        id = ID,
        parent_id = ParentId,
        type = Type,
        poster_id = PosterID,
        content = Content
    }),
    ok.

re_tweet(ID, RetweetID, PosterID) ->
    save_tweet(ID, RetweetID, retweet, PosterID, undefined),
    ok.

quote_tweet(ID, QuotedTweetID, PosterID, Quote) ->
    save_tweet(ID, QuotedTweetID, quote, PosterID, Quote),
    ok.

reply_tweet(ID, ReplyTweetID, PosterID, ReplyContent) ->
    save_tweet(ID, ReplyTweetID, reply, PosterID, ReplyContent),
    ok.

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
            get_tweets(?HASHTAG_TABLE_NAME, Hashtag);
        {match, [_, _, _, _, MentionID]} ->
            get_tweets(?MENTION_TABLE_NAME, MentionID);
        {match, [_, _, _, _, _, QueryString]} ->
            %% no choice but to iterate through all the tweets and get the tweets with the given string since we dont have an index on tweet content
            ets:foldl(
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
            []
    end.

get_tweet(ID) ->
    get_value(?TWEET_TABLE_NAME, ID).
