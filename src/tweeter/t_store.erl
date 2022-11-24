-module(t_store).
-export([
    init/0
]).

%% API
-export([
    save_user/1,
    get_user/1,
    save_tweet/3,
    save_tweet/5,
    get_tweet/1
]).
 
-define(USER_TABLE_NAME, user).
-define(TWEET_TABLE_NAME, tweet).

-record(user, {
    id = "",
    tweets = sets:new([{version, 2}]),
    following = sets:new([{version, 2}]),
    followers = sets:new([{version, 2}]),
    created_at = calendar:datetime_to_gregorian_seconds(calendar:universal_time())
}).
-record(tweet, {
    id = "",
    parent_id,
    content = "",
    poster_id = "",
    hashtags = sets:new([{version, 2}]),
    mentions = sets:new([{version, 2}]),
    type = tweet,
    replies = sets:new([{version, 2}]),
    likes = sets:new([{version, 2}]),
    retweets = sets:new([{version, 2}]),
    created_at = calendar:datetime_to_gregorian_seconds(calendar:universal_time())
}).

init() ->
    TableOptions = [
        public,
        named_table,
        {write_concurrency, auto},
        {read_concurrency, true}
    ],
    ets:new(?USER_TABLE_NAME, [ {keypos, #user.id} | TableOptions ]),
    ets:new(?TWEET_TABLE_NAME, [ {keypos, #tweet.id} | TableOptions ]),
    ok.

get_value(TableName, TableKey) ->
    case ets:lookup(TableName, TableKey) of
        [Match] -> Match;
        [] -> null
    end.

save_user(ID) ->
    ets:insert(?USER_TABLE_NAME, #user{ id = ID }).
get_user(ID) ->
    get_value(?USER_TABLE_NAME, ID).

save_tweet(ID, PosterID, Content) ->
    save_tweet(ID, undefined, tweet, PosterID, Content).
save_tweet(ID, ParentId, Type, PosterID, Content) ->
    RegexOptions = [global, multiline, {capture, all, list}],
    Mentions = case re:run(Content, "@(\\S+)", RegexOptions) of
        {match, MentionList} -> sets:from_list(lists:map(fun ([_, Match]) -> Match end, MentionList), [{version, 2}]);
        nomatch -> sets:new([{version, 2}])
    end,
    Hashtags = case re:run(Content, "#(\\S+)", RegexOptions) of
        {match, HashtagList} -> sets:from_list(lists:map(fun ([_, Match]) -> Match end, HashtagList), [{version, 2}]);
        nomatch -> sets:new([{version, 2}])
    end,
    ets:insert(?TWEET_TABLE_NAME, #tweet{
        id = ID,
        parent_id = ParentId,
        type = Type,
        poster_id = PosterID,
        content = Content,
        mentions = Mentions,
        hashtags = Hashtags
    }). 

get_tweet(ID) ->
    get_value(?TWEET_TABLE_NAME, ID).