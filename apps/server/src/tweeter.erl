-module(tweeter).

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

-record(state, {
    user_id,
    challenge,
    authenticated = false,
    public_key
}).

%% cowboy websocket handler impelemtation
init(Req, _State) ->
    {cowboy_websocket, Req, #state{}}.

websocket_init(State) ->
    {[{text, "ack"}], State}.

websocket_handle({text, SerializedJSON}, #state{authenticated = false, challenge = undefined} = State) ->
    RequestMap = util:decode_json(SerializedJSON),
    case RequestMap of
        #{<<"request_id">> := RequestID, <<"register">> := Data} ->
            #{<<"user_id">> := ID, <<"public_key">> := EncodedPublicKey} = Data,
            store:save_user(ID, util:decode_public_key(list_to_binary(EncodedPublicKey))),
            store:set_handler_pid_for_user_id(ID, self()),
            {[{text, util:encode_json(#{request_id => RequestID, success => #{register => true}})}], State#state{user_id = ID, authenticated = true}};
        #{<<"request_id">> := RequestID, <<"login">> := Data} ->
            #{<<"user_id">> := ID, <<"public_key">> := EncodedPublicKey} = Data,
            PublicKey = store:get_public_key(ID),
            case util:decode_public_key(list_to_binary(EncodedPublicKey)) =:= PublicKey of
                true ->
                    Challenge = util:generate_challenge(),
                    {[{text, util:encode_json(#{request_id => RequestID, success => #{login => true}, challenge => Challenge})}], State#state{user_id = ID, challenge = {erlang:system_time(millisecond), Challenge}, public_key = PublicKey}};
                false ->
                    {[{text, util:encode_json(#{request_id => RequestID, error => <<"Invalid Public Key">>})}], State}
            end;
        Request ->
            {[{text, util:encode_json(#{error => <<"403: User Not Logged in">>, request => Request})}], State}
    end;
websocket_handle({binary, SignedChallenge}, #state{user_id = UserID, authenticated = false, challenge = {ChallengeTimestamp, Challenge}, public_key = PublicKey} = State) ->
    case util:verify_challenge(SignedChallenge, Challenge, ChallengeTimestamp, PublicKey) of
        true ->
            io:format("Verification successful!"),
            store:set_handler_pid_for_user_id(UserID, self()),
            {[{text, util:encode_json(#{success => #{challenge => true}})}], State#state{authenticated = true, challenge = undefined}};
        false ->
            io:format("Verification failed!"),
            {[{text, util:encode_json(#{error => #{challenge => false}})}], State}
    end;
websocket_handle({text, SerializedJSON}, #state{user_id = UserID, authenticated = true, challenge = undefined} = State) ->
    RequestMap = util:decode_json(SerializedJSON),
    case RequestMap of
        #{<<"request_id">> := RequestID, <<"tweet">> := Data} ->
            #{<<"content">> := TweetContent} = Data,
            TweetID = UserID ++ integer_to_list(util:get_utc_seconds()),
            store:save_tweet(TweetID, UserID, TweetContent),
            notify_followers(UserID, {tweet, tweet, {UserID, TweetContent}}),
            {[{text, util:encode_json(#{request_id => RequestID, success => #{tweet => #{type => tweet, id => TweetID}}})}], State};
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
            {[{text, util:encode_json(#{request_id => RequestID, success => #{tweet => #{type => re_tweet, id => RetweetID}}})}], State};
        #{<<"request_id">> := RequestID, <<"mentions">> := _Data} ->
            query(RequestID, "@" ++ UserID, State);
        #{<<"request_id">> := RequestID, <<"query">> := Query} ->
            query(RequestID, Query, State);
        Request ->
            {[{text, util:encode_json(#{error => <<"Unknown Request">>, request => Request})}], State}
    end;
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({tweet, TweetType, {PosterID, TweetContent}}, State) ->
    {[{text, util:encode_json(#{tweet => #{poster_id => PosterID, tweet_type => TweetType, content => TweetContent}})}], State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _PartialReq, _State) ->
    ok.

%% Utility Functions
query(RequestID, Query, State) ->
    QueryResult = lists:map(fun ({PosterID, Content}) -> #{poster_id => PosterID, content => Content} end, store:query_tweets(Query)),
    {[{text, util:encode_json(#{request_id => RequestID, success => #{query => Query, result => QueryResult}})}], State}.

notify_followers(UserID, Message) ->
    lists:foreach(
        fun(FollowerID) ->
            store:get_handler_pid_from_user_id(FollowerID) ! Message
        end,
        store:get_followers(UserID)
    ).