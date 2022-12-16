-module(client).
-behaviour(gen_server).

% api to be used for using the client
-export([
    start_link/2,
    start/1,
    follow/2,
    tweet/2,
    re_tweet/2,
    mentions/1,
    query/2,
    disconnect/1
]).

% functions for gen_server behavior
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% startup time in milliseconds to wait other clients to register themselves before performing actions
-define(START_TIME, 500).
%% Interval at which to publish tweet/retweet
-define(TWEET_INTERVAL, 100).
%% Interval at which to query
-define(QUERY_INTERVAL, 500).

-define(PRINT(S, A), io:format(S, A)).

-record(state, {
    n = 1,
    total_followers = 0,
    followers = [],
    user_id,
    start_time = erlang:system_time(),
    pending_requests = [],
    request_times = [],
    conn_pid,
    stream_ref,
    rsa_key_pair,
    ecdh_key_pair = util:generate_key_pair(ecdh),
    new_user,
    authenticated = false,
    hmac_key,
    standalone = false
}).

%% API
start_link(UserID, N) ->
    gen_server:start_link(?MODULE, [UserID, N], []).
start(UserID) ->
    case c_store:get_client_pid(UserID) of
        null ->
            gen_server:start_link(?MODULE, [UserID], []);
        _ ->
            ?PRINT("User ~p is already connected!~n", [UserID])
    end.
follow(UserID, FollowID) ->
    send_cast(UserID, {follow, FollowID}).
tweet(UserID, Content) ->
    send_cast(UserID, {tweet, Content}).
re_tweet(UserID, TweetID) ->
    send_cast(UserID, {re_tweet, TweetID}).
mentions(UserID) ->
    send_cast(UserID, {mentions}).
query(UserID, Query) ->
    send_cast(UserID, {query, Query}).
disconnect(UserID) ->
    case c_store:get_client_pid(UserID) of
        null ->
            ?PRINT("User ~p is not logged in!~n", [UserID]);
        PID ->
            gen_server:stop(PID)
    end.

%% GEN SERVER IMPLEMENTATION
init([UserID]) ->
    init([UserID, 0, true]);
init([UserID, N]) ->
    init([UserID, N, false]);
init([UserID, N, Standalone]) ->
    {NewUser, RSAKeyPair} =
        case c_store:get_key_pair(UserID) of
            null ->
                GeneratedKeyPair = util:generate_key_pair(rsa),
                c_store:store_key_pair(UserID, GeneratedKeyPair),
                {true, GeneratedKeyPair};
            KP ->
                {false, KP}
        end,
    process_flag(trap_exit, true),
    {ok, ConnPid} = gun:open("localhost", 5000),
    State = case Standalone of
        true ->
            #state{
                user_id = UserID,
                conn_pid = ConnPid,
                rsa_key_pair = RSAKeyPair,
                new_user = NewUser,
                standalone = true
            };
        false ->
            IntID = list_to_integer(UserID),
            #state{
                n = N,
                user_id = UserID,
                total_followers = trunc(math:floor((N * 2) / (IntID * 3))),
                conn_pid = ConnPid,
                rsa_key_pair = RSAKeyPair,
                new_user = NewUser,
                standalone = false
            }
    end,
    {ok, State}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(
    {follow, FollowerID},
    #state{
        conn_pid = ConnPid,
        stream_ref = StreamRef,
        pending_requests = PendingRequests,
        hmac_key = HMACKey
    } = State
) ->
    RequestID = send_signed_ws_message(
        ConnPid, StreamRef, #{follow => #{follow_id => FollowerID}}, HMACKey
    ),
    {noreply, State#state{pending_requests = [RequestID | PendingRequests]}};
handle_cast(
    {schedule_tweet},
    #state{
        user_id = UserID,
        n = N,
        total_followers = TotalFollowers,
        pending_requests = PendingRequests,
        conn_pid = ConnPid,
        stream_ref = StreamRef,
        hmac_key = HMACKey
    } = State
) ->
    % code to make the users with more followers tweet more
    case rand:uniform(N) - 1 of
        X when X =< TotalFollowers ->
            % make the tweet a retweet with 25% chance
            case rand:uniform(4) of
                4 ->
                    % retweet the last tweet, may happen that we retweet someone's retweet but it's fine for our simulation purposes
                    case store:get_last_tweet_id() of
                        null ->
                            Request = [];
                        ID ->
                            Request = [
                                send_signed_ws_message(
                                    ConnPid, StreamRef, #{re_tweet => #{tweet_id => ID}}, HMACKey
                                )
                            ]
                    end;
                _ ->
                    Request = [
                        send_signed_ws_message(
                            ConnPid,
                            StreamRef,
                            #{tweet => #{content => generator:generate_tweet(N)}},
                            HMACKey
                        )
                    ]
            end;
        _ ->
            Request = []
    end,
    % schedule next tweet
    schedule_tweet(),
    % randomly disconnect the client
    % the way this works is if we stop the client process then the supervisor will automatically spawn a new client process and that process will log in as existing client and do the handshake and start making requests
    case rand:uniform(1000) of
        1 ->
            ?PRINT("[~p] client disconnecting.....~n", [UserID]),
            {stop, normal, State#state{pending_requests = PendingRequests ++ Request}};
        _ ->
            {noreply, State#state{pending_requests = PendingRequests ++ Request}}
    end;
handle_cast(
    {tweet, TweetContent},
    #state{
        conn_pid = ConnPid,
        stream_ref = StreamRef,
        pending_requests = PendingRequests,
        hmac_key = HMACKey
    } = State
) ->
    TweetRequest = send_signed_ws_message(
        ConnPid, StreamRef, #{tweet => #{content => TweetContent}}, HMACKey
    ),
    {noreply, State#state{pending_requests = [TweetRequest | PendingRequests]}};
handle_cast(
    {re_tweet, TweetID},
    #state{
        conn_pid = ConnPid,
        stream_ref = StreamRef,
        pending_requests = PendingRequests,
        hmac_key = HMACKey
    } = State
) ->
    RetweetRequest = send_signed_ws_message(
        ConnPid, StreamRef, #{re_tweet => #{tweet_id => TweetID}}, HMACKey
    ),
    {noreply, State#state{pending_requests = [RetweetRequest | PendingRequests]}};
handle_cast(
    {schedule_mention_query},
    #state{
        conn_pid = ConnPid,
        stream_ref = StreamRef,
        pending_requests = PendingRequests,
        hmac_key = HMACKey
    } = State
) ->
    % make a mention query with 10% chance
    case rand:uniform(10) of
        1 -> Request = [send_signed_ws_message(ConnPid, StreamRef, #{mentions => true}, HMACKey)];
        _ -> Request = []
    end,
    % schedule next query
    schedule_mention_query(),
    {noreply, State#state{pending_requests = PendingRequests ++ Request}};
handle_cast(
    {mentions},
    #state{
        conn_pid = ConnPid,
        stream_ref = StreamRef,
        pending_requests = PendingRequests,
        hmac_key = HMACKey
    } = State
) ->
    MentionRequest = send_signed_ws_message(ConnPid, StreamRef, #{mentions => true}, HMACKey),
    {noreply, State#state{pending_requests = [MentionRequest | PendingRequests]}};
handle_cast(
    {query, Query},
    #state{
        conn_pid = ConnPid,
        stream_ref = StreamRef,
        pending_requests = PendingRequests,
        hmac_key = HMACKey
    } = State
) ->
    QueryRequest = send_signed_ws_message(ConnPid, StreamRef, #{query => Query}, HMACKey),
    {noreply, State#state{pending_requests = [QueryRequest | PendingRequests]}}.

%% web socket connection establishment
handle_info(
    {gun_up, ConnPid, _},
    #state{conn_pid = ConnPid} = State
) ->
    StreamRef = gun:ws_upgrade(ConnPid, "/ws"),
    {noreply, State#state{stream_ref = StreamRef}};
handle_info(
    {gun_upgrade, _, _, [<<"websocket">>], _},
    #state{user_id = _UserID} = State
) ->
    {noreply, State};
%% user registration after connection establishment
handle_info(
    {gun_ws, ConnPid, StreamRef, {text, <<"ack">>}},
    #state{
        user_id = UserID,
        conn_pid = ConnPid,
        stream_ref = StreamRef,
        pending_requests = PendingRequests,
        rsa_key_pair = {RSAPublicKey, _},
        ecdh_key_pair = {ECDHPublicKey, _},
        new_user = NewUser,
        authenticated = false
    } = State
) ->
    case NewUser of
        true ->
            RequestID = send_ws_message(ConnPid, StreamRef, #{
                register => #{user_id => UserID},
                rsa_public_key => util:encode_public_key(RSAPublicKey),
                ecdh_public_key => util:encode_data(ECDHPublicKey)
            });
        false ->
            RequestID = send_ws_message(ConnPid, StreamRef, #{
                login => #{user_id => UserID},
                rsa_public_key => util:encode_public_key(RSAPublicKey),
                ecdh_public_key => util:encode_data(ECDHPublicKey)
            })
    end,
    {noreply, State#state{pending_requests = [RequestID | PendingRequests]}};
handle_info(
    {gun_ws, ConnPid, StreamRef, {text, SerializedJSON}},
    #state{
        user_id = UserID,
        conn_pid = ConnPid,
        stream_ref = StreamRef,
        n = N,
        total_followers = TotalFollowers,
        request_times = RequestTimes,
        pending_requests = PendingRequests,
        authenticated = false,
        rsa_key_pair = {_, RSAPrivateKey},
        ecdh_key_pair = {_, ECDHPrivateKey},
        standalone = Standalone
    } = State
) ->
    RequestMap = util:decode_json(SerializedJSON),
    case RequestMap of
        #{
            <<"request_id">> := RequestID,
            <<"success">> := #{<<"register">> := true},
            <<"ecdh_public_key">> := EncodedServerECDHPublicKey
        } ->
            ServerECDHPublicKey = util:decode_data(EncodedServerECDHPublicKey),
            {UpdatedRequestTimes, UpdatedPendingRequests} = update_request_times(
                register_account, RequestID, RequestTimes, PendingRequests
            ),
            ?PRINT("[~s, ~p] registered successfully~n", [UserID, RequestID]),
            % store the self pid in table for others to read
            c_store:store_client_pid(UserID),
            %% if starting as a standalone client then dont follow anyone automatically
            case Standalone of
                true ->
                    {noreply,
                        State#state{
                            request_times = UpdatedRequestTimes,
                            pending_requests = UpdatedPendingRequests,
                            authenticated = true,
                            hmac_key = util:generate_hmac_key(ECDHPrivateKey, ServerECDHPublicKey)
                        }};
                false ->
                    IntID = list_to_integer(UserID),
                    Followers = generate_follow_users(
                        TotalFollowers, lists:delete(IntID, lists:seq(1, N)), []
                    ),
                    % wait for START_TIME milliseconds before making users follow current user so that other users have enough time to start up and register themselves
                    {noreply,
                        State#state{
                            followers = Followers,
                            request_times = UpdatedRequestTimes,
                            pending_requests = UpdatedPendingRequests,
                            authenticated = true,
                            hmac_key = util:generate_hmac_key(ECDHPrivateKey, ServerECDHPublicKey)
                        },
                        ?START_TIME}
            end;
        #{
            <<"request_id">> := RequestID,
            <<"success">> := #{<<"login">> := true},
            <<"challenge">> := Challenge,
            <<"ecdh_public_key">> := EncodedServerECDHPublicKey
        } ->
            ServerECDHPublicKey = util:decode_data(EncodedServerECDHPublicKey),
            SignedChallenge = util:sign_challenge(
                util:encode_json(#{
                    timestamp => erlang:system_time(millisecond), challenge => Challenge
                }),
                RSAPrivateKey
            ),
            RequestID = send_ws_message(RequestID, ConnPid, StreamRef, #{
                signed_challenge => util:encode_data(SignedChallenge)
            }),
            {noreply, State#state{
                hmac_key = util:generate_hmac_key(ECDHPrivateKey, ServerECDHPublicKey)
            }};
        #{<<"request_id">> := RequestID, <<"success">> := #{<<"challenge">> := true}} ->
            % store the self pid in table for others to read
            c_store:store_client_pid(UserID),
            {UpdatedRequestTimes, UpdatedPendingRequests} = update_request_times(
                login, RequestID, RequestTimes, PendingRequests
            ),
            % if working as a standalone client then dont generate automatic content
            case Standalone of
                true ->
                    ok;
                false ->
                    % start generating content
                    schedule_tweet(),
                    schedule_mention_query()
            end,
            ?PRINT("[~p] client logged in~n", [UserID]),
            {noreply, State#state{
                authenticated = true,
                request_times = UpdatedRequestTimes,
                pending_requests = UpdatedPendingRequests
            }};
        #{<<"error">> := #{<<"challenge">> := false}} ->
            ?PRINT("[~s] login failed!~n", [UserID]),
            % restart the client to try logging in again
            {stop, normal, State}
    end;
%% acknowledgement messages
handle_info(
    {gun_ws, ConnPid, StreamRef, {text, SerializedJSON}},
    #state{
        user_id = UserID,
        conn_pid = ConnPid,
        stream_ref = StreamRef,
        request_times = RequestTimes,
        pending_requests = PendingRequests,
        authenticated = true
    } = State
) ->
    RequestMap = util:decode_json(SerializedJSON),
    case RequestMap of
        #{<<"error">> := Data} ->
            ?PRINT("Error in request ~p ~n", [Data]),
            {noreply, State};
        #{
            <<"tweet">> := #{
                <<"poster_id">> := PosterID,
                <<"tweet_type">> := TweetType,
                <<"content">> := TweetContent
            }
        } ->
            ?PRINT("[~s] ~s received from ~s: ~s~n", [UserID, TweetType, PosterID, TweetContent]),
            {noreply, State};
        #{<<"request_id">> := RequestID, <<"success">> := #{<<"follow">> := FollowUserID}} ->
            {UpdatedRequestTimes, UpdatedPendingRequests} = update_request_times(
                follow_user, RequestID, RequestTimes, PendingRequests
            ),
            ?PRINT("[~s, ~p] Followed ~s~n", [UserID, RequestID, FollowUserID]),
            {noreply, State#state{
                request_times = UpdatedRequestTimes, pending_requests = UpdatedPendingRequests
            }};
        #{
            <<"request_id">> := RequestID,
            <<"success">> := #{<<"tweet">> := #{<<"type">> := TweetType, <<"id">> := TweetID}}
        } ->
            {UpdatedRequestTimes, UpdatedPendingRequests} = update_request_times(
                publish_tweet, RequestID, RequestTimes, PendingRequests
            ),
            ?PRINT("[~s, ~p] ~p published with ID ~s~n", [UserID, RequestID, TweetType, TweetID]),
            {noreply, State#state{
                request_times = UpdatedRequestTimes, pending_requests = UpdatedPendingRequests
            }};
        #{
            <<"request_id">> := RequestID,
            <<"success">> := #{<<"query">> := Query, <<"result">> := QueryResult}
        } ->
            {UpdatedRequestTimes, UpdatedPendingRequests} = update_request_times(
                query, RequestID, RequestTimes, PendingRequests
            ),
            ?PRINT("[~s, ~p] Query ~s result ~s~n", [
                UserID,
                RequestID,
                Query,
                lists:map(
                    fun(#{<<"poster_id">> := PosterID, <<"content">> := Content}) ->
                        " {[" ++ PosterID ++ "]: " ++ Content ++ "} "
                    end,
                    QueryResult
                )
            ]),
            {noreply, State#state{
                request_times = UpdatedRequestTimes, pending_requests = UpdatedPendingRequests
            }};
        _ ->
            {noreply, State}
    end;
%% connection close related stuff
handle_info({gun_ws, _, _, {close, _, _}}, #state{ user_id = UserID } = State) ->
    ?PRINT("[~p] Client hasn't send any messages in a minute so server has closed the connection, reconnecting and logging in...~n", [UserID]),
    {noreply, State};
handle_info({gun_down, _ConnPid, _Protocol, _Reason, _Killed},State) ->
    %% Do something.
    {noreply, State#state{authenticated = false, new_user = false}};
%% handle initial cooldown timeout before starting the simulation
handle_info(timeout, #state{n = N, user_id = UserID, followers = Followers} = State) ->
    case c_store:get_alive_clients() of
        N ->
            lists:foreach(
                fun(Follower) ->
                    PID = c_store:get_client_pid(Follower),
                    % Make the Follower follow the current user
                    gen_server:cast(PID, {follow, UserID})
                end,
                Followers
            ),
            schedule_tweet(),
            schedule_mention_query(),
            {noreply, State};
        _ ->
            {noreply, State, ?START_TIME}
    end.

terminate(
    Reason,
    #state{
        user_id = UserID, start_time = StartTime, request_times = RequestTimes, conn_pid = ConnPid, standalone = Standalone
    } = State
) ->
    c_store:delete_client_pid(UserID),
    ok = gun:shutdown(ConnPid),
    case Standalone of
        true ->
            ok;
        false ->
            % if the client is a part of simulator then send statistics to the main process
            RequestTimesDict = lists:foldr(
                fun({K, V}, D) -> dict:append(K, V, D) end, dict:new(), RequestTimes
            ),
            TerminationReason =
                case Reason of
                    normal -> disconnect;
                    Other -> Other
                end,
            gen_server:cast(main, {TerminationReason, UserID, StartTime, RequestTimesDict})
    end,
    {stop, Reason, State}.
code_change(_OldVsn, _State, _Extra) ->
    ok.

%% Utility functions not to be exposed

send_ws_message(ConnPid, StreamRef, Message) ->
    RequestID = erlang:system_time(nanosecond),
    send_ws_message(RequestID, ConnPid, StreamRef, Message).
send_ws_message(RequestID, ConnPid, StreamRef, Message) ->
    gun:ws_send(
        ConnPid,
        StreamRef,
        {text, util:encode_json(maps:merge(#{request_id => RequestID}, Message))}
    ),
    RequestID.
send_signed_ws_message(ConnPid, StreamRef, Message, HMACKey) ->
    RequestID = erlang:system_time(nanosecond),
    Data = util:encode_json(maps:merge(#{request_id => RequestID}, Message)),
    EncodedHMAC = util:encode_data(util:generate_hmac(HMACKey, Data)),
    gun:ws_send(
        ConnPid,
        StreamRef,
        {text,
            util:encode_json(#{
                data => maps:merge(#{request_id => RequestID}, Message), hmac => EncodedHMAC
            })}
    ),
    RequestID.

% first parameter calculates the approx no. of users required for rank "ID" to get a zipf distribution with s = 1
generate_follow_users(0, _, List) ->
    List;
generate_follow_users(N, UsersList, _) when N =:= length(UsersList) ->
    UsersList;
generate_follow_users(N, UsersList, List) ->
    {Left, [Follower | Right]} = lists:split(rand:uniform(length(UsersList)) - 1, UsersList),
    generate_follow_users(N - 1, Left ++ Right, [integer_to_list(Follower) | List]).

schedule_tweet() ->
    erlang:send_after(?TWEET_INTERVAL, self(), {'$gen_cast', {schedule_tweet}}).

schedule_mention_query() ->
    erlang:send_after(?QUERY_INTERVAL, self(), {'$gen_cast', {schedule_mention_query}}).

update_request_times(Operation, RequestID, RequestTimes, PendingRequests) ->
    UpdatedPendingRequests = lists:delete(RequestID, PendingRequests),
    UpdatedRequestTimes = [{Operation, erlang:system_time(nanosecond) - RequestID} | RequestTimes],
    {UpdatedRequestTimes, UpdatedPendingRequests}.

send_cast(UserID, Message) ->
    case c_store:get_client_pid(UserID) of
        null ->
            ?PRINT("User ~p is not logged in!~n", [UserID]);
        PID ->
            gen_server:cast(PID, Message)
    end.
