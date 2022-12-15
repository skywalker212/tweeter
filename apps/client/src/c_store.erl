-module(c_store).

-export([
    init/0,
    cleanup/0,
    store_key_pair/2,
    get_key_pair/1,
    store_client_pid/1,
    get_alive_clients/0,
    get_client_pid/1,
    delete_client_pid/1
]).

-define(CLIENT_PID_TABLE_NAME, client_pid).
-define(KEY_TABLE_NAME, key_table).

init() ->
    TableOptions = [
        public,
        named_table,
        {write_concurrency, auto},
        {read_concurrency, true}
    ],
    % create table to store client pids and key pairs
    ets:new(?CLIENT_PID_TABLE_NAME, TableOptions),
    ets:new(?KEY_TABLE_NAME, TableOptions),
    % store words to be used in tweets in an ETS table
    generator:save_words().

store_key_pair(UserID, KeyPair) ->
    ets:insert(?KEY_TABLE_NAME, {UserID, KeyPair}).

get_key_pair(UserID) ->
    get_record_from_table(?KEY_TABLE_NAME, UserID).

store_client_pid(UserID) ->
    ets:insert(?CLIENT_PID_TABLE_NAME, {UserID, self()}).

get_alive_clients() ->
    ets:info(?CLIENT_PID_TABLE_NAME, size).

get_client_pid(UserID) ->
    get_record_from_table(?CLIENT_PID_TABLE_NAME, UserID).

get_record_from_table(TableName, Key) ->
    case ets:lookup(TableName, Key) of
        [{_, Match}] -> Match;
        [] -> null
    end.

delete_client_pid(UserID) ->
    ets:delete(?CLIENT_PID_TABLE_NAME, UserID).

cleanup() ->
    ets:delete(?CLIENT_PID_TABLE_NAME),
    ets:delete(?KEY_TABLE_NAME).