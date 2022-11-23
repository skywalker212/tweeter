-module(cache).
-export([insert/2, lookup/1, delete/1]).

insert(Key, Value) ->
    case c_store:lookup(Key) of
        {ok, Pid} ->
            c_element:replace(Pid, Value);
        {error, _} ->
            {ok, Pid} = c_element:create(Value),
            c_store:insert(Key, Pid)
    end.

lookup(Key) ->
    try
        {ok, Pid} = c_store:lookup(Key),
        {ok, Value} = c_element:fetch(Pid),
        {ok, Value}
    catch
        _Class:_Exception ->
            {error, not_found}
    end.

delete(Key) ->
    case c_store:lookup(Key) of
        {ok, Pid} ->
            c_element:delete(Pid);
        {error, _Reason} ->
            ok
    end.
