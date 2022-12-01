-module(util).
-export([
    get_utc_seconds/0,
    get_utc_seconds/1,
    strip_whitespace/1,
    get_timestamp/0
]).

get_utc_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

get_utc_seconds(SecondsBefore) ->
    get_utc_seconds() - SecondsBefore.

strip_whitespace(String) ->
    re:replace(String, "(^\\s+)|(\\s+$)", "", [global, {return, list}]).

get_timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).
