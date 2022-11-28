-module(util).
-export([
    get_utc_seconds/0,
    get_utc_seconds/1,
    strip_whitespace/1,
    generate_string/0
]).

get_utc_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

get_utc_seconds(SecondsBefore) ->
    get_utc_seconds() - SecondsBefore.

strip_whitespace(String) ->
    re:replace(String, "(^\\s+)|(\\s+$)", "", [global, {return, list}]).

generate_string() -> generate_random_list(32, []).

generate_random_list(0, List) -> List;
generate_random_list(N, List) -> generate_random_list(N - 1, [generate_random_char() | List]).

generate_random_char() -> rand:uniform(93) + 33.
