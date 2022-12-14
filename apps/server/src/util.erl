-module(util).
-export([
    get_utc_seconds/0,
    get_utc_seconds/1,
    strip_whitespace/1,
    decode_json/1,
    encode_json/1
]).

get_utc_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

get_utc_seconds(SecondsBefore) ->
    get_utc_seconds() - SecondsBefore.

strip_whitespace(String) ->
    re:replace(String, "(^\\s+)|(\\s+$)", "", [global, {return, list}]).

decode_json(SerializedJSON) ->
    update_map(jiffy:decode(SerializedJSON, [return_maps, copy_strings])).

encode_json(Map) ->
    jiffy:encode(Map).

% function to convert every binary value of map to a list
update_map(Map) ->
    maps:map(fun (_, V) -> 
        case is_binary(V) of
            true -> binary_to_list(V);
            false ->
                case is_map(V) of
                    true -> update_map(V);
                    false -> V
                end
        end
    end, Map).