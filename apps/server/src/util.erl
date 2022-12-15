-module(util).
-export([
    get_utc_seconds/0,
    get_utc_seconds/1,
    strip_whitespace/1,
    decode_json/1,
    encode_json/1,
    encode_data/1,
    decode_data/1,
    generate_challenge/0,
    generate_key_pair/1,
    generate_hmac_key/2,
    encode_public_key/1,
    decode_public_key/1,
    sign_challenge/2,
    verify_challenge/4
]).
-include_lib("public_key/include/public_key.hrl").

get_utc_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

get_utc_seconds(SecondsBefore) ->
    get_utc_seconds() - SecondsBefore.

strip_whitespace(String) ->
    re:replace(String, "(^\\s+)|(\\s+$)", "", [global, {return, list}]).

encode_json(Map) ->
    jiffy:encode(Map).

decode_json(SerializedJSON) ->
    update_map(jiffy:decode(SerializedJSON, [return_maps, copy_strings])).

encode_data(BinaryData) ->
    base64:encode_to_string(BinaryData).

decode_data(Base64) ->
    base64:decode(Base64).

% function to convert every binary value of map to a list
update_map(Map) ->
    maps:map(
        fun(_, V) ->
            case is_binary(V) of
                true ->
                    binary_to_list(V);
                false ->
                    case is_map(V) of
                        true -> update_map(V);
                        false -> V
                    end
            end
        end,
        Map
    ).

%% functions for security ------ TOP SECRET

generate_challenge() ->
    generate_random_list(32, []).
% returns {PublicKey, PrivateKey}
generate_key_pair(rsa) ->
    generate_rsa_key_pair();
generate_key_pair(ecdh) ->
    generate_ecdh_key_pair().

generate_rsa_key_pair() ->
    PrivateKey = public_key:generate_key({rsa, 2048, 65537}),
    {
        #'RSAPublicKey'{
            modulus = PrivateKey#'RSAPrivateKey'.modulus,
            publicExponent = PrivateKey#'RSAPrivateKey'.publicExponent
        },
        PrivateKey
    }.

generate_ecdh_key_pair() ->
    crypto:generate_key(ecdh, secp192k1).

generate_hmac_key(MyPrivateKey, OthersPublicKey) ->
    crypto:compute_key(ecdh, OthersPublicKey, MyPrivateKey, secp192k1).

encode_public_key(PublicKey) ->
    PemEntry = public_key:pem_entry_encode('RSAPublicKey', PublicKey),
    public_key:pem_encode([PemEntry]).

decode_public_key(PublicKeyBinary) ->
    [PemEntry] = public_key:pem_decode(PublicKeyBinary),
    public_key:pem_entry_decode(PemEntry).

sign_challenge(Challenge, PrivKey) ->
    public_key:encrypt_private(Challenge, PrivKey).

verify_challenge(SignedChallenge, OriginalChallenge, ChallengeTimestamp, PubKey) ->
    try
        #{<<"timestamp">> := _, <<"challenge">> := OriginalChallenge} = decode_json(
            public_key:decrypt_public(SignedChallenge, PubKey)
        )
    of
        _ -> erlang:system_time(millisecond) =< ChallengeTimestamp + 1000
    catch
        _ -> false
    end.

generate_random_list(0, List) -> List;
generate_random_list(N, List) -> generate_random_list(N - 1, [generate_random_char() | List]).

generate_random_char() -> rand:uniform(93) + 33.
