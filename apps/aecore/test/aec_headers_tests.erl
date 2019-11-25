-module(aec_headers_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-include("blocks.hrl").

-import(aec_headers, [raw_key_header/0,
                      raw_micro_header/0
                     ]).

-define(TEST_MODULE, aec_headers).
-define(GENESIS_HEIGHT, aec_block_genesis:height()).
-define(GENESIS_VERSION, aec_block_genesis:version()).
-define(GENESIS_TIME, aec_block_genesis:time_in_msecs()).

network_key_serialization_test() ->
    Header = raw_key_header(),
    SerializedHeader = ?TEST_MODULE:serialize_to_binary(Header),
    DeserializedHeader =
        ?TEST_MODULE:deserialize_from_binary(SerializedHeader),
    ?assertEqual(Header, DeserializedHeader),
    ?assertEqual(SerializedHeader,
                 ?TEST_MODULE:serialize_to_binary(DeserializedHeader)).

network_micro_serialization_test() ->
    Header = raw_micro_header(),
    SerializedHeader = ?TEST_MODULE:serialize_to_binary(Header),
    DeserializedHeader =
        ?TEST_MODULE:deserialize_from_binary(SerializedHeader),
    ?assertEqual(Header, DeserializedHeader),
    ?assertEqual(SerializedHeader,
                 ?TEST_MODULE:serialize_to_binary(DeserializedHeader)).

hash_test() ->
    {ok, _HeaderHash1} = ?TEST_MODULE:hash_header(raw_key_header()),
    {ok, _HeaderHash2} = ?TEST_MODULE:hash_header(raw_micro_header()).

raw_key_header_minerva(MinervaHeight) ->
    ?TEST_MODULE:set_version_and_height(raw_key_header(), ?MINERVA_PROTOCOL_VSN, MinervaHeight).

raw_key_header_roma(MinervaHeight) ->
    ?TEST_MODULE:set_version_and_height(raw_key_header(), ?ROMA_PROTOCOL_VSN, MinervaHeight - 1).

raw_key_header_fortuna(FortunaHeight) ->
    ?TEST_MODULE:set_version_and_height(raw_key_header(), ?FORTUNA_PROTOCOL_VSN, FortunaHeight).

info_test_() ->
    MinervaHeight = 10,
    FortunaHeight = 15,
    {foreach,
     fun() ->
             meck:new(aec_hard_forks, [passthrough]),
             meck:expect(aec_hard_forks, protocol_effective_at_height,
                         fun(X) when X <  MinervaHeight -> ?ROMA_PROTOCOL_VSN;
                            (X) when X <  FortunaHeight -> ?MINERVA_PROTOCOL_VSN;
                            (X) when X >= FortunaHeight -> ?FORTUNA_PROTOCOL_VSN
                         end),
             ok
     end,
     fun(_) ->
             meck:unload(aec_hard_forks)
     end,
     [{"Serialization/deserialization of set info",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               WithInfo = ?TEST_MODULE:set_info(RawKey, 123),
               SerializedWithInfo = ?TEST_MODULE:serialize_to_binary(WithInfo),
               ?assertEqual(WithInfo,
                            ?TEST_MODULE:deserialize_from_binary(SerializedWithInfo)),
               ok
       end},
      {"Client serialization/deserialization of set info",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               WithInfo = ?TEST_MODULE:set_info(RawKey, 123),
               SerializedWithInfo = ?TEST_MODULE:serialize_for_client(WithInfo, key),
               Serialized = SerializedWithInfo#{<<"nonce">> => ?TEST_MODULE:nonce(WithInfo),
                                                <<"pow">>   => ?TEST_MODULE:pow(WithInfo)
                                               },
               ?assertEqual({ok, WithInfo},
                            ?TEST_MODULE:deserialize_from_client(key, Serialized)),
               ok
       end},
      {"Serialization/deserialization of unset info",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               WithInfo = ?TEST_MODULE:set_info(RawKey, default),
               SerializedWithInfo = ?TEST_MODULE:serialize_to_binary(WithInfo),
               ?assertEqual(WithInfo,
                            ?TEST_MODULE:deserialize_from_binary(SerializedWithInfo)),
               ok
       end},
      {"Client serialization/deserialization of unset info",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               WithInfo = ?TEST_MODULE:set_info(RawKey, default),
               SerializedWithInfo = ?TEST_MODULE:serialize_for_client(WithInfo, key),
               Serialized = SerializedWithInfo#{<<"nonce">> => ?TEST_MODULE:nonce(WithInfo),
                                                <<"pow">>   => ?TEST_MODULE:pow(WithInfo)
                                               },
               ?assertEqual({ok, WithInfo},
                            ?TEST_MODULE:deserialize_from_client(key, Serialized)),
               ok
       end},
      {"Serialization of set info in Roma",
       fun() ->
               RawKey = raw_key_header_roma(MinervaHeight),
               WithInfo = ?TEST_MODULE:set_info(RawKey, default),
               ?assertMatch(X when is_binary(X), ?TEST_MODULE:serialize_to_binary(WithInfo)),
               ok
       end},
      {"Deserialization of set info in Roma",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               RomaHeight = ?TEST_MODULE:height(raw_key_header_roma(MinervaHeight)),
               WithInfo = ?TEST_MODULE:set_info(RawKey, 123),
               SerMinerva = ?TEST_MODULE:serialize_to_binary(WithInfo),
               CommonVersionBits = 32,
               CommonFlagsBits = 32,
               CommonHeightBits = 64,
               <<?MINERVA_PROTOCOL_VSN:CommonVersionBits, Flags:CommonFlagsBits, MinervaHeight:CommonHeightBits, Rest/binary>> = SerMinerva,
               SerRoma = <<?ROMA_PROTOCOL_VSN:CommonVersionBits, Flags:CommonFlagsBits, RomaHeight:CommonHeightBits, Rest/binary>>,
               ?assertException(error, malformed_header,
                                ?TEST_MODULE:deserialize_from_binary(SerRoma)),
               ok
       end},
      {"Deserialization of too big info",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               WithInfo = ?TEST_MODULE:set_info(RawKey, 123),
               SerMinerva = ?TEST_MODULE:serialize_to_binary(WithInfo),
               TestBinary = <<SerMinerva/binary, 0:8>>,
               ?assertException(error, malformed_header,
                               ?TEST_MODULE:deserialize_from_binary(TestBinary)),
               ok
       end},
      {"Deserialization of too small info",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               WithInfo = ?TEST_MODULE:set_info(RawKey, 123),
               SerMinerva = ?TEST_MODULE:serialize_to_binary(WithInfo),
               Size = byte_size(SerMinerva) - 1,
               <<TestBinary:Size/binary, _:1/unit:8>> = SerMinerva,
               ?assertException(error, malformed_header,
                               ?TEST_MODULE:deserialize_from_binary(TestBinary)),
               ok
       end},
      {"Deserialization of no info with info flag set",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               WithInfo = ?TEST_MODULE:set_info(RawKey, 123),
               SerMinerva = ?TEST_MODULE:serialize_to_binary(WithInfo),
               Size = byte_size(SerMinerva) - ?OPTIONAL_INFO_BYTES,
               <<TestBinary:Size/binary, _:?OPTIONAL_INFO_BYTES/unit:8>> = SerMinerva,
               ?assertException(error, malformed_header,
                               ?TEST_MODULE:deserialize_from_binary(TestBinary)),
               ok
       end},
      {"Default value of the info field in the pre release of Fortuna: Minerva protocol",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               WithInfo = ?TEST_MODULE:new_key_header(
                             ?TEST_MODULE:height(RawKey),
                             ?TEST_MODULE:prev_hash(RawKey),
                             ?TEST_MODULE:prev_key_hash(RawKey),
                             ?TEST_MODULE:root_hash(RawKey),
                             ?TEST_MODULE:miner(RawKey),
                             ?TEST_MODULE:beneficiary(RawKey),
                             ?TEST_MODULE:target(RawKey),
                             ?TEST_MODULE:pow(RawKey),
                             ?TEST_MODULE:nonce(RawKey),
                             ?TEST_MODULE:time_in_msecs(RawKey),
                             default,
                             ?MINERVA_PROTOCOL_VSN),
               Info = ?KEY_HEADER_INFO_FORTUNA_POINT_RELEASE,
               ?assertEqual(Info, ?TEST_MODULE:info(WithInfo))
       end},
      {"Default value of the info field in the pre release of Fortuna: Roma protocol",
       fun() ->
               RawKey = raw_key_header_roma(MinervaHeight),
               WithInfo = ?TEST_MODULE:new_key_header(
                             ?TEST_MODULE:height(RawKey),
                             ?TEST_MODULE:prev_hash(RawKey),
                             ?TEST_MODULE:prev_key_hash(RawKey),
                             ?TEST_MODULE:root_hash(RawKey),
                             ?TEST_MODULE:miner(RawKey),
                             ?TEST_MODULE:beneficiary(RawKey),
                             ?TEST_MODULE:target(RawKey),
                             ?TEST_MODULE:pow(RawKey),
                             ?TEST_MODULE:nonce(RawKey),
                             ?TEST_MODULE:time_in_msecs(RawKey),
                             default,
                             ?ROMA_PROTOCOL_VSN),
               Info = undefined,
               ?assertEqual(Info, ?TEST_MODULE:info(WithInfo))
       end},
      {"Default value of the info field in the pre release of Fortuna: Fortuna protocol",
       fun() ->
               RawKey = raw_key_header_fortuna(FortunaHeight),
               WithInfo = ?TEST_MODULE:new_key_header(
                             ?TEST_MODULE:height(RawKey),
                             ?TEST_MODULE:prev_hash(RawKey),
                             ?TEST_MODULE:prev_key_hash(RawKey),
                             ?TEST_MODULE:root_hash(RawKey),
                             ?TEST_MODULE:miner(RawKey),
                             ?TEST_MODULE:beneficiary(RawKey),
                             ?TEST_MODULE:target(RawKey),
                             ?TEST_MODULE:pow(RawKey),
                             ?TEST_MODULE:nonce(RawKey),
                             ?TEST_MODULE:time_in_msecs(RawKey),
                             default,
                             ?FORTUNA_PROTOCOL_VSN),
               Info = ?KEY_HEADER_INFO_FORTUNA_POINT_RELEASE,
               ?assertEqual(Info, ?TEST_MODULE:info(WithInfo))
       end}
     ]}.
