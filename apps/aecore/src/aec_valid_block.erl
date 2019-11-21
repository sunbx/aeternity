%%%=============================================================================
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%% TODO
%%% @end
%%%=============================================================================

-module(aec_valid_block).

%% API
-export([new/2,
         check/3,
         pending_env/2,
         pending_checks/1,
         block/1,
         set_txs/2,
         origin/1,
         type/1
        ]).

-export([block_presence/1,
         pow/1,
         max_time/1,
         prev_header/2,
         protocol/2,
         chain_connection/1,
         pof/2,
         cycle_time/2,
         delta_height/2,
         max_time/1,
         signature/2,
         txs_hash/2,
         gas_limit/2,
         txs_fee/2
        ]).

-export_type([block/0]).

-include_lib("aeminer/include/aeminer.hrl").

-define(PEER_CONN, aec_peer_connection).
-define(SYNC, aec_sync).
-define(CONDUCTOR, aec_conductor).

-define(KEY_HEADER_SYNC_CHECKS,
        [{block_presence,   [header]},
         {pow,              [header]},
         {max_time,         [header]},
         {prev_header,      [header, prev_header]},
         {protocol,         [header, protocol],     #{process => ?SYNC}},
         {chain_connection, [header],               #{process => ?SYNC}}
        ]).

-define(KEY_HEADER_GOSSIP_CHECKS,
        [{gossiped_height,  [header, sync_height]},
         {block_presence,   [header]},
         {chain_connection, [header]},
         {delta_height,     [header, top_header]},
         {prev_header,      [header, prev_header]},
         {protocol,         [header, prev_key_header]},
         {cycle_time,       [header, prev_header]},
         {max_time,         [header]},
         {signature,        [header, prev_key_header]}
        ]).

-define(MICRO_HEADER_SYNC_CHECKS,
        [{block_presence,   [header]},
         {max_time,         [header]},
         {prev_header,      [header, prev_header]},
         {protocol,         [header, prev_key_header]},
         {cycle_time,       [header, prev_header]},
         {signature,        [header, prev_key_header]},
         {pof,              [header, pof]},
         {txs_hash,         [header, txs]},
         {gas_limit,        [header, txs]},
         {txs_fee,          [header, txs]},
         {chain_connection, [header],                   #{process => ?SYNC}}
        ]).

-define(KEY_HEADER_HTTP_CHECKS, []).

-define(MICRO_HEADER_GOSSIP_CHECKS,
        [{gossiped_height,  [header, sync_height]},
         {block_presence,   [header]},
         {chain_connection, [header]},
         {delta_height,     [header, top_header]},
         {prev_header,      [header, prev_header]},
         {protocol,         [header, prev_key_header]},
         {cycle_time,       [header, prev_header]},
         {max_time,         [header]},
         {signature,        [header, prev_key_header]},
         {txs_hash,         [header, txs]}
        ]).

-define(LIGHT_MICRO_HEADER_GOSSIP_CHECKS,
        [{gossiped_height,  [header, sync_height]},
         {protocol,         [header, prev_key_header]},
         {cycle_time,       [header, prev_header]},
         {max_time,         [header]},
         {chain_connection, [header]},
         {delta_height,     [header, top_header]},
         {prev_header,      [header, prev_header]},
         {signature,        [header, prev_key_header]},
         {pof,              [header, pof]}
        ]).

-define(ENV_HEADER_HASHES,
        #{top_header      => top_header_hash,
          prev_header     => prev_header_hash,
          prev_key_header => prev_key_header_hash}).

-record(valid_block,
        {block,
         checks,
         origin
        }).

-opaque block() :: #valid_block{}.

%% API

new(Block, Origin) ->
    #valid_block{block  = Block,
                 checks = block_checks(Block, Origin),
                 origin = Origin}.

check(#valid_block{block = Block, checks = Checks} = VBlock, Env, Opts) ->
    HeaderChecks = maps:get(header, Checks),
    Env2 = env(Block, Env),
    case perform_checks(HeaderChecks, Env2, Opts) of
        {ok, PendingChecks, DoneChecks} ->
            Checks2 = Checks#{header => #{pending => PendingChecks,
                                          done => DoneChecks}},
            {ok, VBlock#valid_block{checks = Checks2}};
        {error, _Rsn} = Err ->
            Err
    end.

pending_env(#valid_block{checks = Checks}, all) ->
    #{header := #{pending := HChecks}} = Checks,
    maps:keys(get_pending_env(HChecks, #{}));
pending_env(#valid_block{checks = Checks}, db) ->
    #{header := #{pending := HChecks}} = Checks,
    PendingEnvKeys = get_pending_env(HChecks, #{}),
    maps:keys(maps:with([prev_header, prev_key_header], PendingEnvKeys)).

pending_checks(#valid_block{checks = #{header := #{pending := HChecks}}}) ->
    HChecks.

block(#valid_block{block = Block}) ->
    Block.

set_txs(Txs, #valid_block{block = Block} = VBlock) ->
    VBlock#valid_block{block = aec_blocks:set_txs(Block, Txs)}.

origin(#valid_block{origin = Origin}) ->
    Origin.

type(#valid_block{block = Block}) ->
    aec_blocks:type(Block).

%% Internal functions

env(Block, Env) ->
    env(aec_blocks:type(Block), Block, Env).

env(key, Block, Env) ->
    Env#{header => aec_blocks:to_key_header(Block)};
env(micro, Block, Env) ->
    Env#{header => aec_blocks:to_micro_header(Block),
         txs    => aec_blocks:txs(Block),
         pof    => aec_blocks:pof(Block)}.

block_checks(Block, Origin) ->
    Type = aec_blocks:type(Block),
    block_checks(Type, Block, Origin).

block_checks(key, _Block, sync) ->
    #{header => #{pending => ?KEY_HEADER_SYNC_CHECKS, done => []}};
block_checks(key, _Block, gossip) ->
    Checks = ?KEY_HEADER_GOSSIP_CHECKS,
    #{header => #{pending => Checks, done => []}};
block_checks(key, _Block, http) ->
    Checks = ?KEY_HEADER_HTTP_CHECKS,
    #{header => #{pending => Checks, done => []}};
block_checks(micro, _Block, sync) ->
    %% TODO: txs
    HeaderChecks = ?MICRO_HEADER_SYNC_CHECKS,
    #{header => #{pending => HeaderChecks, done => []}};
block_checks(micro, _Block, gossip) ->
    Checks = ?LIGHT_MICRO_HEADER_GOSSIP_CHECKS,
    #{header => #{pending => Checks, done => []}}.

perform_checks(#{pending := PendingChecks}, Env, Opts) ->
    perform_checks(PendingChecks, Env, Opts, [], []).

perform_checks([Check | Rest], Env, Opts, PendingAcc, DoneAcc) ->
    case perform_check(Check, Env, Opts) of
        ok ->
            perform_checks(Rest, Env, Opts, PendingAcc, [Check | DoneAcc]);
        skip ->
            perform_checks(Rest, Env, Opts, [Check | PendingAcc], DoneAcc);
        {error, _Rsn} = Err ->
            Err
    end;
perform_checks([], _Args, _Opts, PendingAcc, DoneAcc) ->
    {ok, lists:reverse(PendingAcc), lists:reverse(DoneAcc)}.

perform_check({Fun, EnvKeys, RequiredOpts}, Env, Opts) ->
    case has_matching_entries(RequiredOpts, Opts) of
        true  -> perform_check(Fun, EnvKeys, Env, Opts);
        false -> skip
    end;
perform_check({Fun, EnvKeys}, Env, Opts) ->
    perform_check(Fun, EnvKeys, Env, Opts).

perform_check(Fun, EnvKeys, Env, _Opts) ->
    case get_required_args(EnvKeys, Env, []) of
        {ok, Args}          -> apply(?MODULE, Fun, Args);
        defer_check         -> skip;
        {error, _Rsn} = Err -> Err
    end.

has_matching_entries(M1, M2) ->
    M1 =:= maps:with(maps:keys(M1), M2).

get_required_args([Key | Rest], Env, Acc) ->
    case maps:get(Key, Env) of
        [] when Key =:= txs -> defer_check;
        defer_check         -> defer_check;
        {error, _Rsn} = Err -> Err;
        Val                 -> get_required_args(Rest, Env, [Val | Acc])
    end;
get_required_args([], _Args, Acc) ->
    {ok, lists:reverse(Acc)}.

get_pending_env([{_Fun, EnvKeys, _Opts} | Rest], Acc) ->
    get_pending_env(Rest, add_keys(EnvKeys, Acc));
get_pending_env([{_Fun, EnvKeys} | Rest], Acc) ->
    get_pending_env(Rest, add_keys(EnvKeys, Acc));
get_pending_env([], Acc) ->
    Acc.

add_keys(EnvKeys, Acc) ->
    lists:foldl(
      fun(Key, Acc2) -> maps:put(Key, true, Acc2) end,
      Acc, EnvKeys).

%% Check functions

block_presence(Header) ->
    {ok, Hash} = aec_headers:hash_header(Header),
    case aec_db:has_block(Hash) of
        false -> ok;
        true  -> {error, already_present}
    end.

protocol(Header, Protocol) when is_integer(Protocol) ->
    case aec_headers:version(Header) =:= Protocol of
        true  -> ok;
        false -> {error, protocol_version_mismatch}
    end;
protocol(Header, PrevKeyHeader) ->
    protocol(Header, aec_headers:version(PrevKeyHeader)).

pow(Header) ->
    Nonce = aec_headers:nonce(Header),
    Evd = aec_headers:pow(Header),
    Target = aec_headers:target(Header),
    pow(Header, Nonce, Evd, Target).

pow(Header, Nonce, Evd, Target) when Nonce >= ?MIN_NONCE, Nonce =< ?MAX_NONCE ->
    %% Zero nonce and pow_evidence before hashing, as this is how the mined
    %% block got hashed.
    Header1 = aec_headers:set_nonce_and_pow(Header, 0, no_value),
    HeaderBinary = aec_headers:serialize_to_binary(Header1),
    case aec_mining:verify(HeaderBinary, Nonce, Evd, Target) of
        true  -> ok;
        false -> {error, incorrect_pow}
    end.

cycle_time(Header, PrevHeader) ->
    Time = aec_headers:time_in_msecs(Header),
    PrevTime = aec_headers:time_in_msecs(PrevHeader),
    MinTime =
        case aec_headers:type(PrevHeader) of
            micro -> PrevTime + aec_governance:micro_block_cycle();
            key   -> PrevTime + 1
        end,
    case Time >= MinTime of
        true  -> ok;
        false -> {error, bad_micro_block_interval}
    end.

max_time(Header) ->
    Time = aec_headers:time_in_msecs(Header),
    MaxTime = aeu_time:now_in_msecs() + aec_governance:accepted_future_block_time_shift(),
    case Time < MaxTime of
        true  -> ok;
        false -> {error, block_from_the_future}
    end.

signature(Header, KeyHeader) ->
    aeu_sig:verify(Header, aec_headers:miner(KeyHeader)).

gossiped_height(Header, SyncHeight) when is_integer(SyncHeight) ->
    case (aec_headers:height(Header) - 1) =< SyncHeight of
        true  -> ok;
        false -> {error, invalid_gossiped_height}
    end;
gossiped_height(_Header, infinity) ->
    ok.

chain_connection(Header) ->
    PrevHeaderHash = aec_headers:prev_hash(Header),
    case aec_chain_state:hash_is_connected_to_genesis(PrevHeaderHash) of
        true  -> ok;
        false -> {error, orphan_blocks_not_allowed}
    end.

delta_height(Header, TopHeader) ->
    Height = aec_headers:height(Header),
    MaxDelta = aec_chain_state:gossip_allowed_height_from_top(),
    case Height >= aec_headers:height(TopHeader) - MaxDelta of
        true  -> ok;
        false -> {error, too_far_below_top}
    end.

prev_header(Header, PrevHeader) ->
    HeaderType = aec_headers:type(Header),
    PrevHeaderType = aec_headers:type(PrevHeader),
    case prev_height({HeaderType, aec_headers:height(Header)},
                     {PrevHeaderType, aec_headers:height(PrevHeader)}) of
        true ->
            case prev_key_hash({HeaderType, Header},
                               {PrevHeaderType, PrevHeader}) of
                true ->
                    case prev_hash(Header, PrevHeader) of
                        true  -> ok;
                        false -> {error, invalid_prev_hash}
                    end;
                false ->
                    {error, ivanlid_prev_key_hash}
            end;
        false ->
            {error, invalid_prev_height}
    end.

prev_height({micro, Height}, {micro, Height})   -> true;
prev_height({micro, Height}, {key, Height})     -> true;
prev_height({key, Height}, {micro, PrevHeight}) -> Height =:= (PrevHeight + 1);
prev_height({key, Height}, {key, PrevHeight})   -> Height =:= (PrevHeight + 1);
prev_height({_, _}, {_, _})                     -> false.

prev_key_hash({micro, Header}, {micro, PrevHeader}) ->
    aec_headers:prev_key_hash(Header) =:= aec_headers:prev_key_hash(PrevHeader);
prev_key_hash({micro, Header}, {key, _PrevHeader}) ->
    aec_headers:prev_key_hash(Header) =:= aec_headers:prev_hash(Header);
prev_key_hash({key, Header}, {micro, PrevHeader}) ->
    aec_headers:prev_key_hash(Header) =:= aec_headers:prev_key_hash(PrevHeader);
prev_key_hash({key, Header}, {key, PrevHeader}) ->
    aec_headers:prev_key_hash(Header) =:= header_hash(PrevHeader).

prev_hash(Header, PrevHeader) ->
    aec_headers:prev_hash(Header) =:= header_hash(PrevHeader).

txs_hash(Header, Txs) ->
    TxsHash = aec_headers:txs_hash(Header),
    TxsHash2 = aec_txs_trees:from_txs(Txs),
    case aec_txs_trees:pad_empty(aec_txs_trees:root_hash(TxsHash2)) of
        TxsHash -> ok;
        _Other  -> {error, malformed_txs_hash}
    end.

gas_limit(Header, Txs) ->
    case aec_blocks:gas(Header, Txs) =< aec_governance:block_gas_limit() of
        true  -> ok;
        false -> {error, gas_limit_exceeded}
    end.

txs_fee(Header, STxs) ->
    Protocol = aec_headers:version(Header),
    Height = aec_headers:height(Header),
    case lists:all(
           fun(STx) ->
                   Tx = aetx_sign:tx(STx),
                   aetx:fee(Tx) >= aetx:min_fee(Tx, Height, Protocol)
           end, STxs) of
        true  -> ok;
        false -> {error, invalid_minimal_tx_fee}
    end.

pof(_Header, no_fraud) ->
    ok;
pof(Header, PoF) ->
    case aec_headers:pof_hash(Header) =:= aec_pof:hash(PoF) of
        true  -> aec_pof:validate(PoF);
        false -> {error, pof_hash_mismatch}
    end.

%% Helper functions

header_hash(Header) ->
    {ok, HeaderHash} = aec_headers:hash_header(Header),
    HeaderHash.
