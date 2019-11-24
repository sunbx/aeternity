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

-export([gossiped_height/2,
         block_presence/1,
         pow/1,
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
         txs_fee/2,
         txs_signatures/2
        ]).

-export_type([block/0]).

-include_lib("aeminer/include/aeminer.hrl").

-define(PEER_CONN, aec_peer_connection).
-define(SYNC, aec_sync).
-define(HTTP, aehttp_dispatch_int).
-define(CONDUCTOR, aec_conductor).

-define(KEY_HEADER_SYNC_CHECKS,
        [{block_presence,   [header],              [?PEER_CONN]},
         {pow,              [header],              [?PEER_CONN]},
         {max_time,         [header],              [?PEER_CONN]},
         {prev_header,      [header, prev_header], [?PEER_CONN, ?SYNC]},
         {protocol,         [header, protocol],    [?SYNC]},
         {chain_connection, [header],              [?SYNC]}
%%         {block_presence},  [header],              [?CONDUCTOR]}
        ]).

-define(KEY_HEADER_GOSSIP_CHECKS,
        [{gossiped_height,  [header, sync_height], [?PEER_CONN]},
         {delta_height,     [header, top_header],  [?PEER_CONN]},
         {pow,              [header],              [?PEER_CONN]},
         {block_presence,   [header],              [?PEER_CONN]},
         {chain_connection, [header],              [?PEER_CONN]},
         {prev_header,      [header, prev_header], [?PEER_CONN]},
         {max_time,         [header],              [?PEER_CONN]},
         {protocol,         [header, protocol],    [?SYNC]}
%%         {block_presence},  [header],              [?CONDUCTOR]}
        ]).

-define(KEY_HEADER_HTTP_CHECKS,
        [{delta_height,     [header, top_header],  [?HTTP]},
         {pow,              [header],              [?HTTP]},
         {block_presence,   [header],              [?HTTP]},
         {chain_connection, [header],              [?HTTP]},
         {prev_header,      [header, prev_header], [?HTTP]},
         {max_time,         [header],              [?HTTP]},
         {protocol,         [header, protocol],    [?HTTP]}
%%         {block_presence},  [header],              [?CONDUCTOR]}
       ]).

-define(MICRO_HEADER_SYNC_CHECKS,
        [{block_presence,   [header],                  [?PEER_CONN]},
         {max_time,         [header],                  [?PEER_CONN]},
         {prev_header,      [header, prev_header],     [?PEER_CONN, ?SYNC]},
         {protocol,         [header, prev_key_header], [?PEER_CONN, ?SYNC]},
         {cycle_time,       [header, prev_header],     [?PEER_CONN, ?SYNC]},
         {signature,        [header, prev_key_header], [?PEER_CONN, ?SYNC]},
         {pof,              [header, pof],             [?PEER_CONN]},
         {txs_hash,         [header, txs],             [?PEER_CONN]},
         {gas_limit,        [header, txs],             [?PEER_CONN]},
         {txs_fee,          [header, txs],             [?PEER_CONN]},
         {chain_connection, [header],                  [?SYNC]}
        ]).

-define(MICRO_HEADER_GOSSIP_CHECKS,
        [{gossiped_height,  [header, sync_height],     [?PEER_CONN]},
         {block_presence,   [header],                  [?PEER_CONN]},
         {chain_connection, [header],                  [?PEER_CONN]},
         {delta_height,     [header, top_header],      [?PEER_CONN]},
         {prev_header,      [header, prev_header],     [?PEER_CONN]},
         {protocol,         [header, prev_key_header], [?PEER_CONN]},
         {cycle_time,       [header, prev_header],     [?PEER_CONN]},
         {max_time,         [header],                  [?PEER_CONN]},
         {signature,        [header, prev_key_header], [?PEER_CONN]},
         {txs_hash,         [header, txs],             [?PEER_CONN]}
        ]).

-define(TXS_CHECKS,
        [{txs_signatures, [header, txs], [?PEER_CONN]}
        ]).

-define(IS_DB_ENV(K),
        K =:= prev_header; K =:= prev_key_header; K =:= top_header).

-record(valid_block,
        {block,
         checks,
         origin
        }).

-opaque block() :: #valid_block{}.

%% API

new(Block, Origin) ->
    #valid_block{block  = Block,
                 checks = block_checks(aec_blocks:type(Block), Origin),
                 origin = Origin}.

check(#valid_block{block = Block, checks = Checks} = VBlock, Proc, Env) ->
    #{header := HeaderChecks} = Checks,
    Env2 = header_env(Block, Env),
    case perform_checks(HeaderChecks, Proc, Env2) of
        {ok, PendingHeaderChecks, DoneHeaderChecks} ->
            Checks2 =
                Checks#{header => #{pending => PendingHeaderChecks,
                                    done    => DoneHeaderChecks}},
            case type(Block) of
                key ->
                    {ok, VBlock#valid_block{checks = Checks2}};
                micro ->
                    #{txs := TxsChecks} = Checks,
                    case perform_checks(TxsChecks, Proc, Env2) of
                        {ok, PendingTxsChecks, DoneTxsChecks} ->
                            Checks3 =
                                Checks2#{txs => #{pending => PendingTxsChecks,
                                                  done    => DoneTxsChecks}},
                                {ok, VBlock#valid_block{checks = Checks3}};
                        {error, _Rsn} = Err ->
                            Err
                    end
            end;
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

header_env(Block, Env) ->
    header_env(aec_blocks:type(Block), Block, Env).

header_env(key, Block, Env) ->
    Env#{header => aec_blocks:to_key_header(Block)};
header_env(micro, Block, Env) ->
    Env#{header => aec_blocks:to_micro_header(Block),
         txs    => aec_blocks:txs(Block),
         pof    => aec_blocks:pof(Block)}.

block_checks(key, sync) ->
    #{header => #{pending => ?KEY_HEADER_SYNC_CHECKS, done => []}};
block_checks(key, gossip) ->
    #{header => #{pending => ?KEY_HEADER_GOSSIP_CHECKS, done => []}};
block_checks(key, http) ->
    #{header => #{pending => ?KEY_HEADER_HTTP_CHECKS, done => []}};
block_checks(micro, sync) ->
    #{header => #{pending => ?MICRO_HEADER_SYNC_CHECKS, done => []},
      txs    => #{pending => ?TXS_CHECKS, done => []}};
block_checks(micro, gossip) ->
    #{header => #{pending => ?MICRO_HEADER_GOSSIP_CHECKS, done => []},
      txs    => #{pending => ?TXS_CHECKS, done => []}}.

perform_checks(#{pending := PendingChecks}, Proc, Env) ->
    perform_checks(PendingChecks, Proc, Env, [], []).

perform_checks([Check | Rest], Proc, Env, PendingAcc, DoneAcc) ->
    case perform_check(Check, Proc, Env) of
        {done, ok, Env2} ->
            DCheck = done_check(Check),
            perform_checks(Rest, Proc, Env2, PendingAcc, [DCheck | DoneAcc]);
        {done, {error, _Rsn} = Err, _Env2} ->
            Err;
        {skip, keep_proc} ->
            perform_checks(Rest, Proc, Env, [Check | PendingAcc], DoneAcc);
        {skip, next_proc} ->
            PCheck = pending_check(Check),
            perform_checks(Rest, Proc, Env, [PCheck | PendingAcc], DoneAcc)
    end;
perform_checks([], _Proc, _Env, PendingAcc, DoneAcc) ->
    {ok, lists:reverse(PendingAcc), lists:reverse(DoneAcc)}.

perform_check({Fun, EnvKeys, [Proc | _Rest] = Procs}, Proc, Env) ->
    perform_check(Fun, EnvKeys, Procs, Env);
perform_check(_Check, _Proc, _Env) ->
    {skip, keep_proc}.

perform_check(Fun, EnvKeys, Procs, Env) ->
    case get_required_args(EnvKeys, Env, []) of
        {ok, Args} when is_list(Args) ->
            {done, apply(?MODULE, Fun, Args), Env};
        {ok, defer_check} ->
            {skip, keep_proc};
        {missing, Key} when ?IS_DB_ENV(Key) ->
            case read_db_env(Key, maps:get(header, Env)) of
                {ok, Val} ->
                    perform_check(Fun, EnvKeys, Procs, Env#{Key => Val});
                {error, not_found} ->
                    case is_last_proc(Procs) of
                        true  -> {error, orphan_block};
                        false -> {skip, next_proc}
                    end
            end
    end.

done_check({Fun, EnvKeys, [Proc | _Rest]}) ->
    {Fun, EnvKeys, Proc}.

pending_check({Fun, EnvKeys, [_Proc | Rest]}) ->
    {Fun, EnvKeys, Rest}.

get_required_args([Key | Rest], Env, Acc) ->
    case maps:get(Key, Env, undefined) of
        [] when Key =:= txs ->
            {ok, defer_check};
        Val when Val =/= undefined ->
            get_required_args(Rest, Env, [Val | Acc]);
        undefined ->
            {missing, Key}
    end;
get_required_args([], _Args, Acc) ->
    {ok, lists:reverse(Acc)}.

read_db_env(top_header, _Header) ->
    %% Always returns header.
    {ok, aec_chain:top_header()};
read_db_env(prev_header, Header) ->
    read_db_env_from_hash(prev_hash, Header);
read_db_env(prev_key_header, Header) ->
    read_db_env_from_hash(prev_key_hash, Header).

read_db_env_from_hash(HeadersFun, Header) ->
    case aec_chain:get_header(aec_headers:HeadersFun(Header)) of
        {ok, Header2} -> {ok, Header2};
        error         -> {error, not_found}
    end.

is_last_proc([_Proc | []]) -> true;
is_last_proc(_Procs)       -> false.

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

txs_signatures(Header, Txs) ->
    Protocol = aec_headers:version(Header),
    %% The Trees are created to make dialyzer happy.
    Trees = aec_trees:new_without_backend(),
    txs_signatures(Txs, Trees, Protocol).

txs_signatures([Tx | Rest], Trees, Protocol) ->
    case aetx:signers_location(Tx) of
        tx ->
            case aetx_sign:verify(Tx, Trees, Protocol) of
                ok ->
                    txs_signatures(Rest, Trees, Protocol);
                {error, _Rsn} = Err ->
                    Err
            end;
        trees ->
            ok
    end;
txs_signatures([], _Trees, _Protocol) ->
    ok.

%% Helper functions

header_hash(Header) ->
    {ok, HeaderHash} = aec_headers:hash_header(Header),
    HeaderHash.
