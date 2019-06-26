%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%% Entrypoint for stand-alone FATE Virtual Machine
%%% @end
%%%-------------------------------------------------------------------
-module(aefate).
-export([main/1]).

-include_lib("aecontract/include/hard_forks.hrl").
-include_lib("aeminer/include/aeminer.hrl").

-define(OPT_SPEC,
    [ {obj_file, undefined, undefined, string, "Fate bytecode file"}
    , {call, undefined, undefined, string, "Call to execute: function(arguments)"}
    , {verbose, $v, "verbose", undefined, "Show code and environment"}
    , {help, $h, "help", undefined, "Show this message"}
    ]).

%%%===================================================================
%%% API
%%%===================================================================

main(Args) ->
    io:format("Loaded mods: ~p\n\n", [lists:sort(code:all_loaded())]),
    io:format("Paths: ~p\n\n", [code:get_path()]),

    case getopt:parse(?OPT_SPEC, Args) of
        {ok, {Opts, []}} ->
            case proplists:get_value(help, Opts, false) of
                false ->
                    run(Opts);
                true ->
                    usage()
            end;

        {ok, {_, NonOpts}} ->
            io:format("Can't understand ~p\n\n", [NonOpts]),
            usage();

        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p\n\n", [Reason, Data]),
            usage()
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

usage() ->
    getopt:usage(?OPT_SPEC, "aefate").

run(Opts) ->
    case proplists:get_value(obj_file, Opts, undefined) of
        undefined ->
            io:format("Error: no input source file\n\n"),
            usage();
        File ->
            load_file(File, Opts)
    end.

load_file(FileName, Opts) ->
    Verbose = proplists:get_value(verbose, Opts, false),
    case proplists:get_value(call, Opts, undefined) of
        undefined ->
            io:format("Error: no call\n\n"),
            usage();
        Call ->
            {ok, File} = file:read_file(FileName),
            Code = aeb_fate_code:deserialize(File),
            SerializedCall = aeb_fate_asm:function_call(Call),
            GasPrice = 1,
            Origin = <<1:256>>,
            Caller = <<1:256>>,
            Trees = aec_trees:new_without_backend(),

            TxEnv = tx_env(),

            What = #{ contract => <<0:256>>
                    , call => SerializedCall
                    , gas => 10000000000
                    , value => 0
                    , store => <<>>
                    , code => File},
            Env = #{ gas_price => GasPrice
                   , origin => Origin
                   , caller => Caller
                   , trees  => Trees
                   , tx_env => TxEnv
                   },

            case aefa_fate:run(What, Env) of
                {ok, NewEnv} ->
                    print_after_run(Verbose, Code, Env, NewEnv),
                    io:format("~0p~n", [aefa_engine_state:accumulator(Env)]);
                {error, Error, NewEnv} ->
                    print_after_run(Verbose, Code, Env, NewEnv),
                    io:format("~p~n", [Error])
            end
    end.

print_after_run(true, Code,_PreEnv, Env) ->
    io:format("Code: ~0p~n", [Code]),
    %% io:format("PreEnv: ~0p~n", [PreEnv]),
    io:format("Env: ~0p~n", [Env]).

tx_env() ->
    Height = 1,
    RootHash    =  <<0:256>>,
    PrevHash    =  <<1:256>>,
    PrevKeyHash =  <<2:256>>,
    KeyHash     =  <<3:256>>,
    Miner = 0,
    Beneficiary =  aec_block_genesis:beneficiary(),
    Target = aeminer_pow:integer_to_scientific(1),
    Evd = 1,
    Nonce = 1,
    Time = aeu_time:now_in_msecs(),
    Version =  ?LIMA_PROTOCOL_VSN,

    aetx_env:tx_env_from_key_header(
      aec_headers:new_key_header(Height, PrevHash, PrevKeyHash, RootHash, Miner,
                                 Beneficiary, Target, Evd, Nonce, Time, Version),
      KeyHash, Time, PrevHash).
