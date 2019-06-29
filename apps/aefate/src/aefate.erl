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

-define(OPTIONALLY(__Flag__, __Opts__, __What__),
        case proplists:get_value(__Flag__, (__Opts__), false) of
            true -> (__What__);
            false -> ok
        end).

-ifdef(TEST).
-define(TRACE_SPEC, ,{show_trace, $s, "showtrace", undefined, "Show the execution trace."}
                    ,{gather_trace, $g, "gathertrace", undefined, "Gather the execution trace and print at end."}).
-define(PRINTTRACE(__T__, __Env__),
        if (__T__) ->
                io:format("~0p~n", [aefa_engine_state:trace(__Env__)]);
           true -> ok
        end).
-else.
-define(TRACE_SPEC, ).
-define(PRINTTRACE(__T__,__Env__), __T__).
-endif.


-define(TIME(__O__, __T__, __F__),
        case proplists:get_value(time, (__O__), false) of
            true ->
                {__T__, __R__} = timer:tc(fun () -> (__F__) end),
                __R__;
            false -> __T__ = 0, (__F__)
        end).


-define(OPT_SPEC,
    [ {obj_file, undefined, undefined, string, "Fate bytecode file"}
    , {call, undefined, undefined, string, "Call to execute: function(arguments)"}
    , {time, $t, "time", undefined, "Time the execution."}
      ?TRACE_SPEC
    , {verbose, $v, "verbose", undefined, "Show code and environment"}
    , {help, $h, "help", undefined, "Show this message"}
    ]).


%%%===================================================================
%%% API
%%%===================================================================

main(Args) ->
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

has_option(O, Os) -> proplists:get_bool(O, Os).

trace_io(I, Env) ->
    {ResType, Args} = aefa_fate_op:get_args(I, Env),
    io:format("~p:~p where args = <~s>~n",
              [aeb_fate_pp:format_op(I, #{}),
               ResType,
               lists:join(", ",[aeb_fate_data:format(V) || V <- Args])
              ]).

load_file(FileName, Opts) ->
    Verbose = has_option(verbose, Opts),
    PrintTrace = has_option(gather_trace, Opts),
    TraceIo = case has_option(show_trace, Opts) of
                  false -> fun(_,_) -> ok end;
                  true -> fun(I, Env) -> trace_io(I, Env) end
              end,

    case proplists:get_value(call, Opts) of
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

            Spec = #{ contract => <<0:256>>
                    , call => SerializedCall
                    , gas => 10000000000
                    , value => 0
                    , store => <<>>
                    , code => File
                    , trace_io => TraceIo
                    },

            Env = #{ gas_price => GasPrice
                   , origin => Origin
                   , caller => Caller
                   , trees  => Trees
                   , tx_env => TxEnv
                   },
            io:format("Code: ~0p~n", [Code]),

            case ?TIME(Opts, ExecutionTime, aefa_fate:run(Spec, Env)) of
                {ok, NewEnv} ->
                    print_after_run(Verbose, Code, Env, NewEnv),
                    ?PRINTTRACE(PrintTrace, NewEnv),
                    ?OPTIONALLY(time, Opts, io:format("Execution Time: ~0p microseconds~n",
                                                      [ExecutionTime])),
                    io:format("~0p~n", [aefa_engine_state:accumulator(NewEnv)]),
                    ok;
                {error, Error, NewEnv} ->
                    print_after_run(Verbose, Code, Env, NewEnv),
                    io:format("~p~n", [Error])
            end
    end.


print_after_run(true, Code,_PreEnv,_Env) ->
    io:format("Code: ~0p~n", [Code]),
    %% io:format("PreEnv: ~0p~n", [PreEnv]),
    %% io:format("Env: ~0p~n", [Env]),
    ok;
print_after_run(false,_Code,_PreEnv,_Env) ->
    ok.

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
