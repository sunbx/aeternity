%%%=============================================================================
%%% @copyright 2019, Aeternity Anstalt
%%% @doc
%%%    Module wrapping the FSM ID - the ID is used as an encryption key for protecting the persisted
%%%    channel state. The ID is wrapped in a getter function - any attempt to print out
%%%    the ID will result in printing the atom representing the function.
%%% @end
%%%=============================================================================
-module(aesc_fsm_id).

-export([ new/0
        , from_binary/1
        , retrieve/1
        , retrieve_for_client/1
        , compare/2
        ]).

-opaque wrapper() :: fun(() -> binary()).
-export_type([wrapper/0]).

-define(FSM_ID_SIZE, 32).

-spec new() -> wrapper().
new() ->
    FsmID = crypto:strong_rand_bytes(?FSM_ID_SIZE),
    fun() -> FsmID end.

-spec from_binary(binary()) -> wrapper().
from_binary(FsmID) when size(FsmID) == ?FSM_ID_SIZE ->
    fun() -> FsmID end.

-spec retrieve(wrapper()) -> binary().
retrieve(FsmIDWrapper) -> FsmIDWrapper().

-spec retrieve_for_client(wrapper()) -> aeser_id:encoded().
retrieve_for_client(FsmIDWrapper) ->
    aeser_api_encoder:encode(bytearray, FsmIDWrapper()).

-spec compare(wrapper(), wrapper()) -> boolean().
%% @doc
%%      The ID is used as a cryptographic key. For reconnecting to the FSM the key is used for
%%      authenticating the user. The user should NEVER compare the ID's directly - only by using
%%      this constant time wrapper in order to avoid timing attacks.
%% @end
compare(FsmIDWrapper1, FsmIDWrapper2) ->
    FsmID1 = retrieve(FsmIDWrapper1),
    FsmId2 = retrieve(FsmIDWrapper2),
    enacl:verify_32(FsmID1, FsmId2).
