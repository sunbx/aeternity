%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    ADT for commitment objects
%%% @end
%%%=============================================================================

-module(aens_commitments).

%% API
-export([new/4,
         update/6,
         serialize/1,
         deserialize/2,
         deserialize_from_fields/3,
         serialization_type/0,
         serialization_template/1
        ]).

-export([is_auction_done_when_elapsed/1,
         get_name_auction_state/2,
         name_length/1]).

%% Getters
-export([hash/1,
         owner_pubkey/1,
         ttl/1,
         created/1,
         updated/1,
         auction/1,
         name_fee/1,
         second_bidder/1,
         second_price/1,
         name_hash/1]).

-behavior(aens_cache).
%%%===================================================================
%%% Types
%%%===================================================================
-define(PRECLAIM, preclaim).
-define(CLAIM_ATTEMPT, claim_attempt).

-type(auction_state() :: ?PRECLAIM | ?CLAIM_ATTEMPT).

-record(commitment,
        {id                :: aeser_id:id(),
         owner_id          :: aeser_id:id(),
         created           :: aec_blocks:height(),
         updated           :: aec_blocks:height(),
         auction           :: auction_state(),
         name_fee          :: non_neg_integer() | undefined,
         second_bidder     :: aeser_id:id()     | undefined,
         second_price      :: non_neg_integer() | undefined,
         name_hash         :: hash()            | undefined,
         ttl               :: aec_blocks:height()
         }).

-opaque commitment() :: #commitment{}.

-type id() :: aeser_id:id().
-type hash() :: aens_hash:commitment_hash().
-type pubkey() :: aec_keys:pubkey().
-type serialized() :: binary().

-export_type([id/0,
              commitment/0,
              serialized/0]).

-define(COMMITMENT_TYPE, name_commitment).
-define(COMMITMENT_OLD, 1).
-define(COMMITMENT_PRECLAIM, 2).
-define(COMMITMENT_CLAIM, 3).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(aeser_id:id(), aeser_id:id(), non_neg_integer(), aec_blocks:height()) -> commitment().
new(Id, OwnerId, DeltaTTL, BlockHeight) ->
    commitment = aeser_id:specialize_type(Id),
    account    = aeser_id:specialize_type(OwnerId),
    #commitment{id       = Id,
                owner_id = OwnerId,
                auction = ?PRECLAIM,
                created  = BlockHeight,
                ttl      = BlockHeight + DeltaTTL}.

-spec update(commitment(), aeser_id:id(), non_neg_integer(), aec_blocks:height(),
             non_neg_integer(), hash()) -> commitment().
update(Commitment, OwnerId, BidDelta, BlockHeight, NameFee, NameHash) ->
    account    = aeser_id:specialize_type(OwnerId),
    PrevNameFee = Commitment#commitment.name_fee,
    PrevOwnerId = Commitment#commitment.owner_id,
    Commitment#commitment{
      owner_id = OwnerId,
      updated  = BlockHeight,
      auction  = ?CLAIM_ATTEMPT,
      name_fee = NameFee,
      second_bidder = PrevOwnerId,
      second_price = PrevNameFee,
      name_hash = NameHash,
      ttl      = BlockHeight + BidDelta}.

-spec serialize(commitment()) -> serialized().
serialize(#commitment{owner_id = OwnerId,
    created = Created,
    auction = Auction,
    ttl = TTL}) ->
    aeser_chain_objects:serialize(
        ?COMMITMENT_TYPE,
        ?COMMITMENT_PRECLAIM,
        serialization_template(?COMMITMENT_PRECLAIM),
        [ {owner_id, OwnerId}
        , {created, Created}
        , {auction, atom_to_binary(Auction, utf8)}
        , {ttl, TTL}]);
serialize(#commitment{owner_id = OwnerId,
                      created = Created,
                      updated = Updated,
                      auction = Auction,
                      name_fee = NameFee,
                      second_bidder = SecondBidder,
                      second_price = SecondPrice,
                      name_hash = NameHash,
                      ttl = TTL}) ->
    aeser_chain_objects:serialize(
      ?COMMITMENT_TYPE,
      ?COMMITMENT_CLAIM,
      serialization_template(?COMMITMENT_CLAIM),
      [ {owner_id, OwnerId}
      , {created, Created}
      , {updated, Updated}
      , {auction, atom_to_binary(Auction, utf8)}
      , {name_fee, NameFee}
      , {second_bidder, SecondBidder}
      , {second_price, SecondPrice}
      , {name_hash, NameHash}
      , {ttl, TTL}]).

-spec deserialize(hash(), binary()) -> commitment().
deserialize(CommitmentHash, Bin) ->
    {Type, Vsn, _Rest} = aeser_chain_objects:deserialize_type_and_vsn(Bin),
    Fields = aeser_chain_objects:deserialize(Type, Vsn, serialization_template(Vsn), Bin),
    deserialize_from_fields(Vsn, CommitmentHash, Fields).

deserialize_from_fields(?COMMITMENT_PRECLAIM, CommitmentHash,
                        [ {owner_id, OwnerId}
                        , {created, Created}
                        , {auction, Auction}
                        , {ttl, TTL}]) ->
    #commitment{id       = aeser_id:create(commitment, CommitmentHash),
                owner_id = OwnerId,
                created  = Created,
                auction = binary_to_existing_atom(Auction, utf8),
                ttl      = TTL};
deserialize_from_fields(?COMMITMENT_CLAIM, CommitmentHash,
                        [ {owner_id, OwnerId}
                        , {created, Created}
                        , {updated, Updated}
                        , {auction, Auction}
                        , {name_fee, NameFee}
                        , {second_bidder, SecondBidder}
                        , {second_price, SecondPrice}
                        , {name_hash, NameHash}
                        , {ttl, TTL}]) ->
    #commitment{id       = aeser_id:create(commitment, CommitmentHash),
                owner_id = OwnerId,
                created  = Created,
                updated = Updated,
                auction = binary_to_existing_atom(Auction, utf8),
                name_fee = NameFee,
                second_bidder = SecondBidder,
                second_price = SecondPrice,
                name_hash = NameHash,
                ttl      = TTL}.

serialization_template(?COMMITMENT_PRECLAIM) ->
    [ {owner_id, id}
    , {created, int}
    , {auction, binary}
    , {ttl, int}
    ];
serialization_template(?COMMITMENT_CLAIM) ->
    [ {owner_id, id}
    , {created, int}
    , {updated, int}
    , {auction, binary}
    , {name_fee, int}
    , {second_bidder, id}
    , {second_price, int}
    , {name_hash, binary}
    , {ttl, int}
    ].

serialization_type() -> ?COMMITMENT_TYPE.
%%%===================================================================
%%% Getters
%%%===================================================================

-spec hash(commitment()) -> hash().
hash(#commitment{id = Id}) ->
    aeser_id:specialize(Id, commitment).

-spec owner_pubkey(commitment()) -> pubkey().
owner_pubkey(#commitment{owner_id = OwnerId}) ->
    aeser_id:specialize(OwnerId, account).

-spec ttl(commitment()) -> aec_blocks:height().
ttl(#commitment{ttl = TTL}) ->
    TTL.

-spec created(commitment()) -> aec_blocks:height().
created(#commitment{created = Created}) ->
    Created.

-spec updated(commitment()) -> aec_blocks:height().
updated(#commitment{updated = Updated}) ->
    Updated.

-spec auction(commitment()) -> auction_state().
auction(#commitment{auction = AuctionState}) ->
    AuctionState.

-spec second_price(commitment()) -> non_neg_integer().
second_price(#commitment{second_price = SecondPrice}) ->
    SecondPrice.

-spec second_bidder(commitment()) -> pubkey().
second_bidder(#commitment{owner_id = SecondBidder}) ->
    aeser_id:specialize(SecondBidder, account).

-spec name_hash(commitment()) -> hash().
name_hash(#commitment{name_hash = NameHash}) ->
    NameHash.

-spec name_fee(commitment()) -> non_neg_integer().
name_fee(#commitment{name_fee = NameFee}) ->
    NameFee.

%%%
%%% Auction handling
%%%

-spec is_auction_done_when_elapsed(commitment()) -> boolean().
is_auction_done_when_elapsed(#commitment{auction = ?PRECLAIM}) ->
    false;
is_auction_done_when_elapsed(#commitment{auction = ?CLAIM_ATTEMPT}) ->
    true.

-spec get_name_auction_state(commitment(), binary()) ->
                             no_auction | auction_opening | auction_ongoing.
get_name_auction_state(#commitment{auction = Auction}, Name) ->
    Length = name_length(Name),
    case {Auction, Length} of
        {?PRECLAIM, false} ->
            no_auction;
        {?PRECLAIM, true} ->
            auction_opening;
        {?CLAIM_ATTEMPT, _} ->
            auction_ongoing
    end.


-spec name_length(binary()) -> non_neg_integer().
name_length(PlainName) ->
    %% Works only for one namespace; improve when we have more
    size(PlainName) - size(hd(aec_governance:name_registrars())).
