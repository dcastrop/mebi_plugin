(** {i See {!Model.S.Bisimilarity}.} *)
module type S = sig
  type states
  type partition
  type fsm

  (** An FSM as given and as saturated ([FSM.S.saturate]; the same FSM if it
      has no silent labels). *)
  module FSMPair : sig
    type t =
      { original : fsm
      ; saturated : fsm
      }

    include Json.S with type k = t (** @closed *)

    (** [get ?on_demand x] is [x] with its saturation alongside: whole, or,
        with [on_demand], saturated on demand holding at most that many weak
        actions ([FSM.S.saturate_on_demand]).

        Raises nothing. *)
    val get : ?on_demand:int -> fsm -> t
  end

  (** The partition of the merged FSM's states, split by whether a block
      holds states from both FSMs. *)
  module Result : sig
    type t =
      { bisim_states : partition
      ; non_bisim_states : partition
      ; roots_related : bool option
        (** whether the two FSMs' initial states share a block; [None] when
            either FSM has no initial state *)
      }

    include Json.S with type k = t (** @closed *)

    (** [are_bisimilar r] is whether the two systems are bisimilar: their
        initial states share a block, which is what bisimilarity of two
        systems means. Only when an FSM has no initial state does it fall
        back to "no block holds states from only one FSM", which is
        necessary but not sufficient: [a.b.x] and [b.a.y] pass it.

        Raises nothing. *)
    val are_bisimilar : t -> bool

    (** [split ?roots_related pi a b] is [pi]'s blocks sorted into those with
        states from both [a] and [b] ([bisim_states]) and the rest, with
        [roots_related] recorded as given.

        Raises nothing. *)
    val split : ?roots_related:bool -> partition -> states -> states -> t
  end

  (** Everything a bisimilarity check produced; the proof solver reads it
      back via {!get_the_result}. *)
  type t =
    { fsm_a : FSMPair.t
    ; fsm_b : FSMPair.t
    ; merged : fsm
    ; result : Result.t
    }

  include Json.S with type k = t (** @closed *)

  (** The last result, kept for the proof solver: [MeBi Sim Begin]
      computes it, and every later step reads it. *)
  val the_cached_result : t option ref

  (** [set_the_result r] makes [r] {!the_cached_result}.

      Raises nothing. *)
  val set_the_result : t -> unit

  exception NoCachedResult of unit

  (** [get_the_result ()] is {!the_cached_result}.

      @raise NoCachedResult if no check has run (raised here). *)
  val get_the_result : unit -> t

  (** For FSMs too large to saturate whole (notes/13): which of the two to
      saturate on demand ([FSM.S.saturate_on_demand], at most [budget] weak
      actions held), and how to partition without saturating: on the
      silent-SCC quotient of the merged originals
      ({!Saturation_estimate.S.val-partition}). *)
  type on_demand =
    { a : bool
    ; b : bool
    ; budget : int
    ; partition : fsm -> partition
    }

  (** [fsm ?on_demand a b] is the result of checking [a] and [b] for (weak)
      bisimilarity: both saturated, merged ([FSM.S.merge]; their states must
      be disjoint, as the plugin's encoding makes them), the merge
      partitioned by [Minimization.S.partition_states], and the result split.

      With [on_demand] and either FSM on demand, that FSM is saturated on
      demand, [merged] is the merge of the originals, and the partition is
      [on_demand.partition merged]: the same partition, by a different route
      (checked in [tests.exe]).

      Raises nothing in practice; would propagate
      [Minimization.S.partition_states]'s exceptions. *)
  val fsm : ?on_demand:on_demand -> fsm -> fsm -> t

  (** [conflicts a b] is the set of states [a] and [b] share (the same
      term, hence the same encoding) whose moves differ between the two:
      labels or targets.

      {!val-fsm} merges [a] and [b] assuming a shared state is one state, which
      is exact when both sides use the same relation (so the same term has
      the same moves) and wrong otherwise: two relations over [nat], both
      from [0], conflate their [0]s. Empty in every checked-in example.
      Found 2026-10-03 (notes/13).

      Raises nothing. *)
  val conflicts : fsm -> fsm -> states
end

module Make
    (C : Components.S)
    (FSM :
       FSM.S
       with type state = C.State.t
        and type states = C.State.Set.t
        and type labels = C.Label.Set.t
        and type edgemap = C.EdgeMap.t'
        and type info = C.Info.t)
    (Minimization :
       Minimization.S
       with type state = C.State.t
        and type states = C.State.Set.t
        and type label = C.Label.t
        and type labels = C.Label.Set.t
        and type edgemap = C.EdgeMap.t'
        and type partition = C.Partition.t
        and type fsm = FSM.t) :
  S
  with type states = C.State.Set.t
   and type partition = C.Partition.t
   and type fsm = FSM.t
