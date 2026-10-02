(** {i See {!Model.S.Bisimilarity}.} *)
module type S = sig
  type states
  type partition
  type fsm

  (** An FSM as given and as saturated ({!FSM.saturate}; the same FSM if it
      has no silent labels). *)
  module FSMPair : sig
    type t =
      { original : fsm
      ; saturated : fsm
      }

    include Json.S with type k = t (** @closed *)

    (** [get x] saturates [x], keeping the original alongside. *)
    val get : fsm -> t
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

    (** [are_bisimilar r]: the two initial states share a block, which is what
        bisimilarity of two systems means. Only when an FSM has no initial
        state does it fall back to "no block holds states from only one FSM",
        which is necessary but not sufficient: [a.b.x] and [b.a.y] pass it. *)
    val are_bisimilar : t -> bool

    (** [split ?roots_related pi a b] sorts the blocks of [pi] into those with
        states from both [a] and [b] ([bisim_states]) and the rest. *)
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

  (** The last result, kept for the proof solver ([MeBi Sim Begin] computes
      it, every later step reads it). *)
  val the_cached_result : t option ref

  val set_the_result : t -> unit

  exception NoCachedResult of unit

  (** @raise NoCachedResult if no check has run. *)
  val get_the_result : unit -> t

  (** [fsm a b] checks [a] and [b] for (weak) bisimilarity: saturates both,
      merges them ({!FSM.merge}; their states must be disjoint, as the
      plugin's encoding makes them), partitions the merged FSM by
      {!Minimization.partition_states}, and splits the result. *)
  val fsm : fsm -> fsm -> t
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
