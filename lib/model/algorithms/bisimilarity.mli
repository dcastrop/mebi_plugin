(** {i See {!Model.S.Bisimilarity}.} *)
module type S = sig
  type states
  type partition
  type fsm

  module FSMPair : sig
    type t =
      { original : fsm
      ; saturated : fsm
      }

    include Json.S with type k = t (** @closed *)

    val get : fsm -> t
  end

  module Result : sig
    type t =
      { bisim_states : partition
      ; non_bisim_states : partition
      }

    include Json.S with type k = t (** @closed *)

    val are_bisimilar : t -> bool
    val split : partition -> states -> states -> t
  end

  type t =
    { fsm_a : FSMPair.t
    ; fsm_b : FSMPair.t
    ; merged : fsm
    ; result : Result.t
    }

  include Json.S with type k = t (** @closed *)

  val the_cached_result : t option ref
  val set_the_result : t -> unit

  exception NoCachedResult of unit

  val get_the_result : unit -> t
  val fsm : fsm -> fsm -> t
end

module Make
    (C : Components.S)
    (FSM :
       FSM.S
       with type state = C.State.t
        and type states = C.States.t
        and type labels = C.Labels.t
        and type edgemap = C.EdgeMap.t'
        and type info = C.Info.t)
    (Minimization :
       Minimization.S
       with type state = C.State.t
        and type states = C.States.t
        and type label = C.Label.t
        and type labels = C.Labels.t
        and type edgemap = C.EdgeMap.t'
        and type partition = C.Partition.t
        and type fsm = FSM.t) :
  S
  with type states = C.States.t
   and type partition = C.Partition.t
   and type fsm = FSM.t
