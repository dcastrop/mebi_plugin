module type S = sig
  type states
  type partition
  type fsm

  module FSMPair : sig
    type t =
      { original : fsm
      ; saturated : fsm
      }

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
    val get : fsm -> t
  end

  type t =
    { fsm_a : FSMPair.t
    ; fsm_b : FSMPair.t
    ; merged : fsm
    ; result : result
    }

  and result =
    { bisim_states : partition
    ; non_bisim_states : partition
    }
    include Json.S with type k = t

  val are_bisimilar : result -> bool
  val the_cached_result : t option ref
  val set_the_result : t -> unit

  exception NoCachedResult of unit

  val get_the_result : unit -> t
  val split : partition -> states -> states -> result
  val fsm : fsm -> fsm -> t
end

module Make
    (Log : Logger.S)
    (State : State.S)
    (States : States.S with type elt = State.t)
    (Label : Label.S with type base = State.base)
    (Labels : Labels.S with type elt = Label.t)
    (Action : Action.S with type label = Label.t)
    (ActionMap :
       Actionmap.S with type action = Action.t and type states = States.t)
    (EdgeMap :
       Edgemap.S with type state = State.t and type actionmap = ActionMap.t')
    (Partition :
       State_partition.S with type elt = States.t and type edgemap = EdgeMap.t')
    (Info : Info.S with type base = State.base and type labels = Labels.t)
    (FSM :
       FSM.S
       with type state = State.t
        and type states = States.t
        and type labels = Labels.t
        and type edgemap = EdgeMap.t'
        and type info = Info.t)
    (Minimize :
       Minimize.S
       with type state = State.t
        and type states = States.t
        and type label = Label.t
        and type labels = Labels.t
        and type edgemap = EdgeMap.t'
        and type partition = Partition.t
        and type fsm = FSM.t) :
  S
  with type states = States.t
   and type partition = Partition.t
   and type fsm = FSM.t