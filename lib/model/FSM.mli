(** {i See {!Model.S.FSM}.} *)
module type S = sig
  type state
  type states
  type labels
  type edgemap
  type info
  type lts

  type t =
    { init : state option
    ; terminals : states
    ; alphabet : labels
    ; states : states
    ; edges : edgemap
    ; info : info
    }

  include Json.S with type k = t

  val of_lts : lts -> t
  val merge : t -> t -> t
  val is_weak_mode : t -> bool
  val saturate : ?only_if_weak:bool -> t -> t
end

module Make
    (Log : Logger.S)
    (State : State.S)
    (States : States.S with type elt = State.t)
    (Labels : Labels.S)
    (EdgeMap : Edgemap.S with type state = State.t and type label = Labels.elt)
    (Info : Info.S with type base = State.base and type labels = Labels.t)
    (LTS :
       LTS.S
       with type state = State.t
        and type states = States.t
        and type labels = Labels.t
        and type transitions = EdgeMap.transitions
        and type info = Info.t)
    (Saturation :
       Saturation.S
       with type state = State.t
        and type states = States.t
        and type labels = Labels.t
        and type edgemap = EdgeMap.t') :
  S
  with type state = State.t
   and type states = States.t
   and type labels = Labels.t
   and type edgemap = EdgeMap.t'
   and type info = Info.t
   and type lts = LTS.t
