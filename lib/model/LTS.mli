module type S = sig
  type state
  type states
  type labels
  type transitions
  type info

  type t =
    { init : state option
    ; terminals : states
    ; alphabet : labels 
    ; states : states
    ; transitions : transitions
    ; info : info
    }
end

module Make
    (Log : Logger.S)
    (State : State.S)
    (States : States.S with type elt = State.t)
    (Labels : Labels.S)
    (Transitions : Transitions.S with type labels = Labels.t )
    (Info : Info.S with type base = State.base and type labels = Labels.t) :
  S
  with type state = State.t
   and type states = States.t
   and type labels = Labels.t
   and type transitions = Transitions.t
   and type info = Info.t 