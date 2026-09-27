module type S = sig
  type state
  type states
  type labels
  type transitions
  type info

  type t =
    { init : state option
    ; alphabet : labels
    ; states : states
    ; transitions : transitions
    ; terminals : states
    ; info : info
    }

  include Json.S with type k = t
end

module Make (C : Components.S) :
  S
  with type state = C.State.t
   and type states = C.States.t
   and type labels = C.Labels.t
   and type transitions = C.Transitions.t
   and type info = C.Info.t = struct
  module State = C.State
  module States = C.States
  module Labels = C.Labels
  module Transitions = C.Transitions
  module Info = C.Info

  type state = State.t
  type states = States.t
  type labels = Labels.t
  type transitions = Transitions.t
  type info = Info.t

  type t =
    { init : state option
    ; alphabet : labels
    ; states : states
    ; transitions : transitions
    ; terminals : states
    ; info : info
    }

  include Json.Thing.Make (struct
      type k = t

      let name = "LTS"

      let json ?as_elt (x : t) : Yojson.t =
        `Assoc
          [ "init", Json.option ~as_elt:true State.json x.init
          ; "info", Info.json ~as_elt:true x.info
          ; "terminals", States.json ~as_elt:true x.terminals
          ; "alphabet", Labels.json ~as_elt:true x.alphabet
          ; "states", States.json ~as_elt:true x.states
          ; "transitions", Transitions.json ~as_elt:true x.transitions
          ]
      ;;
    end)
end
