type state =
  { id : int
  ; pp : string
  }

type label = int

type ('a, 'b) transition =
  { label : 'a
  ; to_state : 'b
  }

type fsm_transition = (label, state) transition
type edges = (state, fsm_transition) Hashtbl.t

type fsm =
  { init : state
  ; edges : (state, fsm_transition) Hashtbl.t
  }
