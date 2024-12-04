type state =
  { id : int
  ; pp : string
  }

type label = int

type ('a, 'b) transition =
  { label : 'a
  ; to_state : 'b
  }

type outgoing_edge = (label, state) transition
type edges = (state, outgoing_edge) Hashtbl.t

type fsm =
  { init : state
  ; edges : (state, (label, state) transition) Hashtbl.t
  }
