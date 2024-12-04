(** [state] is a 2-tuple with a unique [id] and (non-unique) name (or [label]).
    [id] is an integer for identifying the state.
    [pp] is a pretty-printed string of something corresponding to this state. *)
type state =
  { id : int (* ; hash : int *)
  ; pp : string
  }

(** [label] is ... *)
type label = int

(** [('a, 'b) transition] is a 2-tuple with a [label] and [to_state].
    [label] is of type ['a].
    [to_state] is of type ['b]. *)
type ('a, 'b) transition =
  { label : 'a
  ; to_state : 'b
  }

type outgoing_edge = (label, state) transition
type edges = (state, outgoing_edge) Hashtbl.t

(** [fsm] *)
type fsm =
  { (* init : int *)
    init : state (* ; states : *)
  ; edges : (state, (label, state) transition) Hashtbl.t
  }
