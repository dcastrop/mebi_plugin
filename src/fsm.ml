(** [state] is a 2-tuple with a unique [id] and (non-unique) name (or [label]).
    [id] is an integer for identifying the state.
    [pp] is a pretty-printed string of something corresponding to this state. *)
type state =
  { id : int (* ; hash : int *)
  ; pp : string
  }

(** [label] is an alias for [int], corresponding to the index of a Coq-based constructor. *)
type label = int

(** [('a, 'b) transition] is a 2-tuple with a [label] and [to_state].
    [label] is of type ['a].
    [to_state] is of type ['b]. *)
type ('a, 'b) transition =
  { label : 'a
  ; to_state : 'b
  }

(** [fsm_transition] is a type describing outgoing edges of an OCaml FSM.
    [Fsm.label] is a label (corresponding to the Coq-based LTS constructor number).
    [Fsm.state] is thr destination state. *)
type fsm_transition = (label, state) transition

(** [edges] is a hashtable mapping states to their outgoing-transitions. *)
type edges = (state, fsm_transition) Hashtbl.t

(** [fsm] is a type used to describe an FSM in OCaml.
    [init] is the initial state.
    [edges] is a hashtable mapping states to outgoing edges. *)
type fsm =
  { init : state (* ; states : *)
  ; edges : (state, fsm_transition) Hashtbl.t
  }
(* TODO: Currently, there may be many copies of the same state in an [fsm] (i.e., in [init] and the [edges]). Maybe add list of states and change others to be an index referencing their position in the list. *)
