type state =
  { id : int
  ; pp : string
  }

type label = int

type ('a, 'b) transition =
  { label : 'a
  ; to_state : 'b
  }

type fsm_aux =
  { init : state
  ; edges : (state, (label, state) transition) Hashtbl.t
  }

type ed = (label, state) transition
type es = (state, ed) Hashtbl.t

type edge =
  { id : label
  ; lhs : label
  ; rhs : label
  ; label : string
  }

type has_edge =
  | ID of label
  | State of state

val edge : ?label:string -> int -> has_edge -> has_edge -> edge

type fsm =
  { init : label
  ; states : state list
  ; edges : edge list
  }

val fsm : ?init:int -> state list -> edge list -> fsm

type has_state_id =
  | States of state list
  | State of state
  | Edges of edge list
  | Edge of edge
  | Fsm of fsm

val has_state : int -> has_state_id -> bool

type has_state =
  | State of state
  | States of state list
  | Fsm of fsm

val find_state : int -> has_state -> state option

type has_lts = Fsm of fsm

val get_edges : has_edge -> has_lts -> edge list option

module Stringify : sig
  val default_indent_val : label
  val tabs : ?size:label -> label -> string

  type stringable =
    | ID of label
    | Label of string
    | State of state
    | States of state list
    | Edge of edge
    | Edges of edge list
    | Fsm of fsm

  type stringable_context =
    | None
    | ShowIDs
    | States of state list
    | List of stringable_context list

  val add_to_stringable_context
    :  stringable_context
    -> stringable_context
    -> stringable_context

  val to_string
    :  ?context:stringable_context
    -> ?indents:label
    -> ?prefix:string
    -> stringable
    -> string

  val pp : string -> unit
end
