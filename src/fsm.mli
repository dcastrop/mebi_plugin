type state =
  { id : int
  ; name : string
  }

val state : ?name:string -> int -> state

type edge =
  { id : int
  ; lhs : int
  ; rhs : int
  ; label : string
  }

type has_edge =
  | ID of int
  | State of state

val edge : ?label:string -> int -> has_edge -> has_edge -> edge

type fsm =
  { init : int
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
  val default_indent_val : int
  val tabs : ?size:int -> int -> string

  type stringable =
    | ID of int
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
    -> ?indents:int
    -> ?prefix:string
    -> stringable
    -> string

  val pp : string -> unit
end
