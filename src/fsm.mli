type id = int
type label = string
type _labels = label list

type state =
  { id : id
  ; name : label
  }

val default_name : id -> string
val state : ?name:label -> id -> state

type states = state list

val seq_from_states : states -> state Seq.t

type edge =
  { id : id
  ; lhs : id
  ; rhs : id
  ; label : label
  }

type has_edge =
  | ID of id
  | State of state

val edge : ?label:label -> id -> has_edge -> has_edge -> edge

type edges = edge list

type fsm =
  { init : id
  ; states : states
  ; edges : edges
  }

val fsm : ?init:id -> states -> edges -> fsm

type has_state_id =
  | States of states
  | State of state
  | Edges of edges
  | Edge of edge
  | Fsm of fsm

val has_state : id -> has_state_id -> bool

type has_state =
  | State of state
  | States of states
  | Fsm of fsm

val find_state : id -> has_state -> state option

type has_lts = Fsm of fsm

val get_edges : has_edge -> has_lts -> edges option
val default_indent_val : int
val tabs : ?size:int -> int -> string

type stringable =
  | ID of id
  | Label of label
  | State of state
  | States of states
  | Edge of edge
  | Edges of edges
  | Fsm of fsm

type stringable_context =
  | None
  | ShowIDs
  | States of states
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
val pp_tests : unit
