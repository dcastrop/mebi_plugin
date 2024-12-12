type state =
  { id : int
  ; pp : string
  }

val make_state : ?pp:string -> int -> state

module States : sig
  type elt = state
  type t

  val empty : t
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val disjoint : t -> t -> bool
  val diff : t -> t -> t
  val cardinal : t -> int
  val elements : t -> elt list
  val min_elt : t -> elt
  val min_elt_opt : t -> elt option
  val max_elt : t -> elt
  val max_elt_opt : t -> elt option
  val choose : t -> elt
  val choose_opt : t -> elt option
  val find : elt -> t -> elt
  val find_opt : elt -> t -> elt option
  val find_first : (elt -> bool) -> t -> elt
  val find_first_opt : (elt -> bool) -> t -> elt option
  val find_last : (elt -> bool) -> t -> elt
  val find_last_opt : (elt -> bool) -> t -> elt option
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val map : (elt -> elt) -> t -> t
  val filter : (elt -> bool) -> t -> t
  val filter_map : (elt -> elt option) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val split : elt -> t -> t * bool * t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val subset : t -> t -> bool
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val to_list : t -> elt list
  val of_list : elt list -> t
  val to_seq_from : elt -> t -> elt Seq.t
  val to_seq : t -> elt Seq.t
  val to_rev_seq : t -> elt Seq.t
  val add_seq : elt Seq.t -> t -> t
  val of_seq : elt Seq.t -> t
end

type action =
  { id : int
  ; label : string
  }

val make_action : ?label:string -> int -> action

module Alphabet : sig
  type elt = action
  type t

  val empty : t
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val disjoint : t -> t -> bool
  val diff : t -> t -> t
  val cardinal : t -> int
  val elements : t -> elt list
  val min_elt : t -> elt
  val min_elt_opt : t -> elt option
  val max_elt : t -> elt
  val max_elt_opt : t -> elt option
  val choose : t -> elt
  val choose_opt : t -> elt option
  val find : elt -> t -> elt
  val find_opt : elt -> t -> elt option
  val find_first : (elt -> bool) -> t -> elt
  val find_first_opt : (elt -> bool) -> t -> elt option
  val find_last : (elt -> bool) -> t -> elt
  val find_last_opt : (elt -> bool) -> t -> elt option
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val map : (elt -> elt) -> t -> t
  val filter : (elt -> bool) -> t -> t
  val filter_map : (elt -> elt option) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val split : elt -> t -> t * bool * t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val subset : t -> t -> bool
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val to_list : t -> elt list
  val of_list : elt list -> t
  val to_seq_from : elt -> t -> elt Seq.t
  val to_seq : t -> elt Seq.t
  val to_rev_seq : t -> elt Seq.t
  val add_seq : elt Seq.t -> t -> t
  val of_seq : elt Seq.t -> t
end

module Actions : sig
  type key = action
  type !'a t

  val create : int -> 'a t
  val clear : 'a t -> unit
  val reset : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val remove : 'a t -> key -> unit
  val find : 'a t -> key -> 'a
  val find_opt : 'a t -> key -> 'a option
  val find_all : 'a t -> key -> 'a list
  val replace : 'a t -> key -> 'a -> unit
  val mem : 'a t -> key -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
  val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
  val length : 'a t -> int
  val stats : 'a t -> Hashtbl.statistics
  val to_seq : 'a t -> (key * 'a) Seq.t
  val to_seq_keys : 'a t -> key Seq.t
  val to_seq_values : 'a t -> 'a Seq.t
  val add_seq : 'a t -> (key * 'a) Seq.t -> unit
  val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
  val of_seq : (key * 'a) Seq.t -> 'a t
end

val make_actions : ?size:int -> unit -> States.t Actions.t

module Edges : sig
  type key = state
  type !'a t

  val create : int -> 'a t
  val clear : 'a t -> unit
  val reset : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val remove : 'a t -> key -> unit
  val find : 'a t -> key -> 'a
  val find_opt : 'a t -> key -> 'a option
  val find_all : 'a t -> key -> 'a list
  val replace : 'a t -> key -> 'a -> unit
  val mem : 'a t -> key -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
  val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
  val length : 'a t -> int
  val stats : 'a t -> Hashtbl.statistics
  val to_seq : 'a t -> (key * 'a) Seq.t
  val to_seq_keys : 'a t -> key Seq.t
  val to_seq_values : 'a t -> 'a Seq.t
  val add_seq : 'a t -> (key * 'a) Seq.t -> unit
  val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
  val of_seq : (key * 'a) Seq.t -> 'a t
end

val make_edges : ?size:int -> unit -> States.t Actions.t Edges.t

type fsm =
  { init : state
  ; alphabet : Alphabet.t
  ; states : States.t
  ; edges : States.t Actions.t Edges.t
  }

val make_fsm
  :  state
  -> Alphabet.t
  -> States.t
  -> States.t Actions.t Edges.t
  -> fsm

val pstr_state : ?ids:unit -> ?pp:unit -> ?long:unit -> state -> string

val handle_state_pstr
  :  unit option
  -> unit option
  -> unit option
  -> state
  -> string

val pstr_states
  :  ?ids:unit
  -> ?pp:unit
  -> ?long:unit
  -> ?indent:int
  -> States.t
  -> string

val pstr_action : ?ids:unit -> ?long:unit -> action -> string

val handle_action_pstr
  :  unit option
  -> unit option
  -> unit option
  -> action
  -> string

val pstr_action_alphabet
  :  ?ids:unit
  -> ?long:unit
  -> ?indent:int
  -> Alphabet.t
  -> string

val pstr_edge
  :  ?ids:unit
  -> ?pp:unit
  -> ?long:unit
  -> state * action * state
  -> string

val handle_edge_pstr
  :  unit option
  -> unit option
  -> unit option
  -> state * action * state
  -> string

val pstr_actions
  :  ?ids:unit
  -> ?pp:unit
  -> ?long:unit
  -> ?indent:int
  -> ?from_state:state
  -> States.t Actions.t
  -> string

val handle_actions_pstr
  :  ?indent:int
  -> ?from_state:state
  -> unit option
  -> unit option
  -> unit option
  -> States.t Actions.t
  -> string

val pstr_edges
  :  ?ids:unit
  -> ?pp:unit
  -> ?long:unit
  -> ?indent:int
  -> States.t Actions.t Edges.t
  -> string

val handle_states_pstr
  :  ?indent:int
  -> unit option
  -> unit option
  -> unit option
  -> States.t
  -> string

val handle_action_alphabet_pstr
  :  unit option
  -> unit option
  -> unit option
  -> Alphabet.t
  -> string

val handle_edges_pstr
  :  unit option
  -> unit option
  -> unit option
  -> States.t Actions.t Edges.t
  -> string

val pstr_fsm : ?ids:unit -> ?pp:unit -> ?long:unit -> fsm -> string

exception StateNotFoundWithID of (int * States.t)
exception MultipleStatesFoundWithID of (int * States.t)

val get_state_by_id : States.t -> int -> state

exception StateNotFoundWithName of (string * States.t)
exception MultipleStatesFoundWithName of (string * States.t)

val get_state_by_name : States.t -> string -> state
val get_action_alphabet_from_actions : States.t Actions.t -> Alphabet.t
val get_action_alphabet_from_edges : States.t Actions.t Edges.t -> Alphabet.t

exception ActionNotFoundWithID of (int * Alphabet.t)
exception MultipleActionsFoundWithID of (int * Alphabet.t)

val get_action_by_id : Alphabet.t -> int -> action

exception ActionNotFoundWithLabel of (string * Alphabet.t)
exception MultipleActionsFoundWithLabel of (string * Alphabet.t)

val get_action_by_label : Alphabet.t -> string -> action

val get_outgoing_actions
  :  States.t Actions.t Edges.t
  -> state
  -> action
  -> States.t Actions.t

val get_outgoing_actions_by_id
  :  States.t Actions.t Edges.t
  -> state
  -> int
  -> States.t Actions.t

val get_outgoing_actions_by_label
  :  States.t Actions.t Edges.t
  -> state
  -> string
  -> States.t Actions.t

exception ReverseStateHashtblLookupFailed of ((state, state) Hashtbl.t * state)

val get_reverse_map_state : (state, state) Hashtbl.t -> state -> state
