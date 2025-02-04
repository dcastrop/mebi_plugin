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

module Block = States

module Partition : sig
  type elt = Block.t
  type t = Set.Make(Block).t

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

val add_new_outgoing_edge
  :  States.t Actions.t Edges.t
  -> state
  -> action
  -> state
  -> unit

type fsm =
  { init : state
  ; alphabet : Alphabet.t
  ; states : Block.t
  ; edges : Block.t Actions.t Edges.t
  }

val make_fsm
  :  state
  -> Alphabet.t
  -> States.t
  -> States.t Actions.t Edges.t
  -> fsm

val make_fsm_from_lts
  :  string
  -> (string * (string * string list) list) list
  -> fsm

module PStr : sig
  type formatting_params =
    { tabs : int
    ; no_leading_tab : bool
    ; params : Utils.logging_params
    }

  val default_formatting_params
    :  ?params:Utils.logging_params
    -> unit
    -> formatting_params

  val inc_tab : ?by:int -> formatting_params -> formatting_params
  val dec_tab : ?by:int -> formatting_params -> formatting_params
  val no_tab : formatting_params -> formatting_params
  val no_leading_tab : bool -> formatting_params -> formatting_params

  type pstr_params =
    | Logging of Utils.logging_params
    | Formatting of formatting_params

  val handle_formatting_params : pstr_params -> formatting_params
  val state : ?params:pstr_params -> state -> string
  val states : ?params:pstr_params -> Block.t -> string
  val partition : ?params:pstr_params -> Partition.t -> string
  val action : ?params:pstr_params -> action -> string
  val alphabet : ?params:pstr_params -> Alphabet.t -> string
  val edge : ?params:pstr_params -> state * action * state -> string

  val actions
    :  ?params:pstr_params
    -> ?from:state
    -> Block.t Actions.t
    -> string

  val edges : ?params:pstr_params -> Block.t Actions.t Edges.t -> string
  val fsm : ?params:pstr_params -> fsm -> string
end

exception StateNotFoundWithID of (int * States.t)
exception MultipleStatesFoundWithID of (int * States.t)

val get_state_by_id : States.t -> int -> state

exception StateNotFoundWithName of (string * States.t)
exception MultipleStatesFoundWithName of (string * States.t)

val _get_state_by_name : States.t -> string -> state
val _get_action_alphabet_from_actions : States.t Actions.t -> Alphabet.t
val _get_action_alphabet_from_edges : States.t Actions.t Edges.t -> Alphabet.t

exception ActionNotFoundWithID of (int * Alphabet.t)
exception MultipleActionsFoundWithID of (int * Alphabet.t)

val get_action_by_id : Alphabet.t -> int -> action

exception ActionNotFoundWithLabel of (string * Alphabet.t)
exception MultipleActionsFoundWithLabel of (string * Alphabet.t)

val get_action_by_label : Alphabet.t -> string -> action

val get_edges_of
  :  action
  -> States.t Actions.t Edges.t
  -> States.t Actions.t Edges.t

val get_actions_from : state -> States.t Actions.t Edges.t -> States.t Actions.t

type has_destinations =
  | Actions of Block.t Actions.t
  | Edges of Block.t Actions.t Edges.t

val get_all_destinations : has_destinations -> States.t

exception ReverseStateHashtblLookupFailed of ((state, state) Hashtbl.t * state)

val get_reverse_map_state : (state, state) Hashtbl.t -> state -> state

exception StateNotFoundInMergedStates of (state * States.t)

val map_merge_states
  :  States.t
  -> States.t
  -> States.t * (state, state) Hashtbl.t * (state, state) Hashtbl.t

exception ActionNotFoundInMergedAlphabet of (action * Alphabet.t)

val map_merge_alphabet
  :  Alphabet.t
  -> Alphabet.t
  -> Alphabet.t * (action, action) Hashtbl.t

exception StateNotFoundInMapOfStates of (state * (state, state) Hashtbl.t)
exception ActionNotFoundInMapOfAlphabet of (action * (action, action) Hashtbl.t)

val map_merge_edges
  :  States.t Actions.t Edges.t
  -> States.t Actions.t Edges.t
  -> (action, action) Hashtbl.t
  -> (state, state) Hashtbl.t
  -> States.t Actions.t Edges.t

val merge_fsm : fsm -> fsm -> fsm * (state, state) Hashtbl.t
