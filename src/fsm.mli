type state =
  { id : int
  ; name : string
  }

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
  ; is_tau : bool
  ; mutable annotation : annotations
  }

and annotation = (state * action) list
and annotations = annotation list

val tau : action

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

type fsm =
  { init : state option
  ; mutable alphabet : Alphabet.t
  ; mutable states : Block.t
  ; mutable edges : Block.t Actions.t Edges.t
  ; is_complete : bool
  }

module PStr : sig
  val state : ?params:Utils.Formatting.pstr_params -> state -> string
  val states : ?params:Utils.Formatting.pstr_params -> Block.t -> string
  val partition : ?params:Utils.Formatting.pstr_params -> Partition.t -> string
  val action : ?params:Utils.Formatting.pstr_params -> action -> string
  val alphabet : ?params:Utils.Formatting.pstr_params -> Alphabet.t -> string
  val annotation : ?params:Utils.Formatting.pstr_params -> annotation -> string

  val annotations
    :  ?params:Utils.Formatting.pstr_params
    -> annotations
    -> string

  val edge
    :  ?params:Utils.Formatting.pstr_params
    -> state * action * state
    -> string

  val actions
    :  ?params:Utils.Formatting.pstr_params
    -> ?from:state
    -> Block.t Actions.t
    -> string

  val edges
    :  ?params:Utils.Formatting.pstr_params
    -> Block.t Actions.t Edges.t
    -> string

  val fsm : ?params:Utils.Formatting.pstr_params -> fsm -> string
end

module Create : sig
  type state_params =
    | From of (state * int)
    | Of of (int * string)

  val state : state_params -> state

  type states_params =
    | From of (Block.t * int)
    | ()

  val states : states_params -> Block.t
  val label : int -> string

  type action_param =
    | Of of (int * string)
    | From of int

  val action : ?is_tau:bool -> ?annotation:annotations -> action_param -> action

  type alphabet_param =
    | ()
    | From of action list

  val alphabet : alphabet_param -> Alphabet.t

  type actions_params =
    | New of (action * Block.t)
    | Singleton of (action * state)
    | Destinations of (action * Block.t)
    | ()

  val actions : actions_params -> Block.t Actions.t
  val edges : ?size:int -> 'a -> Block.t Actions.t Edges.t

  val fsm
    :  ?is_complete:bool
    -> state option
    -> Alphabet.t
    -> Block.t
    -> Block.t Actions.t Edges.t
    -> fsm
end

module Clone : sig
  val state : state -> state
  val states : Block.t -> Block.t
  val init_state : state option -> state option
  val alphabet : Alphabet.t -> Alphabet.t
  val action : action -> action
  val actions : Block.t Actions.t -> Block.t Actions.t
  val edges : Block.t Actions.t Edges.t -> Block.t Actions.t Edges.t
  val fsm : fsm -> fsm
end

module IsMatch : sig
  val action : ?weak:bool -> action -> action -> bool

  val edge
    :  ?weak:bool
    -> state * action * state
    -> state * action * state
    -> bool
end

module New : sig
  val state : string -> fsm -> state

  val action
    :  ?is_tau:bool
    -> ?annotation:annotations
    -> string
    -> fsm
    -> action
end

module Append : sig
  val annotation' : state * action -> annotation -> annotation
  val annotation : state * action -> annotation -> annotation option

  type of_annotation =
    | Anno of annotation
    | Annos of annotations

  val annotations' : of_annotation -> annotations -> annotations
  val annotations : of_annotation -> annotations -> annotations option
  val alphabet : fsm -> action -> unit
  val state : ?skip_duplicate_names:bool -> fsm -> state -> unit
  val states : ?skip_duplicate_names:bool -> fsm -> Block.t -> unit

  type of_destination =
    | Singleton of state
    | Destinations of Block.t

  val action : Block.t Actions.t -> action * of_destination -> unit

  type has_edges =
    | FSM of fsm
    | Edges of Block.t Actions.t Edges.t

  val edge : has_edges -> state * action * of_destination -> unit
  val edges : has_edges -> state * Block.t Actions.t -> unit
end

module Filter : sig
  type kind_action_filter =
    | Matches of action
    | Label of string
    | To of state
    | IsSilent

  type kind_state_filter =
    | State of state
    | Action of kind_action_filter

  type kind_edge_filter =
    | From of state
    | FromAny of Block.t
    | Action of kind_action_filter

  type has_states =
    | FSM of fsm
    | States of Block.t

  type has_edges =
    | FSM of fsm
    | Edges of Block.t Actions.t Edges.t

  exception
    CannotFilterStatesUsingActionWithoutFSM of (Block.t * kind_action_filter)

  val filter_states : Block.t -> kind_state_filter -> Block.t

  val filter_actions
    :  ?weak:bool
    -> Block.t Actions.t
    -> kind_action_filter
    -> Block.t Actions.t

  val filter_edges
    :  ?weak:bool
    -> Block.t Actions.t Edges.t
    -> kind_edge_filter
    -> Block.t Actions.t Edges.t

  val actions
    :  ?weak:bool
    -> Block.t Actions.t
    -> kind_action_filter
    -> Block.t Actions.t

  val edges
    :  ?weak:bool
    -> has_edges
    -> kind_edge_filter
    -> Block.t Actions.t Edges.t

  val states : has_states -> kind_state_filter -> Block.t
end

module Get : sig
  type has_states =
    | FSM of fsm
    | Anno of annotation
    | Annos of annotations

  val states : has_states -> Block.t
  val actions_from : state -> Block.t Actions.t Edges.t -> Block.t Actions.t

  val actions_of
    :  ?weak:bool
    -> action
    -> Block.t Actions.t
    -> Block.t Actions.t option

  val silent_actions : Block.t Actions.t -> Block.t Actions.t
  val silent_edges : Block.t Actions.t Edges.t -> Block.t Actions.t Edges.t

  val edges_of
    :  ?weak:bool
    -> action
    -> Block.t Actions.t Edges.t
    -> Block.t Actions.t Edges.t

  val from_states : Block.t Actions.t Edges.t -> Block.t

  type has_silent_states =
    | Edges of Block.t Actions.t Edges.t
    | FSM of fsm

  val silent_states : has_silent_states -> Block.t

  type has_destinations =
    | Actions of Block.t Actions.t
    | Edges of Block.t Actions.t Edges.t

  val destinations : has_destinations -> Block.t
end

module Merge : sig
  val edges
    :  ?params:Utils.Logging.params
    -> int * Alphabet.t
    -> Block.t Actions.t Edges.t
    -> Block.t Actions.t Edges.t
    -> Block.t Actions.t Edges.t

  val fsms
    :  ?params:Utils.Logging.params
    -> fsm
    -> fsm
    -> fsm * (state, state) Hashtbl.t
end

module Organize : sig
  val edges : Block.t Actions.t Edges.t -> Block.t Actions.t Edges.t
  val fsm : fsm -> fsm
end

module Saturate : sig
  val visited_state : (state, int) Hashtbl.t -> state -> unit
  val max_revisit_num : int
  val is_state_revisitable : (state, int) Hashtbl.t -> state -> bool

  val saturated_action
    :  action
    -> action option
    -> state
    -> annotation
    -> action

  val collect_annotated_actions
    :  ?params:Utils.Logging.params
    -> (state, int) Hashtbl.t
    -> annotation
    -> Block.t
    -> Block.t Actions.t
    -> action option
    -> fsm
    -> unit

  val fsm : ?params:Utils.Logging.params -> fsm -> fsm

  exception AppendAnnotationIsNone of ((state * action) * annotation)
  exception AppendAnnotationsIsNone of (Append.of_annotation * annotations)

  val append_annotation : state * action -> annotation -> annotation
  val append_annotations : Append.of_annotation -> annotations -> annotations

  val explore_from
    :  state
    -> annotation
    -> Block.t Actions.t Edges.t
    -> annotations

  val annotated
    :  action
    -> state
    -> annotation
    -> Block.t Actions.t option
    -> action option

  val fsm_states : ?params:Utils.Logging.params -> fsm -> fsm
end
