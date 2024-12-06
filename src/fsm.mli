type state =
  { id : int
  ; hash : int
  ; pp : string
  }

val state : ?pp:string -> ?hash:int -> int -> state

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

module Actions : sig
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

type ('a, 'b) transition =
  { action : 'a
  ; to_state : 'b
  }

type fsm_transition = (action, state) transition
type edges = (state, fsm_transition) Hashtbl.t

type fsm =
  { init : state
  ; states : States.t
  ; actions : Actions.t
  ; edges : (state, fsm_transition) Hashtbl.t
  }

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

val pstr_actions : ?ids:unit -> ?long:unit -> ?indent:int -> Actions.t -> string

val pstr_edge
  :  ?ids:unit
  -> ?pp:unit
  -> ?long:unit
  -> state * fsm_transition
  -> string

val handle_edge_pstr
  :  unit option
  -> unit option
  -> unit option
  -> state * fsm_transition
  -> string

val pstr_edges
  :  ?ids:unit
  -> ?pp:unit
  -> ?long:unit
  -> ?indent:int
  -> edges
  -> string

val handle_states_pstr
  :  unit option
  -> unit option
  -> unit option
  -> States.t
  -> string

val handle_actions_pstr
  :  unit option
  -> unit option
  -> unit option
  -> Actions.t
  -> string

val handle_edges_pstr
  :  unit option
  -> unit option
  -> unit option
  -> edges
  -> string

val pstr_fsm : ?ids:unit -> ?pp:unit -> ?long:unit -> fsm -> string
