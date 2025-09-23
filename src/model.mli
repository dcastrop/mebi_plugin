module Info : sig
  module Coq : sig
    type t =
      Mebi_wrapper.IntEncoding.t * (string * string list)

    val to_string : t -> string
  end

  type t = {
    is_complete : bool;
    bound : int;
    num_terminals : int;
    num_labels : int;
    num_states : int;
    num_edges : int;
    coq_info : Coq.t list option;
    weak_info : Params.WeakKindEnc.t list option;
  }

  val merge :
    ?bound:int ->
    ?num_terminals:int ->
    ?num_labels:int ->
    ?num_states:int ->
    ?num_edges:int ->
    t ->
    t ->
    t

  val to_string : ?pstr:bool -> ?indents:int -> t -> string

  val opt_to_string :
    ?pstr:bool -> ?indents:int -> t option -> string

  val pstr :
    ?skip_leading_tab:bool -> ?indents:int -> t -> string

  val opt_pstr :
    ?skip_leading_tab:bool ->
    ?indents:int ->
    t option ->
    string

  val opt_is_complete : t option -> bool option
end

module State : sig
  type t = Mebi_wrapper.IntEncoding.t * string option

  val eq : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int

  val to_string :
    ?skip_leading_tab:bool ->
    ?indents:int ->
    ?pstr:bool ->
    t ->
    string

  val pstr :
    ?skip_leading_tab:bool -> ?indents:int -> t -> string
end

module Action : sig
  module Label : sig
    type t =
      Mebi_wrapper.IntEncoding.t
      * (string option * bool option)

    val to_string : t -> string
    val pstr : ?indents:int -> t -> string
    val is_silent : 'a * ('b * 'c) -> 'c
    val eq : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
  end

  module MetaData : sig
    type t = string list

    val merge : t -> t -> t
    val from_opt : t option -> t option
    val to_string : t -> string
    val eq : t -> t -> bool
    val eq_opt : t option -> t option -> bool
    val compare : t -> t -> int
    val compare_opt : t option -> t option -> int
  end

  type t = {
    label : Label.t;
    meta : MetaData.t;
    mutable annos : annotations;
  }

  and annotation_pair = State.t * t
  and annotation = annotation_pair list
  and annotations = annotation list

  val saturated : ?anno:annotation -> t -> t

  exception CannotMergeActionWithDifferentLabels of (t * t)

  val merge : t -> t -> t
  val annotation_pair_to_string : annotation_pair -> string
  val annotation_to_string : annotation -> string
  val annotations_to_string : annotations -> string

  val to_string :
    ?skip_leading_tab:bool ->
    ?indents:int ->
    ?pstr:bool ->
    t ->
    string

  val pstr : ?indents:int -> t -> string

  exception ActionSilenceIsNone of t

  val is_silent : t -> bool
  val has_label : Label.t -> t -> bool
  val eq : ?annos:bool -> ?meta:bool -> t -> t -> bool

  val anno_eq :
    ?annos:bool ->
    ?meta:bool ->
    annotation ->
    annotation ->
    bool

  val annos_eq :
    ?annos:bool ->
    ?meta:bool ->
    annotations ->
    annotations ->
    bool

  val compare : ?annos:bool -> ?meta:bool -> t -> t -> Int.t

  val anno_compare :
    ?annos:bool ->
    ?meta:bool ->
    annotation ->
    annotation ->
    Int.t

  val annos_compare :
    ?annos:bool ->
    ?meta:bool ->
    annotations ->
    annotations ->
    Int.t

  val hash : t -> int
end

module Transition : sig
  type t =
    State.t
    * Action.Label.t
    * State.t
    * Action.MetaData.t option

  val to_string : t -> string
  val pstr : ?indents:int -> t -> string
  val eq : t -> t -> bool
  val compare : t -> t -> int
end

module Edge : sig
  type t = State.t * Action.t * State.t

  exception CannotMergeEdgeWithDifferentStates of (t * t)

  val merge : t -> t -> t

  val to_string :
    ?skip_leading_tab:bool ->
    ?indents:int ->
    ?pstr:bool ->
    t ->
    string

  val pstr :
    ?skip_leading_tab:bool -> ?indents:int -> t -> string
end

module States : sig
  type elt = State.t
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

module Partition : sig
  type elt = States.t
  type t = Set.Make(States).t

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

module Alphabet : sig
  type elt = Action.Label.t
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
  type key = Action.t
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

  val filter_map_inplace :
    (key -> 'a -> 'a option) -> 'a t -> unit

  val fold :
    (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

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
  type key = State.t
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

  val filter_map_inplace :
    (key -> 'a -> 'a option) -> 'a t -> unit

  val fold :
    (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

  val length : 'a t -> int
  val stats : 'a t -> Hashtbl.statistics
  val to_seq : 'a t -> (key * 'a) Seq.t
  val to_seq_keys : 'a t -> key Seq.t
  val to_seq_values : 'a t -> 'a Seq.t
  val add_seq : 'a t -> (key * 'a) Seq.t -> unit
  val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
  val of_seq : (key * 'a) Seq.t -> 'a t
end

module Transitions : sig
  type elt = Transition.t
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

module ActionPair : sig
  type t = Action.t * States.t

  val compare :
    Action.t * States.t -> Action.t * States.t -> Int.t
end

module ActionPairs : sig
  type elt = ActionPair.t
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

val action_pairs_to_list : ActionPairs.t -> ActionPair.t list

type t =
  | LTS of
      (State.t option
      * States.t
      * Alphabet.t
      * States.t
      * Transitions.t
      * Info.t option)
  | FSM of
      (State.t option
      * States.t
      * Alphabet.t
      * States.t
      * States.t Actions.t Edges.t
      * Info.t option)

val label_to_action :
  ?meta:Action.MetaData.t option -> Action.Label.t -> Action.t

val action_to_label : Action.t -> Action.Label.t

val action_list_to_label_list :
  Action.t list -> Action.Label.t list

val edges_to_list : States.t Actions.t Edges.t -> Edge.t list

val edges_to_transitions :
  States.t Actions.t Edges.t -> Transitions.t

val transitions_to_edges :
  Transitions.t -> States.t Actions.t Edges.t

val edges_to_transition_list :
  States.t Actions.t Edges.t -> Transition.t list

val transition_list_to_edges :
  Transition.t list -> States.t Actions.t Edges.t

val alphabet_from_actions :
  ?acc:Alphabet.t -> States.t Actions.t -> Alphabet.t

val alphabet_from_edges :
  States.t Actions.t Edges.t -> Alphabet.t

val alphabet_from_transitions : Transitions.t -> Alphabet.t

val add_action :
  States.t Actions.t -> Action.t -> State.t -> unit

val add_edge :
  States.t Actions.t Edges.t ->
  State.t ->
  Action.t ->
  State.t ->
  unit

val get_num_labels : ?num:int -> States.t Actions.t -> int

val get_num_edges :
  ?num:int -> States.t Actions.t Edges.t -> int

val get_actions :
  States.t Actions.t Edges.t -> States.t Actions.t

val get_actions_from :
  State.t -> States.t Actions.t Edges.t -> States.t Actions.t

val get_actions_of :
  Action.t -> States.t Actions.t Edges.t -> States.t Actions.t

val get_actions_with_label :
  Action.Label.t ->
  States.t Actions.t Edges.t ->
  States.t Actions.t

val get_edges_with_label :
  Action.Label.t ->
  States.t Actions.t Edges.t ->
  States.t Actions.t Edges.t

val get_edges_from :
  State.t ->
  States.t Actions.t Edges.t ->
  States.t Actions.t Edges.t

val get_destinations : States.t Actions.t -> States.t

val get_reachable_blocks :
  States.t Actions.t -> Partition.t -> Partition.t

val get_reachable_blocks_opt :
  States.t Actions.t -> Partition.t -> Partition.t option

val get_num_blocks : Partition.t -> int

val merge_actions :
  States.t Actions.t ->
  States.t Actions.t ->
  States.t Actions.t

val merge_edges :
  States.t Actions.t Edges.t ->
  States.t Actions.t Edges.t ->
  States.t Actions.t Edges.t

val pstr_info : ?indents:int -> Info.t -> string
val pstr_info_opt : ?indents:int -> Info.t option -> string
val pstr_label : ?indents:int -> Action.Label.t -> string
val pstr_action : ?indents:int -> Action.t -> string

val pstr_alphabet :
  ?skip_leading_tab:bool ->
  ?indents:int ->
  ?details:bool ->
  Alphabet.t ->
  string

val pstr_state :
  ?skip_leading_tab:bool -> ?indents:int -> State.t -> string

val pstr_state_opt :
  ?skip_leading_tab:bool ->
  ?indents:int ->
  State.t option ->
  string

val pstr_states :
  ?skip_leading_tab:bool ->
  ?indents:int ->
  ?details:bool ->
  States.t ->
  string

val pstr_partition :
  ?skip_leading_tab:bool ->
  ?indents:int ->
  ?details:bool ->
  Partition.t ->
  string

val pstr_transition : ?indents:int -> Transition.t -> string

val pstr_transitions :
  ?skip_leading_tab:bool ->
  ?indents:int ->
  ?details:bool ->
  Transitions.t ->
  string

val pstr_edge :
  ?skip_leading_tab:bool -> ?indents:int -> Edge.t -> string

val pstr_edges_from_a :
  ?skip_leading_tab:bool ->
  ?indents:int ->
  State.t ->
  Action.t ->
  States.t ->
  string

val pstr_edges_from :
  ?skip_leading_tab:bool ->
  ?indents:int ->
  State.t ->
  States.t Actions.t ->
  string

val pstr_edges :
  ?skip_leading_tab:bool ->
  ?indents:int ->
  States.t Actions.t Edges.t ->
  string

val check_info : t -> unit

val merge_action_pairs :
  ActionPairs.t -> ActionPairs.t -> ActionPairs.t
