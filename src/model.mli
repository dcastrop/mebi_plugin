module Info = Model_info
module State = Model_state
module Label = Model_label
module Note = Model_note
module Transition = Model_transition
module Transition_opt = Model_transition_opt
module Action = Model_action
module Edge = Model_edge
module Enc = Mebi_setup.Enc
module Tree = Mebi_constr.Tree

val nest : Utils.Strfy.style_args -> Utils.Strfy.style_args

type style_args = Utils.Strfy.style_args

val style_args :
  ?indent:int ->
  ?newline:bool ->
  ?nested:bool ->
  ?name:string ->
  ?style:Utils.Strfy.collection_style option ->
  unit ->
  Utils.Strfy.style_args

val collection_style :
  Utils.Strfy.collection_kind -> Utils.Strfy.collection_style

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

val states_to_string : ?args:style_args -> States.t -> string

val destinations_to_string :
  ?args:style_args -> States.t -> string

val decode_state_opt : Enc.t -> States.t -> State.t option

exception Model_CannotDecodeState of (Enc.t * States.t)

val decode_state : Enc.t -> States.t -> State.t

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

val partition_to_string :
  ?args:style_args -> Partition.t -> string

exception Model_Bisim_State_NotFound of (State.t * Partition.t)

val get_bisim_states : State.t -> Partition.t -> States.t

module Alphabet : sig
  type elt = Label.t
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

val alphabet_to_string :
  ?args:style_args -> Alphabet.t -> string

exception
  Model_Alphabet_LabelOfEncNotFound of (Enc.t * Alphabet.t)

val find_label_of_enc : Enc.t -> Alphabet.t -> Label.t
val silent_label_opt : Alphabet.t -> Label.t option

exception Model_Alphabet_SilentLabelNotFound of Alphabet.t

val silent_label : Alphabet.t -> Label.t
val silent_action : Alphabet.t -> Action.t

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

val transitions_to_string :
  ?args:style_args -> Transitions.t -> string

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

val actions_to_string :
  ?args:style_args -> States.t Actions.t -> string

val action_labels_to_string :
  ?args:style_args -> States.t Actions.t -> string

exception Model_Action_HasNoAnnotations of Action.t

val get_shortest_annotation : Action.t -> Note.annotation

val get_shortest_annotation_from :
  State.t -> Action.t -> Note.annotation

exception Model_Actions_IsEmpty of States.t Actions.t

val get_action_with_shortest_annotation :
  States.t Actions.t -> Action.t

exception Model_Action_HasNoConstructors of Action.t

val get_shortest_constructor : Action.t -> Tree.node list

exception
  Model_Action_HasSilentLabel_ButIsSaturated of Action.t

val is_action_annotated : Action.t -> bool
val is_action_silent : Action.t -> bool

exception
  Model_NoActionLabelled of
    (bool * Label.t * States.t Actions.t)

val get_actions_labelled :
  ?annotated:bool ->
  Label.t ->
  States.t Actions.t ->
  States.t Actions.t

val get_action_destinations : States.t Actions.t -> States.t

val get_reachable_partition :
  Partition.t -> States.t Actions.t -> Partition.t

val get_reachable_partition_opt :
  Partition.t -> States.t Actions.t -> Partition.t option

val update_destinations :
  States.t Actions.t -> Action.t -> States.t -> States.t

val update_action :
  States.t Actions.t -> Action.t -> States.t -> unit

val alphabet_of_actions : States.t Actions.t -> Alphabet.t

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

val update_edge :
  States.t Actions.t Edges.t ->
  State.t ->
  Action.t ->
  States.t ->
  unit

val add_edge : States.t Actions.t Edges.t -> Edge.t -> unit

val add_edges :
  States.t Actions.t Edges.t -> Edge.t list -> unit

exception
  Model_NoActionLabelledFrom of
    (bool * State.t * Label.t * States.t Actions.t Edges.t)

val get_actions_labelled_from :
  ?annotated:bool ->
  State.t ->
  Label.t ->
  States.t Actions.t Edges.t ->
  States.t Actions.t

val get_edges_labelled :
  Label.t ->
  States.t Actions.t Edges.t ->
  States.t Actions.t Edges.t

val get_reachable_blocks_opt :
  Partition.t ->
  States.t Actions.t Edges.t ->
  State.t ->
  Partition.t option

exception Model_TransitionOptGotoNone of Transition_opt.t

val transition_opt_to_transition :
  Transition_opt.t -> Transition.t

val transition_to_action : Transition.t -> Action.t
val transition_opt_to_action : Transition_opt.t -> Action.t
val transition_to_edge : Transition.t -> Edge.t
val edge_to_transition : Edge.t -> Transition.t

val action_destinations_to_transitions :
  State.t ->
  Action.t ->
  States.t ->
  Transitions.t ->
  Transitions.t

val actions_to_transitions :
  State.t ->
  States.t Actions.t ->
  Transitions.t ->
  Transitions.t

val edges_to_transitions :
  States.t Actions.t Edges.t -> Transitions.t

val transitions_to_edges :
  Transitions.t -> States.t Actions.t Edges.t

val edges_to_string :
  ?args:style_args -> States.t Actions.t Edges.t -> string

val merge_info_field :
  'a list option -> 'a list option -> 'a list option

val merge_info : Info.t -> Info.t -> Info.t
val merge_action : Action.t -> Action.t -> Action.t

val merge_actions :
  States.t Actions.t ->
  States.t Actions.t ->
  States.t Actions.t

val merge_edges :
  States.t Actions.t Edges.t ->
  States.t Actions.t Edges.t ->
  States.t Actions.t Edges.t

type kind =
  | LTS of
      (State.t option
      * States.t
      * Alphabet.t
      * States.t
      * Transitions.t
      * Info.t)
  | FSM of
      (State.t option
      * States.t
      * Alphabet.t
      * States.t
      * States.t Actions.t Edges.t
      * Info.t)

module Lts : sig
  type t = {
    init : State.t option;
    terminals : States.t;
    alphabet : Alphabet.t;
    states : States.t;
    transitions : Transitions.t;
    info : Info.t;
  }

  val to_model : t -> kind
  val of_model : kind -> t
  val to_string : ?args:style_args -> t -> string
end

module Fsm : sig
  type t = {
    init : State.t option;
    terminals : States.t;
    alphabet : Alphabet.t;
    states : States.t;
    edges : States.t Actions.t Edges.t;
    info : Info.t;
  }

  and pair = t * t

  val to_model : t -> kind
  val of_model : kind -> t
  val clone : t -> t
  val merge : t -> t -> t

  exception Model_Fsm_OriginOfStateNotFound of (State.t * pair)

  val origin_of_state_opt : State.t -> t -> t -> int option
  val origin_of_state : State.t -> t -> t -> int
  val to_string : ?args:style_args -> t -> string
end

module Saturate : sig
  val add_annotation :
    State.t -> Action.t -> Note.annotation -> Note.annotation

  exception
    Model_Saturate_CannotSaturateActionsWithUnknownVisibility of
      Action.t

  module StateTracker : sig
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

  val max_visit_num : int
  val can_revisit : State.t -> int StateTracker.t -> bool
  val log_visit : State.t -> int StateTracker.t -> unit

  val check_update_named :
    Action.t -> Action.t option -> Action.t option

  val update_named_annotation :
    Action.t -> Note.annotation -> Action.t

  val stop :
    ?named:Action.t option ->
    Note.annotation ->
    State.t ->
    (Action.t * States.t) list ->
    (Action.t * States.t) list

  val check_from :
    ?named:Action.t option ->
    Note.annotation ->
    States.t Actions.t Edges.t ->
    State.t ->
    int StateTracker.t ->
    (Action.t * States.t) list ->
    (Action.t * States.t) list

  val check_actions :
    ?named:Action.t option ->
    Note.annotation ->
    States.t Actions.t Edges.t ->
    State.t ->
    States.t Actions.t ->
    int StateTracker.t ->
    (Action.t * States.t) list ->
    (Action.t * States.t) list

  val check_named :
    ?named:Action.t option ->
    Note.annotation ->
    States.t Actions.t Edges.t ->
    State.t ->
    Action.t ->
    States.t ->
    int StateTracker.t ->
    (Action.t * States.t) list ->
    bool ref ->
    (Action.t * States.t) list

  val check_destinations :
    ?named:Action.t option ->
    Note.annotation ->
    States.t Actions.t Edges.t ->
    States.t ->
    int StateTracker.t ->
    (Action.t * States.t) list ->
    (Action.t * States.t) list

  val merge_saturated_tuples :
    (Action.t * States.t) list ->
    (Action.t * States.t) list ->
    (Action.t * States.t) list

  val try_update_saturated_tuple :
    Action.t * States.t ->
    (Action.t * States.t) list ->
    (Action.t * States.t) option * (Action.t * States.t) list

  val edge_action_destinations :
    ?named:Action.t option ->
    Note.annotation ->
    States.t Actions.t Edges.t ->
    State.t ->
    States.t ->
    (Action.t * States.t) list ->
    (Action.t * States.t) list

  val edge_actions :
    ?named:Action.t option ->
    States.t Actions.t Edges.t ->
    State.t ->
    States.t Actions.t ->
    (Action.t * States.t) list ->
    (Action.t * States.t) list

  val edge :
    States.t Actions.t ->
    States.t Actions.t Edges.t ->
    State.t ->
    States.t Actions.t ->
    unit

  val edges :
    Alphabet.t ->
    States.t ->
    States.t Actions.t Edges.t ->
    States.t Actions.t Edges.t

  val fsm : Fsm.t -> Fsm.t
end
