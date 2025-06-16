module States : sig
  type elt = Model_state.t
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
  type elt = Model_label.t
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
  type key = Model_action.t
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
  type key = Model_state.t
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

val label_to_action :
  ?meta:Model_label.meta option ->
  Model_label.t ->
  Model_action.t

val action_to_label : Model_action.t -> Model_label.t

val edges_to_list :
  States.t Actions.t Edges.t -> Model_edge.t list

val edges_to_transition_list :
  States.t Actions.t Edges.t -> Model_transition.t list

val transition_list_to_edges :
  Model_transition.t list -> States.t Actions.t Edges.t

val alphabet_from_actions :
  ?acc:Alphabet.t -> States.t Actions.t -> Alphabet.t

val alphabet_from_edges :
  States.t Actions.t Edges.t -> Alphabet.t
