type t = {
  from : Model_state.t;
  via : Model_label.t;
  using : Mebi_constr.Tree.t list;
  goto : Model_state.t;
}

val create :
  Model_state.t ->
  Model_label.t ->
  Mebi_constr.Tree.t list ->
  Model_state.t ->
  t

val equal : t -> t -> bool
val compare : t -> t -> int

type annotation = { this : t; next : annotation option }

val annotation_equal : annotation -> annotation -> bool
val annotation_compare : annotation -> annotation -> int
val annotation_is_empty : annotation -> bool
val annotation_depth : annotation -> int
val shorter_annotation : annotation -> annotation -> annotation
val exists : t -> annotation -> bool
val add_note : t -> annotation -> annotation
val exists_label : Model_label.t -> annotation -> bool
val last : annotation -> t

exception Model_note_CannotDropLast of annotation

val drop_last : annotation -> annotation

module Annotations : sig
  type elt = annotation
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

val to_string : ?args:Utils.Strfy.style_args -> t -> string

val annotation_to_string :
  ?args:Utils.Strfy.style_args -> annotation -> string

val annotations_to_string :
  ?args:Utils.Strfy.style_args -> Annotations.t -> string
