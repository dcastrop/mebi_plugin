type t =
  { from : Model_state.t
  ; via : Model_label.t
  ; using : Mebi_constr.Tree.t list
  ; goto : Model_state.t
  }

val create
  :  Model_state.t
  -> Model_label.t
  -> Mebi_constr.Tree.t list
  -> Model_state.t
  -> t

val equal : t -> t -> bool
val compare : t -> t -> int

type annotation =
  { this : t
  ; next : annotation option
  }

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
val to_string : ?args:Utils.Strfy.style_args -> t -> string
val annotation_to_string : ?args:Utils.Strfy.style_args -> annotation -> string
