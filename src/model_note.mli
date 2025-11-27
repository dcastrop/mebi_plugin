type t =
  { from : Model_state.t
  ; via : Model_label.t
  }

val equal : t -> t -> bool
val compare : t -> t -> int

type annotation = t list

val annotation_equal : annotation -> annotation -> bool
val annotation_compare : annotation -> annotation -> int
val annotation_is_empty : annotation -> bool
val add_note : t -> annotation -> annotation
val union_annotation : annotation -> annotation -> annotation

type annotations = annotation list

val annotations_equal : annotations -> annotations -> bool
val annotations_compare : annotations -> annotations -> int
val annotations_is_empty : annotations -> bool
val union_annotations : annotations -> annotations -> annotations
val add_annotation : annotation -> annotations -> annotations
val to_string : ?args:Utils.Strfy.style_args -> t -> string
val annotation_to_string : ?args:Utils.Strfy.style_args -> annotation -> string

val annotations_to_string
  :  ?args:Utils.Strfy.style_args
  -> annotations
  -> string
