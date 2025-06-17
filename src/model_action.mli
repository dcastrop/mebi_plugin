type t = {
  label : Model_label.t;
  is_silent : bool option;
  info : string option;
  mutable annos : annotations;
}

and annotation_pair = Model_state.t * t
and annotation = annotation_pair list
and annotations = annotation list

val saturated : ?anno:annotation -> t -> t
val annotate : t -> annotation -> unit

exception ActionSilenceIsNone of t

val is_silent : t -> bool
val has_label : Model_label.t -> t -> bool
val to_label_meta_pair : t -> Model_label.t * Model_label.meta
val eq : t -> t -> bool
val anno_eq : annotation -> annotation -> bool
val annos_eq : annotations -> annotations -> bool
val compare : t -> t -> Int.t
val anno_compare : annotation -> annotation -> Int.t
val annos_compare : annotations -> annotations -> Int.t
val hash : t -> int
