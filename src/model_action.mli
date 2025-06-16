type t = {
  label : Model_label.t;
  is_silent : bool option;
  info : string option;
  mutable annos : annotations;
}

and annotation = (Model_label.t * t) list
and annotations = annotation list

val anno_eq : annotation -> annotation -> bool
val annos_eq : annotations -> annotations -> bool
val eq : t -> t -> bool
val anno_compare : annotation -> annotation -> int
val annos_compare : annotations -> annotations -> int
val compare : t -> t -> int
val hash : t -> int
