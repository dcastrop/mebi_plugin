type t = {
  label : Model_label.t;
  annotation : Model_note.annotation;
  constructor_trees : Mebi_constr.Tree.t list;
}

val hash : t -> int

val check_equal :
  ?annotation:bool -> ?constructor_trees:bool -> t -> t -> bool

val equal : t -> t -> bool
val wk_equal : t -> t -> bool

val check_compare :
  ?annotation:bool -> ?constructor_trees:bool -> t -> t -> int

val compare : t -> t -> int
val to_string : ?args:Utils.Strfy.style_args -> t -> string
