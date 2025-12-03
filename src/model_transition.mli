type t = {
  from : Model_state.t;
  label : Model_label.t;
  goto : Model_state.t;
  annotations : Model_note.annotations option;
  constructor_trees : Mebi_constr.Tree.t list option;
}

val equal : t -> t -> bool
val compare : t -> t -> int
val annotations_is_empty : t -> bool
val to_string : ?args:Utils.Strfy.style_args -> t -> string
