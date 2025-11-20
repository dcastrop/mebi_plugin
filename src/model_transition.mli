type t = {
  from : Model_state.t;
  label : Model_label.t;
  goto : Model_state.t;
  constructor_trees : Mebi_constr.Tree.t list option;
}

val equal : t -> t -> bool
val compare : t -> t -> int
val to_string : t -> string
