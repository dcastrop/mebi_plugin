type t = {
  label : Model_label.t;
  annotations_list : annotations list;
  constructor_trees : Mebi_constr.Tree.t list;
}

and annotations = annotation list
and annotation = { from : Model_state.t; via : Model_label.t }

val hash : t -> int

val check_equal :
  ?annotations:bool ->
  ?constructor_trees:bool ->
  t ->
  t ->
  bool

val eq_annotations_list :
  annotations list -> annotations list -> bool

val eq_annotations : annotations -> annotations -> bool
val eq_annotation : annotation -> annotation -> bool

val eq_constructor_tree_list :
  Mebi_constr.Tree.t list -> Mebi_constr.Tree.t list -> bool

val equal : t -> t -> bool

val check_compare :
  ?annotations:bool -> ?constructor_trees:bool -> t -> t -> int

val annotations_list_compare :
  annotations list -> annotations list -> int

val annotations_compare : annotations -> annotations -> int
val annotation_compare : annotation -> annotation -> int

val constructor_tree_list_compare :
  Mebi_constr.Tree.t list -> Mebi_constr.Tree.t list -> int

val compare : t -> t -> int
val to_string : ?args:Utils.Strfy.style_args -> t -> string

val annotations_list_to_string :
  ?args:Utils.Strfy.style_args -> annotations list -> string

val annotations_to_string :
  ?args:Utils.Strfy.style_args -> annotations -> string

val annotation_to_string :
  ?args:Utils.Strfy.style_args -> annotation -> string

val constructor_tree_list_to_string :
  ?args:Utils.Strfy.style_args ->
  Mebi_constr.Tree.t list ->
  string
