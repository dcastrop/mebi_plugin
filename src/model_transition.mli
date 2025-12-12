type t =
  { from : Model_state.t
  ; label : Model_label.t
  ; goto : Model_state.t
  ; annotation : Model_note.annotation option
  ; constructor_trees : Mebi_constr.Tree.t list
  }

val create
  :  Model_state.t
  -> Model_label.t
  -> Model_state.t
  -> ?annotation:Model_note.annotation option
  -> ?constructor_trees:Mebi_constr.Tree.t list
  -> unit
  -> t

val equal : t -> t -> bool
val compare : t -> t -> int
val annotation_is_empty : t -> bool
val to_string : ?args:Utils.Strfy.style_args -> t -> string
