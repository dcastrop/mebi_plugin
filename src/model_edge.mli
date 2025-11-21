type t = {
  from : Model_state.t;
  action : Model_action.t;
  goto : Model_state.t;
}

val equal : t -> t -> bool
val compare : t -> t -> int
val to_string : ?args:Utils.Strfy.style_args -> t -> string
