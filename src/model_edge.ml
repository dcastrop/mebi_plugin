type t =
  { from : Model_state.t
  ; via : Model_action.t
  ; goto : Model_state.t
  }

let equal (a : t) (b : t) : bool =
  Model_state.equal a.from b.from
  && Model_action.equal a.via b.via
  && Model_state.equal a.goto b.goto
;;

let compare (a : t) (b : t) : int =
  Int.compare
    (Model_state.compare a.from b.from)
    (Int.compare
       (Model_action.compare a.via b.via)
       (Model_state.compare a.goto b.goto))
;;

let to_string (a : t) (b : t) : string = "TODO: model_edge.to_string"
