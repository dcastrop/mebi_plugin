type t =
  { from : Model_state.t
  ; action : Model_action.t
  ; goto : Model_state.t
  }

let create
      (from : Model_state.t)
      (action : Model_action.t)
      (goto : Model_state.t)
  : t
  =
  { from; action; goto }
;;

let equal (a : t) (b : t) : bool =
  Model_state.equal a.from b.from
  && Model_action.equal a.action b.action
  && Model_state.equal a.goto b.goto
;;

let compare (a : t) (b : t) : int =
  Utils.compare_chain
    [ Model_state.compare a.from b.from
    ; Model_action.compare a.action b.action
    ; Model_state.compare a.goto b.goto
    ]
;;

open Utils.Strfy

let to_string ?(args : style_args = record_args ()) (x : t) : string =
  "TODO: model_edge.to_string"
;;
