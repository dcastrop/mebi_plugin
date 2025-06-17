open Model

type t =
  { init : Model_state.t option
  ; states : States.t
  ; transitions : Transitions.t
  ; info : Utils.model_info option
  }

let create
      (init : Model_state.t option)
      (states : States.t)
      (transitions : Transitions.t)
      (info : Utils.model_info option)
  : t
  =
  { init; states; transitions; info }
;;

(*********************************************************************)
(****** Add **********************************************************)
(*********************************************************************)

let add_state (g : t) (s : Model_state.t) : t =
  match g with
  | { init; states; transitions; info } ->
    { g with states = States.add s states }
;;

let add_state_list (g : t) (ss : Model_state.t list) : t =
  match g with
  | { init; states; transitions; info } ->
    { g with states = States.add_seq (List.to_seq ss) states }
;;

let add_states (g : t) (ss : States.t) : t =
  match g with
  | { init; states; transitions; info } ->
    { g with states = States.union ss states }
;;

let add_transition
      (g : t)
      (from : Model_state.t)
      (l : Model_label.t)
      (dest : Model_state.t)
      (meta : Model_label.meta option)
  : t
  =
  match g with
  | { init; states; transitions; info } ->
    { g with transitions = Transitions.add (from, l, dest, meta) transitions }
;;

let add_transition_from_action
      (g : t)
      (from : Model_state.t)
      (a : Model_action.t)
      (dest : Model_state.t)
  : t
  =
  let l, meta = Model_action.to_label_meta_pair a in
  add_transition g from l dest (Some meta)
;;

(*********************************************************************)
(****** Pretty-Strings ***********************************************)
(*********************************************************************)
let pstr ?(skip_leading_tab : bool = false) ?(indents : int = 0) (m : t)
  : string
  =
  (* Printf.sprintf *)
  "TODO: Lts.pstr"
;;
