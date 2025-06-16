open Model

type t =
  { init : Model_state.t option
  ; states : States.t
  ; transitions : Model_transition.t list
  ; info : Utils.model_info option
  }

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
