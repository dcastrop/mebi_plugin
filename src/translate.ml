(*********************************************************************)
(****** Fsm <-> Lts **************************************************)
(*********************************************************************)

let lts_to_fsm (g : Lts.t) : Fsm.t =
  match g with
  | { init; transitions; states; info } ->
    let edges = Model.transitions_to_edges transitions in
    let alphabet = Model.alphabet_from_edges edges in
    { init; alphabet; states; edges; info }
;;

let fsm_to_lts (m : Fsm.t) : Lts.t =
  match m with
  | { init; alphabet = _; states; edges; info } ->
    let transitions = Model.edges_to_transitions edges in
    { init; transitions; states; info }
;;
