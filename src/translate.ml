(*********************************************************************)
(****** Fsm <-> Lts **************************************************)
(*********************************************************************)

let lts_to_fsm (g : Lts.t) : Fsm.t =
  match g with
  | { init; alphabet; transitions; states; info } ->
    let edges = Model.transitions_to_edges transitions in
    (* let alphabet =
       if Model.Alphabet.is_empty alphabet
       then Model.alphabet_from_edges edges
       else alphabet
       in *)
    { init; alphabet; states; edges; info }
;;

let fsm_to_lts (m : Fsm.t) : Lts.t =
  match m with
  | { init; alphabet; states; edges; info } ->
    let transitions = Model.edges_to_transitions edges in
    { init; alphabet; transitions; states; info }
;;
