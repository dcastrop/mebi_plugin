open Fsm

let to_fsm (lts : Lts.lts) : fsm =
  match lts with
  | { init = _init; transitions; _ } ->
    let init : state =
      Create.state
        (Of
           ( 0
           , match _init with
             | None -> (Lts.Transitions.min_elt transitions).from
             | Some init' -> init' ))
    in
    let states : States.t = States.singleton init in
    let fsm : fsm =
      Create.fsm (Some init) Alphabet.empty states (Edges.create 0)
    in
    Lts.Transitions.fold
      (fun (t : Lts.transition) (acc : fsm) ->
         let a : action = New.action t.label fsm in
         let from : state = New.state t.from fsm in
         let destination : state = New.state t.destination fsm in
         Append.alphabet fsm a;
         Append.state fsm from;
         Append.state fsm destination;
         Append.edge fsm (from, a, destination);
         fsm)
      transitions
      fsm
;;

(* let to_lts (fsm:fsm) : lts =

;; *)
