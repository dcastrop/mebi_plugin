open Utils
open Fsm

exception UnexpectedBisimResult of Bisimilarity.result

let run ?(params : Params.log = Params.Default.log ()) (the_fsm : fsm) : fsm =
  let bisim_result : Bisimilarity.of_bisim_result =
    Bisimilarity.RCP.KS90.run ~params (Minimize the_fsm)
  in
  let res : Bisimilarity.result = Bisimilarity.RCP.KS90.result bisim_result in
  match res with
  | MinimResult (the_fsm, pi) -> the_fsm
  | _ -> raise (UnexpectedBisimResult res)
;;
