open Utils
open Fsm

exception UnexpectedBisimResult of Bisimilarity.result

type result = fsm * (state, state) Hashtbl.t

let run ?(params : Params.log = Params.Default.log ()) (the_fsm : fsm) : result =
  let bisim_result : Bisimilarity.of_bisim_result =
    Bisimilarity.RCP.KS90.run ~params (Minimize the_fsm)
  in
  let res : Bisimilarity.result = Bisimilarity.RCP.KS90.result bisim_result in
  match res with
  | MinimResult pi ->
    let minim_fsm : fsm =
      (* TODO: minimize? *)
      the_fsm
    in
    let map_of_states : (state, state) Hashtbl.t =
      Partition.cardinal pi |> Hashtbl.create
    in
    minim_fsm, map_of_states
  | _ -> raise (UnexpectedBisimResult res)
;;
