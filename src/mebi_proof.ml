open Logging
open Mebi_wrapper
open Model

(***********************************************************************)
(*** Proof State *******************************************************)
(***********************************************************************)
let nothing () : unit Proofview.tactic = Proofview.tclUNIT ()

module State = struct
  type t =
    | NewProof
    | NewWeakSim
    | NewCofix
    | NewTransition of Transition_opt.t
    | GoalTransition of Transition_opt.t
    | ApplyConstructors of applicable_constructors

  and applicable_constructors =
    { annotation : Note.annotation
    ; tactics : tactic_to_apply list option
    }

  and tactic_to_apply = unit -> unit Proofview.tactic

  let to_string : t -> string = function
    | NewProof -> "NewProof"
    | NewWeakSim -> "NewWeakSim"
    | NewCofix -> "NewCofix"
    | NewTransition transition ->
      Printf.sprintf "NewTransition:\n%s" (Transition_opt.to_string transition)
    | GoalTransition transition ->
      Printf.sprintf "GoalTransition:\n%s" (Transition_opt.to_string transition)
    | ApplyConstructors applicable_constructors -> ""
  ;;
end

let default_proof_state : State.t = State.NewProof
let the_proof_state : State.t ref = ref default_proof_state
let reset_the_proof_state () : unit = the_proof_state := default_proof_state

(***********************************************************************)
(*** Handle Proof States ***********************************************)
(***********************************************************************)

let handle_proof_state (gl : Proofview.Goal.t) : unit Proofview.tactic =
  match !the_proof_state with
  | NewProof -> nothing ()
  | NewWeakSim -> nothing ()
  | NewCofix -> nothing ()
  | NewTransition mtransition -> nothing ()
  | GoalTransition mtransition -> nothing ()
  | ApplyConstructors napplicable_constructors -> nothing ()
;;

(***********************************************************************)
(*** Solve Proof *******************************************************)
(***********************************************************************)

let solve (upper_bound : int) (pstate : Declare.Proof.t) : Declare.Proof.t =
  let iter () : unit Proofview.tactic =
    Mebi_theories.tactics
      [ Proofview.Goal.enter (fun gl -> handle_proof_state gl)
      ; Mebi_tactics.simplify_and_subst_all ()
      ]
  in
  let rec iter_body (n : int) (pstate : Declare.Proof.t) : int * Declare.Proof.t
    =
    match Proof.is_done (Declare.Proof.get pstate), Int.compare n 0 with
    | true, _ ->
      Log.notice (Printf.sprintf "Solved in (%i) iterations." (upper_bound - n));
      n, pstate
    | false, -1 ->
      Log.notice
        (Printf.sprintf "Unsolved after (%i) iterations." (upper_bound - n));
      0, pstate
    | false, _ ->
      let pstate = Mebi_tactics.update_proof_by_tactic pstate (iter ()) in
      iter_body (n - 1) pstate
  in
  let rem, pstate = iter_body upper_bound pstate in
  Log.notice
    (Printf.sprintf "Stopped after (%i) iterations." (upper_bound - rem));
  pstate
;;
