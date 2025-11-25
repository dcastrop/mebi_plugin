open Logging
open Mebi_wrapper
open Model
module Hyp = Mebi_hypothesis

let do_nothing () : unit Proofview.tactic =
  Log.warning "mebi_proof, case just does nothing";
  Proofview.tclUNIT ()
;;

let do_simplify gl : unit Proofview.tactic =
  Mebi_tactics.simplify_and_subst_all ~gl ()
;;

let do_rt1n_refl gl =
  Mebi_theories.tactics
    [ Mebi_tactics.apply ~gl (Mebi_theories.c_rt1n_refl ()); do_simplify gl ]
;;

(***********************************************************************)
(*** Proof State *******************************************************)
(***********************************************************************)

module PState = struct
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

let default_proof_state : PState.t = PState.NewProof
let the_proof_state : PState.t ref = ref default_proof_state
let reset_the_proof_state () : unit = the_proof_state := default_proof_state

(***********************************************************************)
(*** Bisimilarity Result ***********************************************)
(***********************************************************************)

let mfsm () : Fsm.t = (Algorithms.Bisimilar.get_the_result ()).the_fsm_1
let nfsm () : Fsm.t = (Algorithms.Bisimilar.get_the_result ()).the_fsm_2

(***********************************************************************)
(*** Hypothesis ********************************************************)
(***********************************************************************)

module Cofix : Hyp.HYP_TYPE = Hyp.Make (struct
    type t =
      { m : State.t
      ; n : State.t
      }

    let of_hty (sigma : Evd.evar_map) ((ty, tys) : Hyp.atomic_pair) : t =
      if Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_weak_sim ())
      then (
        let m : State.t =
          decode_state (get_encoding tys.(5)) (mfsm ()).states
        in
        let n : State.t =
          decode_state (get_encoding tys.(6)) (nfsm ()).states
        in
        { m; n })
      else raise (Hyp.Mebi_proof_Hypothesis_HTy (sigma, (ty, tys)))
    ;;
  end)

module Invertible : Hyp.HYP_TYPE = Hyp.MakeHyp (struct
    module HTy : Hyp.HTY_TYPE = Hyp.MakeHTy (struct
        type t =
          | Full
          | Layer

        let of_hty (sigma : Evd.evar_map) ((ty, tys) : Hyp.atomic_pair) : t =
          if Mebi_theories.is_var sigma tys.(2)
          then Full
          else if Mebi_theories.is_var sigma tys.(1)
          then Layer
          else raise (Hyp.Mebi_proof_Hypothesis_HTy (sigma, (ty, tys)))
        ;;
      end)

    type t =
      { kind : HTy.t
      ; tactic : unit Proofview.tactic
      }

    let of_hyp (sigma : Evd.evar_map) (h : Rocq_utils.hyp) : t =
      try
        { kind = HTy.of_hty sigma (Hyp.hyp_to_atomic sigma h)
        ; tactic = Mebi_tactics.do_inversion h
        }
      with
      | Hyp.Mebi_proof_Hypothesis_HTy (sigma, p) ->
        raise (Hyp.Mebi_proof_Hypothesis_Hyp (sigma, h, p))
    ;;
  end)

module TransOpt : Hyp.HYP_TYPE = Hyp.Make (struct
    type t = Transition_opt.t

    let try_decode (sigma : Evd.evar_map) (x : EConstr.t) : Enc.t option =
      if EConstr.isRel sigma x then None else get_encoding_opt x
    ;;

    let try_find_state
          (sigma : Evd.evar_map)
          (x : EConstr.t)
          (states : States.t)
      : State.t option
      =
      Option.map (fun (y : Enc.t) -> decode_state y states) (try_decode sigma x)
    ;;

    exception
      Mebi_proof_Hyp_TransOpt_CouldNotDecodeState of
        (Evd.evar_map * EConstr.t * States.t)

    let find_state (sigma : Evd.evar_map) (x : EConstr.t) (states : States.t)
      : State.t
      =
      match try_find_state sigma x states with
      | Some s -> s
      | None ->
        raise (Mebi_proof_Hyp_TransOpt_CouldNotDecodeState (sigma, x, states))
    ;;

    exception
      Mebi_proof_Hyp_TransOpt_CouldNotDecodeLabel of
        (Evd.evar_map * EConstr.t * Alphabet.t)

    let try_find_label
          (sigma : Evd.evar_map)
          (x : EConstr.t)
          (labels : Alphabet.t)
      : Label.t option
      =
      Option.map
        (fun (y : Enc.t) -> find_label_of_enc y labels)
        (try_decode sigma x)
    ;;

    let find_label (sigma : Evd.evar_map) (x : EConstr.t) (labels : Alphabet.t)
      : Label.t
      =
      match try_find_label sigma x labels with
      | Some s -> s
      | None ->
        raise (Mebi_proof_Hyp_TransOpt_CouldNotDecodeLabel (sigma, x, labels))
    ;;

    let of_hty (sigma : Evd.evar_map) ((ty, tys) : Hyp.atomic_pair) : t =
      try
        let states : States.t = (mfsm ()).states in
        let labels : Alphabet.t = (mfsm ()).alphabet in
        let from : State.t = find_state sigma tys.(0) states in
        let label : Label.t = find_label sigma tys.(1) labels in
        let goto : State.t option = try_find_state sigma tys.(2) states in
        let { annotations; constructor_trees; _ } : Action.t =
          get_action_labelled label (Edges.find (mfsm ()).edges from)
        in
        Transition_opt.create from label goto annotations constructor_trees
      with
      | Mebi_proof_Hyp_TransOpt_CouldNotDecodeState (sigma, x, states) ->
        raise (Hyp.Mebi_proof_Hypothesis_HTy (sigma, (ty, tys)))
    ;;
  end)

(***********************************************************************)
(*** Proof Tools *******************************************************)
(***********************************************************************)

(** precedence of hyps:
    - cofix
    - full invert
    - layer invert
    - transition *)
let hyp_is_something (sigma : Evd.evar_map) (h : Rocq_utils.hyp) : bool =
  try
    let p : Hyp.atomic_pair = Hyp.hyp_to_atomic sigma h in
    if Cofix.hty_is_a sigma p
    then true
    else if Invertible.hty_is_a sigma p
    then true
    else if TransOpt.hty_is_a sigma p
    then true
    else false
  with
  | Hyp.Mebi_proof_HypIsNot_Atomic _ -> false
;;

let hyps_is_essentially_empty (gl : Proofview.Goal.t) : bool =
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  Bool.not
    (List.for_all
       (fun (h : Rocq_utils.hyp) -> hyp_is_something sigma h)
       (Proofview.Goal.hyps gl))
;;

(** first checks if hypothesis are empty, and if they are not then it checks if [hyps_is_essentially_empty]
*)
let hyps_is_empty (gl : Proofview.Goal.t) : bool =
  if List.is_empty (Proofview.Goal.hyps gl)
  then true
  else hyps_is_essentially_empty gl
;;

(***********************************************************************)
(*** Handle Proof States ***********************************************)
(***********************************************************************)

exception Mebi_proof_NewProof of unit
exception Mebi_proof_NewWeakSim of unit

(** [handle_new_proof gl] checks if the [hyps] of [gl] are empty before moving to state [NewWeakSim]
*)
let rec handle_new_proof (gl : Proofview.Goal.t) : unit Proofview.tactic =
  if hyps_is_empty gl
  then (
    the_proof_state := NewWeakSim;
    handle_proof_state gl)
  else (
    Log.warning "New Proof: Expected Hypothesis to be empty";
    raise (Mebi_proof_NewProof ()))

and handle_new_weak_sim (gl : Proofview.Goal.t) : unit Proofview.tactic =
  do_nothing ()

and handle_new_cofix (gl : Proofview.Goal.t) : unit Proofview.tactic =
  do_nothing ()

and handle_new_transition
      (gl : Proofview.Goal.t)
      (mtransition : Transition_opt.t)
  : unit Proofview.tactic
  =
  do_nothing ()

and handle_goal_transition
      (gl : Proofview.Goal.t)
      (mtransition : Transition_opt.t)
  : unit Proofview.tactic
  =
  do_nothing ()

and handle_apply_constructors
      (gl : Proofview.Goal.t)
      (napplicable_constructors : PState.applicable_constructors)
  : unit Proofview.tactic
  =
  do_nothing ()

and handle_proof_state (gl : Proofview.Goal.t) : unit Proofview.tactic =
  match !the_proof_state with
  | NewProof -> handle_new_proof gl
  | NewWeakSim -> handle_new_weak_sim gl
  | NewCofix -> handle_new_cofix gl
  | NewTransition mtransition -> handle_new_transition gl mtransition
  | GoalTransition mtransition -> handle_goal_transition gl mtransition
  | ApplyConstructors napplicable_constructors ->
    handle_apply_constructors gl napplicable_constructors
;;

(***********************************************************************)
(*** Solve Proof *******************************************************)
(***********************************************************************)

let step () : unit Proofview.tactic =
  Mebi_theories.tactics
    [ Proofview.Goal.enter (fun gl -> handle_proof_state gl)
    ; Mebi_tactics.simplify_and_subst_all ()
    ]
;;

let solve (upper_bound : int) (pstate : Declare.Proof.t) : Declare.Proof.t =
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
      let pstate = Mebi_tactics.update_proof_by_tactic pstate (step ()) in
      iter_body (n - 1) pstate
  in
  let rem, pstate = iter_body upper_bound pstate in
  Log.notice
    (Printf.sprintf "Stopped after (%i) iterations." (upper_bound - rem));
  pstate
;;
