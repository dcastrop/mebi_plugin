open Logging
open Mebi_wrapper
open Model
module Hyp = Mebi_hypothesis

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
    | DetectState

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
    | DetectState -> "DetectState"
  ;;
end

let default_proof_state : PState.t = PState.NewProof
let the_proof_state : PState.t ref = ref default_proof_state
let reset_the_proof_state () : unit = the_proof_state := default_proof_state

(***********************************************************************)
(*** Bisimilarity Result ***********************************************)
(***********************************************************************)

let the_result () : Algorithms.Bisimilar.result =
  Algorithms.Bisimilar.get_the_result ()
;;

let mfsm () : Fsm.t = (the_result ()).the_fsm_1
let nfsm () : Fsm.t = (the_result ()).the_fsm_2
let the_bisim_states () : Partition.t = (the_result ()).bisim_states

(***********************************************************************)
(*** Proof Tools *******************************************************)
(***********************************************************************)

exception Mebi_proof_StatesNotBisimilar of (State.t * State.t * Partition.t)

let get_constructor_annotations
      (gl : Proofview.Goal.t)
      (mgoto : State.t)
      (nfrom : State.t)
      (nlabel : Label.t)
      (ngoto : State.t)
  : PState.applicable_constructors
  =
  let msimilar : States.t =
    Model.get_bisim_states mgoto (the_bisim_states ())
  in
  if States.mem ngoto msimilar
  then (
    let nactions : States.t Actions.t = Edges.find (nfsm ()).edges nfrom in
    let naction : Action.t = Model.get_action_labelled nlabel nactions in
    let annotation : Note.annotation = Model.get_shortest_annotation naction in
    { annotation; tactics = None })
  else raise (Mebi_proof_StatesNotBisimilar (mgoto, ngoto, the_bisim_states ()))
;;

(***********************************************************************)

exception
  Mebi_proof_TyDoesNotMatchTheories of (Evd.evar_map * Rocq_utils.kind_pair)

let do_nothing () : unit Proofview.tactic =
  Log.warning "mebi_proof, case just does nothing";
  Proofview.tclUNIT ()
;;

let do_simplify (gl : Proofview.Goal.t) : unit Proofview.tactic =
  Mebi_tactics.simplify_and_subst_all ~gl ()
;;

let do_rt1n_refl (gl : Proofview.Goal.t) : unit Proofview.tactic =
  Mebi_theories.tactics
    [ Mebi_tactics.apply ~gl (Mebi_theories.c_rt1n_refl ()); do_simplify gl ]
;;

let do_solve_cofix (gl : Proofview.Goal.t) : unit Proofview.tactic =
  the_proof_state := DetectState;
  Auto.gen_trivial ~debug:Hints.Info [] None
;;

let do_weak_silent_transition
      (gl : Proofview.Goal.t)
      (from : State.t)
      (goto : State.t)
  : unit Proofview.tactic
  =
  the_proof_state := NewWeakSim;
  Mebi_theories.tactics
    [ Mebi_tactics.apply ~gl (Mebi_theories.c_wk_none ())
    ; Mebi_tactics.unfold_econstr gl (Mebi_theories.c_silent ())
    ; (if State.equal from goto
       then (
         Log.notice "apply rt1n_refl.";
         Mebi_tactics.apply ~gl (Mebi_theories.c_rt1n_refl ()))
       else (
         Log.notice "apply rt1n_trans.";
         Mebi_tactics.apply ~gl (Mebi_theories.c_rt1n_trans ())))
    ]
;;

let do_weak_visible_transition
      (gl : Proofview.Goal.t)
      (mgoto : State.t)
      (nfrom : State.t)
      (nlabel : Label.t)
      (ngoto : State.t)
  : unit Proofview.tactic
  =
  the_proof_state
  := ApplyConstructors (get_constructor_annotations gl mgoto nfrom nlabel ngoto);
  Mebi_theories.tactics
    [ Mebi_tactics.eapply ~gl (Mebi_theories.c_wk_some ())
    ; Mebi_tactics.unfold_econstr gl (Mebi_theories.c_silent ())
    ]
;;

(***********************************************************************)

let try_decode (sigma : Evd.evar_map) (x : EConstr.t) : Enc.t option =
  if EConstr.isRel sigma x then None else get_encoding_opt x
;;

let typ_is_weak_sim (sigma : Evd.evar_map) ((ty, _) : Rocq_utils.kind_pair)
  : bool
  =
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_weak_sim ())
;;

let typ_is_weak_transition
      (sigma : Evd.evar_map)
      ((ty, _) : Rocq_utils.kind_pair)
  : bool
  =
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_weak ())
;;

let typ_is_silent_transition
      (sigma : Evd.evar_map)
      ((ty, _) : Rocq_utils.kind_pair)
  : bool
  =
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_silent ())
;;

let typ_is_silent1_transition
      (sigma : Evd.evar_map)
      ((ty, _) : Rocq_utils.kind_pair)
  : bool
  =
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_silent1 ())
;;

let typ_is_lts_transition
      (sigma : Evd.evar_map)
      ((ty, _) : Rocq_utils.kind_pair)
  : bool
  =
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_LTS ())
;;

(***********************************************************************)

let try_find_state (sigma : Evd.evar_map) (x : EConstr.t) (states : States.t)
  : State.t option
  =
  Option.map (fun (y : Enc.t) -> decode_state y states) (try_decode sigma x)
;;

exception
  Mebi_proof_CouldNotDecodeState of (Evd.evar_map * EConstr.t * States.t)

let find_state (sigma : Evd.evar_map) (x : EConstr.t) (states : States.t)
  : State.t
  =
  match try_find_state sigma x states with
  | Some s -> s
  | None -> raise (Mebi_proof_CouldNotDecodeState (sigma, x, states))
;;

(***********************************************************************)

exception
  Mebi_proof_CouldNotDecodeLabel of (Evd.evar_map * EConstr.t * Alphabet.t)

let try_find_label (sigma : Evd.evar_map) (x : EConstr.t) (labels : Alphabet.t)
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
  | None -> raise (Mebi_proof_CouldNotDecodeLabel (sigma, x, labels))
;;

(***********************************************************************)

exception
  Mebi_proof_CouldNotDecodeTransition of (Evd.evar_map * EConstr.t * Fsm.t)

let get_transition
      (sigma : Evd.evar_map)
      (fromty : EConstr.t)
      (labelty : EConstr.t)
      (gototy : EConstr.t)
      (fsm : Fsm.t)
  : Transition_opt.t
  =
  try
    let states : States.t = fsm.states in
    let labels : Alphabet.t = fsm.alphabet in
    let from : State.t = find_state sigma fromty states in
    let label : Label.t = find_label sigma labelty labels in
    let goto : State.t option = try_find_state sigma gototy states in
    let { annotations; constructor_trees; _ } : Action.t =
      get_action_labelled label (Edges.find fsm.edges from)
    in
    Transition_opt.create from label goto annotations constructor_trees
  with
  | Mebi_proof_CouldNotDecodeState (sigma, ty, states) ->
    raise (Mebi_proof_CouldNotDecodeTransition (sigma, ty, fsm))
  | Mebi_proof_CouldNotDecodeLabel (sigma, ty, alphabet) ->
    raise (Mebi_proof_CouldNotDecodeTransition (sigma, ty, fsm))
;;

let get_lts_transition
      (sigma : Evd.evar_map)
      (fsm : Fsm.t)
      ((ty, tys) : Rocq_utils.kind_pair)
  : Transition_opt.t
  =
  if typ_is_lts_transition sigma (ty, tys)
  then get_transition sigma tys.(0) tys.(1) tys.(2) fsm
  else raise (Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)))
;;

let get_weak_transition
      (sigma : Evd.evar_map)
      (fsm : Fsm.t)
      ((ty, tys) : Rocq_utils.kind_pair)
  : Transition_opt.t
  =
  if typ_is_weak_transition sigma (ty, tys)
  then get_transition sigma tys.(3) tys.(4) tys.(5) fsm
  else raise (Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)))
;;

let get_silent_transition
      (sigma : Evd.evar_map)
      (fsm : Fsm.t)
      ((ty, tys) : Rocq_utils.kind_pair)
  : Transition_opt.t
  =
  if typ_is_silent_transition sigma (ty, tys)
  then get_transition sigma tys.(2) tys.(4) tys.(3) fsm
  else raise (Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)))
;;

let get_silent1_transition
      (sigma : Evd.evar_map)
      (fsm : Fsm.t)
      ((ty, tys) : Rocq_utils.kind_pair)
  : Transition_opt.t
  =
  if typ_is_silent1_transition sigma (ty, tys)
  then get_transition sigma tys.(2) tys.(4) tys.(3) fsm
  else raise (Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)))
;;

(***********************************************************************)

exception Mebi_proof_CouldNotGetStateM of (Evd.evar_map * Rocq_utils.kind_pair)

let weak_sim_get_m_state
      (sigma : Evd.evar_map)
      ((ty, tys) : Rocq_utils.kind_pair)
  : State.t
  =
  if typ_is_weak_sim sigma (ty, tys)
  then (
    try find_state sigma tys.(0) (mfsm ()).states with
    | Mebi_proof_CouldNotDecodeState (sigma, statety, states) ->
      raise (Mebi_proof_CouldNotGetStateM (sigma, (ty, tys))))
  else raise (Mebi_proof_CouldNotGetStateM (sigma, (ty, tys)))
;;

(***********************************************************************)
(*** Hypothesis ********************************************************)
(***********************************************************************)

module Cofix : Hyp.HYP_TYPE = Hyp.Make (struct
    type t =
      { m : State.t
      ; n : State.t
      }

    let of_hty (sigma : Evd.evar_map) ((ty, tys) : Rocq_utils.kind_pair) : t =
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

        let of_hty (sigma : Evd.evar_map) ((ty, tys) : Rocq_utils.kind_pair) : t
          =
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
        { kind = HTy.of_hty sigma (Rocq_utils.hyp_to_atomic sigma h)
        ; tactic = Mebi_tactics.do_inversion h
        }
      with
      | Hyp.Mebi_proof_Hypothesis_HTy (sigma, p) ->
        raise (Hyp.Mebi_proof_Hypothesis_Hyp (sigma, h, p))
    ;;
  end)

module TransOpt : Hyp.HYP_TYPE = Hyp.Make (struct
    type t = Transition_opt.t

    let of_hty (sigma : Evd.evar_map) ((ty, tys) : Rocq_utils.kind_pair) : t =
      try get_lts_transition sigma (mfsm ()) (ty, tys) with
      | Mebi_proof_CouldNotDecodeTransition (sigma, x, fsm) ->
        raise (Hyp.Mebi_proof_Hypothesis_HTy (sigma, (ty, tys)))
      | Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)) ->
        raise (Hyp.Mebi_proof_Hypothesis_HTy (sigma, (ty, tys)))
    ;;
  end)

(** precedence of hyps:
    - cofix
    - full invert
    - layer invert
    - transition *)
let hyp_is_something (sigma : Evd.evar_map) (h : Rocq_utils.hyp) : bool =
  try
    let p : Rocq_utils.kind_pair = Rocq_utils.hyp_to_atomic sigma h in
    if Cofix.hty_is_a sigma p
    then true
    else if Invertible.hty_is_a sigma p
    then true
    else if TransOpt.hty_is_a sigma p
    then true
    else false
  with
  | Rocq_utils.Rocq_utils_HypIsNot_Atomic _ -> false
;;

(** is [true] if none of the hyps appear to be mid-way through a proof. *)
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
(*** Conclusion ********************************************************)
(***********************************************************************)

(* precedence of concl:
   - n-transition
   - new weak sim
   - exists n2 *)

exception Mebi_proof_ConclIsNot_Exists of (Evd.evar_map * Rocq_utils.kind_pair)

let concl_get_eexists (sigma : Evd.evar_map) ((ty, tys) : Rocq_utils.kind_pair)
  : Transition_opt.t * State.t
  =
  let _, _, constr = Rocq_utils.econstr_to_lambda sigma tys.(1) in
  let _, apptys = Rocq_utils.econstr_to_app sigma constr in
  match Array.to_list apptys with
  | [ wk_trans; wk_sim ] ->
    (try
       let nt : Transition_opt.t =
         Rocq_utils.econstr_to_atomic sigma wk_trans
         |> get_weak_transition sigma (nfsm ())
       in
       let mstate : State.t =
         Rocq_utils.econstr_to_atomic sigma wk_sim |> weak_sim_get_m_state sigma
       in
       nt, mstate
     with
     | Mebi_proof_CouldNotDecodeTransition (sigma, x, fsm) ->
       raise (Mebi_proof_ConclIsNot_Exists (sigma, (ty, tys)))
     | Mebi_proof_CouldNotGetStateM (sigma, (ty, tys)) ->
       raise (Mebi_proof_ConclIsNot_Exists (sigma, (ty, tys)))
     | Rocq_utils.Rocq_utils_EConstrIsNot_Atomic (sigma, x, k) ->
       raise (Mebi_proof_ConclIsNot_Exists (sigma, (ty, tys)))
     | Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)) ->
       raise (Mebi_proof_ConclIsNot_Exists (sigma, (ty, tys))))
  | _ -> raise (Mebi_proof_ConclIsNot_Exists (sigma, (ty, tys)))
;;

let hyps_has_cofix (sigma : Evd.evar_map) (concl : EConstr.t)
  : Rocq_utils.hyp list -> bool
  =
  List.exists (fun (h : Rocq_utils.hyp) ->
    Mebi_setup.Eq.econstr sigma concl (Context.Named.Declaration.get_type h))
;;

(***********************************************************************)
(*** Handle Proof States ***********************************************)
(***********************************************************************)

exception Mebi_proof_NewProof of unit
exception Mebi_proof_NewWeakSim of unit
exception Mebi_proof_NewTransition of unit
exception Mebi_proof_GoalTransition of unit
exception Mebi_proof_ApplyConstructors of unit

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
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let the_concl : EConstr.t = Proofview.Goal.concl gl in
  let the_hyps : Rocq_utils.hyp list = Proofview.Goal.hyps gl in
  let concltyp : Rocq_utils.kind_pair =
    Rocq_utils.econstr_to_atomic sigma the_concl
  in
  (* NOTE: if concl is [weak_sim] and hyp has corresponding [cofix] then [auto]. *)
  if typ_is_weak_sim sigma concltyp && hyps_has_cofix sigma the_concl the_hyps
  then do_solve_cofix gl
  (* NOTE: else, we continue *)
  else (
    (* TODO: new_weak_sim > exists *)
    try
      (* TODO: new_weak_sim *)
      (* ... *)
      (* NOTE: eexists*)
      let ntransition, mstate =
        Rocq_utils.econstr_to_atomic sigma the_concl |> concl_get_eexists sigma
      in
      the_proof_state := NewCofix;
      handle_proof_state gl
    with
    | Mebi_proof_ConclIsNot_Exists _ -> raise (Mebi_proof_NewWeakSim ()))

and handle_new_cofix (gl : Proofview.Goal.t) : unit Proofview.tactic =
  do_nothing ()

and handle_new_transition
      (gl : Proofview.Goal.t)
      (mtransition : Transition_opt.t)
  : unit Proofview.tactic
  =
  try do_nothing () with _ -> raise (Mebi_proof_NewTransition ())

and handle_goal_transition (gl : Proofview.Goal.t) (mtrans : Transition_opt.t)
  : unit Proofview.tactic
  =
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  try
    let { from = nfrom; label = nlabel; goto = ngoto; _ } : Transition_opt.t =
      Proofview.Goal.concl gl
      |> Rocq_utils.econstr_to_atomic sigma
      |> get_weak_transition sigma (nfsm ())
    in
    match mtrans.goto, ngoto with
    | Some _, Some ngoto ->
      (* ? TODO: check if this should be [mtrans.label] *)
      if Label.is_silent nlabel
      then do_weak_silent_transition gl nfrom ngoto
      else do_weak_visible_transition gl mtrans.from nfrom nlabel ngoto
    | _, _ -> raise (Mebi_proof_GoalTransition ())
  with
  | Mebi_proof_CouldNotDecodeTransition (sigma, x, fsm) ->
    raise (Mebi_proof_GoalTransition ())

and handle_apply_constructors
      (gl : Proofview.Goal.t)
      (napplicable_constructors : PState.applicable_constructors)
  : unit Proofview.tactic
  =
  try do_nothing () with _ -> raise (Mebi_proof_ApplyConstructors ())

and handle_proof_state (gl : Proofview.Goal.t) : unit Proofview.tactic =
  match !the_proof_state with
  | NewProof -> handle_new_proof gl
  | NewWeakSim -> handle_new_weak_sim gl
  | NewCofix -> handle_new_cofix gl
  | NewTransition mtransition -> handle_new_transition gl mtransition
  | GoalTransition mtransition -> handle_goal_transition gl mtransition
  | ApplyConstructors napplicable_constructors ->
    handle_apply_constructors gl napplicable_constructors
  | DetectState -> detect_proof_state gl

and detect_proof_state (gl : Proofview.Goal.t) : unit Proofview.tactic =
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let the_concl : EConstr.t = Proofview.Goal.concl gl in
  let the_hyps : Rocq_utils.hyp list = Proofview.Goal.hyps gl in
  let concltyp : Rocq_utils.kind_pair =
    Rocq_utils.econstr_to_atomic sigma the_concl
  in
  if typ_is_weak_sim sigma concltyp && hyps_has_cofix sigma the_concl the_hyps
  then do_solve_cofix gl
  else do_nothing ()
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
