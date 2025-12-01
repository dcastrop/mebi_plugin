open Logging
open Mebi_wrapper
open Model
module Hyp = Mebi_hypothesis

let econstr_to_string (gl : Proofview.Goal.t) : EConstr.t -> string =
  Rocq_utils.Strfy.econstr (Proofview.Goal.env gl) (Proofview.Goal.sigma gl)
;;

let hyp_to_string (gl : Proofview.Goal.t) : Rocq_utils.hyp -> string =
  Rocq_utils.Strfy.hyp (Proofview.Goal.env gl) (Proofview.Goal.sigma gl)
;;

let hyps_to_string (gl : Proofview.Goal.t) : string =
  match Proofview.Goal.hyps gl with
  | [] -> "[ ] (Hyps: empty)"
  | h :: t ->
    List.fold_left
      (fun (acc : string) (x : Rocq_utils.hyp) ->
        Printf.sprintf "%s\n%s" acc (hyp_to_string gl x))
      (hyp_to_string gl h)
      t
;;

let concl_to_string (gl : Proofview.Goal.t) : string =
  econstr_to_string gl (Proofview.Goal.concl gl)
;;

let log_trace_concl (gl : Proofview.Goal.t) (prefix : string) : unit =
  Log.trace (Printf.sprintf "%s%s" (Utils.prefix prefix) (concl_to_string gl))
;;

let log_trace_hyps (gl : Proofview.Goal.t) (prefix : string) : unit =
  Log.trace (Printf.sprintf "%s%s" (Utils.prefix prefix) (hyps_to_string gl))
;;

(***********************************************************************)
(*** Proof State *******************************************************)
(***********************************************************************)

type tactic =
  { msg : string option
  ; x : unit Proofview.tactic
  }

let tactic ?(msg : string option) (x : unit Proofview.tactic) : tactic =
  { msg; x }
;;

let tactic_chain : tactic list -> tactic = function
  | [] -> tactic (Proofview.tclUNIT ())
  | h :: tl ->
    let f : string option * string option -> string option = function
      | None, None -> None
      | Some xmsg, None -> Some xmsg
      | None, Some ymsg -> Some ymsg
      | Some xmsg, Some ymsg -> Some (Printf.sprintf "%s; %s" xmsg ymsg)
    in
    List.fold_left
      (fun { msg = xmsg; x } { msg = ymsg; x = y } ->
        { msg = f (xmsg, ymsg); x = Mebi_theories.tactics [ x; y ] })
      h
      tl
;;

module PState = struct
  type t =
    | NewProof
    | NewWeakSim
    | NewCofix
    | GoalTransition of Transition_opt.t
    | ApplyConstructors of applicable_constructors
    | DetectState

  and applicable_constructors =
    { annotation : Note.annotation
    ; tactics : tactic list option
    }

  (* and tactic_to_apply = unit -> unit Proofview.tactic *)

  let empty_tactics : tactic list option -> bool =
    Log.trace "Mebi_proof.PState.empty_tactics";
    function None -> true | Some [] -> true | Some (_ :: _) -> false
  ;;

  let finished_applying_constructors : applicable_constructors -> bool =
    Log.trace "Mebi_proof.PState.finished_applying_constructors";
    function
    | { annotation = []; tactics = Some (_ :: _) } -> true
    | { annotation = []; tactics = Some [] } -> true
    | { annotation = []; tactics = None } -> true
    | _ -> false
  ;;

  let to_string ?(short : bool = true) : t -> string =
    Log.trace "Mebi_proof.PState.to_string";
    function
    | NewProof -> "NewProof"
    | NewWeakSim -> "NewWeakSim"
    | NewCofix -> "NewCofix"
    | GoalTransition transition ->
      Printf.sprintf
        "GoalTransition%s"
        (if short
         then ""
         else Printf.sprintf ":\n%s" (Transition_opt.to_string transition))
    | ApplyConstructors { annotation; tactics } ->
      Printf.sprintf
        "ApplyConstructors%s"
        (if short
         then ""
         else
           Printf.sprintf
             ":\nNote:\n%s\nTactics:\n%s"
             (Note.annotation_to_string annotation)
             (Option.cata
                (fun xs ->
                  List.fold_left
                    (fun acc { msg; _ } ->
                      Printf.sprintf
                        "%s; %s"
                        acc
                        (Option.cata (fun y -> y) "None" msg))
                    ""
                    xs)
                "None"
                tactics))
    | DetectState -> "DetectState"
  ;;
end

let default_proof_state : PState.t = PState.NewProof
let the_old_proof_state : PState.t ref = ref default_proof_state
let the_proof_state : PState.t ref = ref default_proof_state
let reset_the_proof_state () : unit = the_proof_state := default_proof_state

let debug_proof_state () : unit =
  Log.notice
    (Printf.sprintf
       "%s => %s"
       (PState.to_string !the_old_proof_state)
       (PState.to_string !the_proof_state))
;;

let get_tactic ?(state : bool = true) : tactic -> unit Proofview.tactic
  = function
  | { msg = None; x } -> x
  | { msg = Some msg; x } ->
    if state then debug_proof_state ();
    the_old_proof_state := !the_proof_state;
    Log.notice (Printf.sprintf "%s." msg);
    x
;;

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
(*** Warning Messages **************************************************)
(***********************************************************************)

let _warn_model_action_hasnoannotations (naction : Action.t) : unit =
  Log.warning
    (Printf.sprintf
       "Model_Action_HasNoAnnotations:\n%s"
       (Action.to_string naction))
;;

(***********************************************************************)
(*** Proof Tools *******************************************************)
(***********************************************************************)

exception Mebi_proof_StatesNotBisimilar of (State.t * State.t * Partition.t)

let are_states_bisimilar (m : State.t) (n : State.t) : bool =
  Log.trace "Mebi_proof.are_states_bisimilar";
  Model.get_bisim_states m (the_bisim_states ()) |> States.mem n
;;

let get_naction (nfrom : State.t) (nlabel : Label.t) : Action.t =
  Log.trace "Mebi_proof.get_naction";
  Edges.find (nfsm ()).edges nfrom |> Model.get_action_labelled nlabel
;;

let get_constructor_annotation (nfrom : State.t) (nlabel : Label.t)
  : Note.annotation
  =
  Log.trace "Mebi_proof.get_constructor_annotation";
  get_naction nfrom nlabel |> Model.get_shortest_annotation nfrom
;;

let get_annotation_constructor (nfrom : State.t) (nlabel : Label.t)
  : Tree.node list
  =
  Log.trace "Mebi_proof.get_annotation_constructor";
  get_naction nfrom nlabel |> Model.get_shortest_constructor
;;

(***********************************************************************)

exception
  Mebi_proof_TyDoesNotMatchTheories of (Evd.evar_map * Rocq_utils.kind_pair)

let do_nothing () : tactic =
  Log.trace "Mebi_proof.do_nothing";
  Log.warning "mebi_proof, case just does nothing";
  tactic ~msg:"((do_nothing))" (Proofview.tclUNIT ())
;;

let get_all_non_cofix (gl : Proofview.Goal.t) : Names.Id.Set.t =
  Names.Id.Set.filter
    (fun (x : Names.Id.t) -> Bool.not (Mebi_theories.is_cofix x))
    (Context.Named.to_vars (Proofview.Goal.hyps gl))
;;

let clear_old_hyps (gl : Proofview.Goal.t) : tactic =
  tactic
    ~msg:"(clear_old_hyps)"
    (Tactics.clear (Names.Id.Set.to_list (get_all_non_cofix gl)))
;;

let do_cofix (gl : Proofview.Goal.t) : tactic =
  tactic ~msg:"(do cofix)" (Mebi_tactics.cofix gl)
;;

let do_apply_In_sim (gl : Proofview.Goal.t) : tactic =
  tactic
    ~msg:"apply In_sim"
    (Mebi_tactics.apply ~gl (Mebi_theories.c_In_sim ()))
;;

let do_apply_Pack_sim (gl : Proofview.Goal.t) : tactic =
  tactic
    ~msg:"apply Pack_sim"
    (Mebi_tactics.apply ~gl (Mebi_theories.c_Pack_sim ()))
;;

let do_intros_all () : tactic =
  tactic ~msg:"intros" (Mebi_tactics.intros_all ())
;;

let do_new_cofix (gl : Proofview.Goal.t) : tactic =
  tactic_chain
    [ do_cofix gl
    ; do_apply_In_sim gl
    ; do_apply_Pack_sim gl
    ; do_intros_all ()
    ; clear_old_hyps gl
    ]
;;

let do_inversion (h : Rocq_utils.hyp) : tactic =
  tactic ~msg:"(do_inversion)" (Mebi_tactics.do_inversion h)
;;

let do_simplify (gl : Proofview.Goal.t) : tactic =
  tactic ~msg:"(do_simplify)" (Mebi_tactics.simplify_and_subst_all ~gl ())
;;

let do_apply_rt1n_refl (gl : Proofview.Goal.t) : tactic =
  tactic
    ~msg:"apply rt1n_refl"
    (Mebi_theories.tactics
       [ Mebi_tactics.apply ~gl (Mebi_theories.c_rt1n_refl ()) ])
;;

let do_eapply_rt1n_refl (gl : Proofview.Goal.t) : tactic =
  tactic
    ~msg:"eapply rt1n_refl"
    (Mebi_theories.tactics
       [ Mebi_tactics.eapply ~gl (Mebi_theories.c_rt1n_refl ()) ])
;;

let _do_apply_rt1n_trans (gl : Proofview.Goal.t) : tactic =
  tactic
    ~msg:"apply rt1n_trans"
    (Mebi_theories.tactics
       [ Mebi_tactics.apply ~gl (Mebi_theories.c_rt1n_trans ()) ])
;;

let do_eapply_rt1n_trans (gl : Proofview.Goal.t) : tactic =
  tactic
    ~msg:"eapply rt1n_trans"
    (Mebi_theories.tactics
       [ Mebi_tactics.eapply ~gl (Mebi_theories.c_rt1n_trans ()) ])
;;

let do_rt1n_via (gl : Proofview.Goal.t) (via : Label.t) : tactic =
  tactic_chain
    [ (if Label.is_silent via
       then do_eapply_rt1n_trans gl
       else do_eapply_rt1n_refl gl)
    ; do_simplify gl
    ]
;;

let do_solve_cofix (gl : Proofview.Goal.t) : tactic =
  Log.trace "Mebi_proof.do_solve_cofix";
  (* NOTE: update [the_proof_state] *)
  the_proof_state := DetectState;
  tactic ~msg:"(do_solve)" (Auto.gen_trivial ~debug:Hints.Info [] None)
;;

let do_apply_wk_none (gl : Proofview.Goal.t) : tactic =
  tactic
    ~msg:"apply wk_none"
    (Mebi_tactics.apply ~gl (Mebi_theories.c_wk_none ()))
;;

let do_unfold_silent (gl : Proofview.Goal.t) : tactic =
  tactic
    ~msg:"unfold silent"
    (Mebi_tactics.unfold_econstr gl (Mebi_theories.c_silent ()))
;;

let do_eapply_wk_some (gl : Proofview.Goal.t) : tactic =
  tactic
    ~msg:"eapply wk_some"
    (Mebi_tactics.eapply ~gl (Mebi_theories.c_wk_some ()))
;;

let do_constructor_transition
      (gl : Proofview.Goal.t)
      (nfrom : State.t)
      (nlabel : Label.t)
      (htactic : tactic)
  : tactic
  =
  let annotation = get_constructor_annotation nfrom nlabel in
  (* NOTE: update [the_proof_state] *)
  the_proof_state := ApplyConstructors { annotation; tactics = None };
  tactic_chain [ htactic; do_unfold_silent gl ]
;;

let do_refl_none (gl : Proofview.Goal.t) : tactic =
  (* NOTE: update [the_proof_state] *)
  the_proof_state := NewWeakSim;
  tactic_chain
    [ do_apply_wk_none gl; do_unfold_silent gl; do_apply_rt1n_refl gl ]
;;

(* let do_weak_silent_transition
   (gl : Proofview.Goal.t)
   (from : State.t)
   (goto : State.t)
   : tactic
   =
   Log.trace "Mebi_proof.do_weak_silent_transition";
   if State.equal from goto
   then (
   (* NOTE: update [the_proof_state] *)
   the_proof_state := NewWeakSim;
   tactic_chain
   [ do_apply_wk_none gl; do_unfold_silent gl; do_apply_rt1n_refl gl ])
   else
   do_constructor_transition gl nfrom nlabel (do_apply_wk_none gl)
   (* do_eapply_rt1n_trans gl *)
   ;; *)

(* let do_weak_visible_transition
   (gl : Proofview.Goal.t)
   (mfrom : State.t)
   (nfrom : State.t)
   (nlabel : Label.t)
   (ngoto : State.t)
   : tactic
   =
   Log.trace "Mebi_proof.do_weak_visible_transition";
   if are_states_bisimilar mfrom ngoto
   then do_constructor_transition gl nfrom nlabel (do_eapply_wk_some gl)
   else raise (Mebi_proof_StatesNotBisimilar (mfrom, ngoto, the_bisim_states ()))
   ;; *)

let get_constructor_tactic ((enc, index) : Tree.node) : tactic =
  Log.trace "Mebi_proof.get_constructor_tactic";
  let index : int = index + 1 in
  tactic
    ~msg:(Printf.sprintf "constructor %i" index)
    (Tactics.one_constructor index Tactypes.NoBindings)
;;

let rec get_constructor_tactics_to_apply : Tree.node list -> tactic list =
  Log.trace "Mebi_proof.get_constructor_tactics_to_apply";
  function
  | [] -> []
  | h :: tl -> get_constructor_tactic h :: get_constructor_tactics_to_apply tl
;;

let do_build_constructor_tactics (gl : Proofview.Goal.t)
  : Note.annotation -> tactic
  =
  Log.trace "Mebi_proof.do_build_constructor_tactics";
  function
  | [] -> do_nothing ()
  | { from; via } :: annotation ->
    let constructors : Tree.node list = get_annotation_constructor from via in
    let tactics : tactic list = get_constructor_tactics_to_apply constructors in
    (* NOTE: update [the_proof_state] *)
    the_proof_state := ApplyConstructors { annotation; tactics = Some tactics };
    do_rt1n_via gl via
;;

let do_constructor_tactic (gl : Proofview.Goal.t) (annotation : Note.annotation)
  : tactic list option -> tactic
  = function
  | None -> do_build_constructor_tactics gl annotation
  | Some [] ->
    tactic_chain [ do_simplify gl; do_build_constructor_tactics gl annotation ]
  | Some (h :: tl) ->
    the_proof_state := ApplyConstructors { annotation; tactics = Some tl };
    h
;;

(***********************************************************************)

let try_decode (sigma : Evd.evar_map) (x : EConstr.t) : Enc.t option =
  Log.trace "Mebi_proof.try_decode";
  if EConstr.isRel sigma x then None else get_encoding_opt x
;;

let typ_is_exists (sigma : Evd.evar_map) ((ty, _) : Rocq_utils.kind_pair) : bool
  =
  Log.trace "Mebi_proof.typ_is_exists";
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_ex ())
;;

let typ_is_weak_sim (sigma : Evd.evar_map) ((ty, _) : Rocq_utils.kind_pair)
  : bool
  =
  Log.trace "Mebi_proof.typ_is_weak_sim";
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_weak_sim ())
;;

let typ_is_weak_transition
      (sigma : Evd.evar_map)
      ((ty, _) : Rocq_utils.kind_pair)
  : bool
  =
  Log.trace "Mebi_proof.typ_is_weak_transition";
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_weak ())
;;

let typ_is_silent_transition
      (sigma : Evd.evar_map)
      ((ty, _) : Rocq_utils.kind_pair)
  : bool
  =
  Log.trace "Mebi_proof.typ_is_silent_transition";
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_silent ())
;;

let typ_is_silent1_transition
      (sigma : Evd.evar_map)
      ((ty, _) : Rocq_utils.kind_pair)
  : bool
  =
  Log.trace "Mebi_proof.typ_is_silent1_transition";
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_silent1 ())
;;

let typ_is_lts_transition
      (sigma : Evd.evar_map)
      ((ty, _) : Rocq_utils.kind_pair)
  : bool
  =
  Log.trace "Mebi_proof.typ_is_lts_transition";
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_LTS ())
;;

(***********************************************************************)

let try_find_state (sigma : Evd.evar_map) (x : EConstr.t) (states : States.t)
  : State.t option
  =
  Log.trace "Mebi_proof.try_find_state";
  Option.map (fun (y : Enc.t) -> decode_state y states) (try_decode sigma x)
;;

exception
  Mebi_proof_CouldNotDecodeState of (Evd.evar_map * EConstr.t * States.t)

let find_state (sigma : Evd.evar_map) (x : EConstr.t) (states : States.t)
  : State.t
  =
  Log.trace "Mebi_proof.find_state";
  try
    match try_find_state sigma x states with
    | Some s -> s
    | None -> raise (Mebi_proof_CouldNotDecodeState (sigma, x, states))
  with
  | Model_CannotDecodeState (enc, states) ->
    Log.warning
      (Printf.sprintf
         "Mebi_proof.find_state, Model_CannotDecodeState:\nenc: %s\nstates: %s"
         (Enc.to_string enc)
         (states_to_string states));
    raise (Mebi_proof_CouldNotDecodeState (sigma, x, states))
;;

(***********************************************************************)

exception
  Mebi_proof_CouldNotDecodeLabel of (Evd.evar_map * EConstr.t * Alphabet.t)

let try_find_label (sigma : Evd.evar_map) (x : EConstr.t) (labels : Alphabet.t)
  : Label.t option
  =
  Log.trace "Mebi_proof.try_find_label";
  Option.map
    (fun (y : Enc.t) -> find_label_of_enc y labels)
    (try_decode sigma x)
;;

let find_label (sigma : Evd.evar_map) (x : EConstr.t) (labels : Alphabet.t)
  : Label.t
  =
  Log.trace "Mebi_proof.find_label";
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
  Log.trace "Mebi_proof.get_transition";
  try
    let states : States.t = fsm.states in
    let labels : Alphabet.t = fsm.alphabet in
    Log.debug "A";
    let from : State.t = find_state sigma fromty states in
    Log.debug (Printf.sprintf "From: %s" (State.to_string from));
    let label : Label.t = find_label sigma labelty labels in
    Log.debug (Printf.sprintf "Label: %s" (Label.to_string label));
    let goto : State.t option = try_find_state sigma gototy states in
    Log.debug
      (Printf.sprintf
         "Goto: %s"
         (Option.cata (fun x -> State.to_string x) "None" goto));
    Actions.iter
      (fun x y ->
        Log.debug
          (Printf.sprintf
             "Action:\n%s\nGoto:\n%s"
             (Action.to_string x)
             (states_to_string y)))
      (Edges.find fsm.edges from);
    let { annotations; constructor_trees; _ } : Action.t =
      (* TODO: [get_action_labelled] does not take into account saturated actions? Or, perhaps the saturated action label isnt updated (and maybe annotations not correctly updated?) *)
      get_action_labelled label (Edges.find fsm.edges from)
    in
    Log.debug "F";
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
  Log.trace "Mebi_proof.get_lts_transition";
  if typ_is_lts_transition sigma (ty, tys)
  then get_transition sigma tys.(0) tys.(1) tys.(2) fsm
  else raise (Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)))
;;

exception
  Mebi_proof_CouldNotFindHypTransition of
    (Evd.evar_map * Fsm.t * Rocq_utils.hyp list)

let get_hyp_transition
      (* (sigma : Evd.evar_map) *)
        (gl : Proofview.Goal.t)
      (fsm : Fsm.t)
      (hyps : Rocq_utils.hyp list)
  : Transition_opt.t
  =
  Log.trace "Mebi_proof.get_hyp_lts_transition";
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let hyp_transitions : Transition_opt.t list =
    List.filter_map
      (fun (x : Rocq_utils.hyp) ->
        try
          let ty, tys = Rocq_utils.hyp_to_atomic sigma x in
          let t : Transition_opt.t =
            get_transition sigma tys.(0) tys.(1) tys.(2) fsm
            (* get_lts_transition sigma fsm (Rocq_utils.hyp_to_atomic sigma x) *)
          in
          Some t
        with
        | Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)) -> None
        | Mebi_proof_CouldNotDecodeTransition (sigma, y, fsm) -> None)
      (* hyps *)
      (Proofview.Goal.hyps gl)
  in
  match hyp_transitions with
  | [] -> raise (Mebi_proof_CouldNotFindHypTransition (sigma, fsm, hyps))
  | h :: [] -> h
  | h :: _ ->
    Log.warning
      (Printf.sprintf
         "Mebi_proof.get_hyp_lts_transition, multiple transitions found (%i)"
         (List.length hyp_transitions));
    h
;;

let get_weak_transition
      (sigma : Evd.evar_map)
      (fsm : Fsm.t)
      ((ty, tys) : Rocq_utils.kind_pair)
  : Transition_opt.t
  =
  Log.trace "Mebi_proof.get_weak_transition";
  if typ_is_weak_transition sigma (ty, tys)
  then get_transition sigma tys.(3) tys.(5) tys.(4) fsm
  else raise (Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)))
;;

let _get_silent_transition
      (sigma : Evd.evar_map)
      (fsm : Fsm.t)
      ((ty, tys) : Rocq_utils.kind_pair)
  : Transition_opt.t
  =
  Log.trace "Mebi_proof.get_silent_transition";
  if typ_is_silent_transition sigma (ty, tys)
  then get_transition sigma tys.(2) tys.(4) tys.(3) fsm
  else raise (Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)))
;;

let _get_silent1_transition
      (sigma : Evd.evar_map)
      (fsm : Fsm.t)
      ((ty, tys) : Rocq_utils.kind_pair)
  : Transition_opt.t
  =
  Log.trace "Mebi_proof.get_silent1_transition";
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
  Log.trace "Mebi_proof.weak_sim_get_m_state";
  if typ_is_weak_sim sigma (ty, tys)
  then (
    try find_state sigma tys.(5) (mfsm ()).states with
    | Mebi_proof_CouldNotDecodeState (sigma, statety, states) ->
      raise (Mebi_proof_CouldNotGetStateM (sigma, (ty, tys))))
  else raise (Mebi_proof_CouldNotGetStateM (sigma, (ty, tys)))
;;

(***********************************************************************)
(*** Hypothesis ********************************************************)
(***********************************************************************)

module Cofix_HTy : Hyp.HTY_S = struct
  type t =
    { _m : State.t
    ; _n : State.t
    }

  let of_hty (sigma : Evd.evar_map) ((ty, tys) : Rocq_utils.kind_pair) : t =
    Log.trace "Mebi_proof.Cofix_HTy.of_hty";
    if Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_weak_sim ())
    then (
      let _m : State.t = decode_state (get_encoding tys.(5)) (mfsm ()).states in
      let _n : State.t = decode_state (get_encoding tys.(6)) (nfsm ()).states in
      { _m; _n })
    else raise (Hyp.Mebi_proof_Hypothesis_HTy (sigma, (ty, tys)))
  ;;
end

module Cofix : Hyp.HYP_TYPE = Hyp.Make (Cofix_HTy)

module Invertible = struct
  type t =
    { kind : k
    ; tactic : tactic
    }

  and k =
    | Full
    | Layer

  let of_hty (sigma : Evd.evar_map) ((ty, tys) : Rocq_utils.kind_pair) : k =
    Log.trace "Mebi_proof.Invertible.of_hty";
    try
      if Mebi_theories.is_var sigma tys.(2)
      then Full
      else if Mebi_theories.is_var sigma tys.(1)
      then Layer
      else raise (Hyp.Mebi_proof_Hypothesis_HTy (sigma, (ty, tys)))
    with
    | Invalid_argument _ ->
      raise (Hyp.Mebi_proof_Hypothesis_HTy (sigma, (ty, tys)))
  ;;

  let opt_of_hty (sigma : Evd.evar_map) (p : Rocq_utils.kind_pair) : k option =
    Log.trace "Mebi_proof.Invertible.opt_of_hty";
    try Some (of_hty sigma p) with Hyp.Mebi_proof_Hypothesis_HTy _ -> None
  ;;

  let hty_is_a (sigma : Evd.evar_map) (p : Rocq_utils.kind_pair) : bool =
    Log.trace "Mebi_proof.Invertible.hty_is_a";
    Option.has_some (opt_of_hty sigma p)
  ;;

  let of_hyp (sigma : Evd.evar_map) (h : Rocq_utils.hyp) : t =
    Log.trace "Mebi_proof.Invertible.of_hyp";
    try
      { kind = of_hty sigma (Rocq_utils.hyp_to_atomic sigma h)
      ; tactic = do_inversion h
      }
    with
    | Hyp.Mebi_proof_Hypothesis_HTy (sigma, p) ->
      raise (Hyp.Mebi_proof_Hypothesis_Hyp (sigma, h, p))
  ;;

  let opt_of_hyp (sigma : Evd.evar_map) (h : Rocq_utils.hyp) : t option =
    Log.trace "Mebi_proof.Invertible.opt_of_hyp";
    try Some (of_hyp sigma h) with Hyp.Mebi_proof_Hypothesis_Hyp _ -> None
  ;;

  let hyp_is_a (sigma : Evd.evar_map) (h : Rocq_utils.hyp) : bool =
    Log.trace "Mebi_proof.Invertible.hyp_is_a";
    Option.has_some (opt_of_hyp sigma h)
  ;;
end

module TransOpt : Hyp.HYP_TYPE = Hyp.Make (struct
    type t = Transition_opt.t

    let of_hty (sigma : Evd.evar_map) ((ty, tys) : Rocq_utils.kind_pair) : t =
      Log.trace "Mebi_proof.TransOpt.of_hty";
      try get_lts_transition sigma (mfsm ()) (ty, tys) with
      | Mebi_proof_CouldNotDecodeTransition (sigma, x, fsm) ->
        raise (Hyp.Mebi_proof_Hypothesis_HTy (sigma, (ty, tys)))
      | Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)) ->
        raise (Hyp.Mebi_proof_Hypothesis_HTy (sigma, (ty, tys)))
    ;;
  end)

(***********************************************************************)

(** precedence of hyps:
    - cofix
    - full invert
    - layer invert
    - transition *)
let hyp_is_something (sigma : Evd.evar_map) (h : Rocq_utils.hyp) : bool =
  Log.trace "Mebi_proof.hyp_is_something";
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
  Log.trace "Mebi_proof.hyps_is_essentially_empty";
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  Bool.not
    (List.for_all
       (fun (h : Rocq_utils.hyp) -> hyp_is_something sigma h)
       (Proofview.Goal.hyps gl))
;;

(** first checks if hypothesis are empty, and if they are not then it checks if [hyps_is_essentially_empty]
*)
let hyps_is_empty (gl : Proofview.Goal.t) : bool =
  Log.trace "Mebi_proof.hyps_is_empty";
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

let concl_get_eexists
      (gl : Proofview.Goal.t)
      (* (sigma : Evd.evar_map) *)
        ((ty, tys) : Rocq_utils.kind_pair)
  : Transition_opt.t * State.t
  =
  Log.trace "Mebi_proof.concl_get_eexists";
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
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
         weak_sim_get_m_state sigma (Rocq_utils.econstr_to_atomic sigma wk_sim)
       in
       nt, mstate
     with
     | Mebi_proof_CouldNotDecodeTransition (sigma, x, fsm) ->
       Log.warning
         (Printf.sprintf
            "Mebi_proof.concl_get_eexists Mebi_proof_CouldNotDecodeTransition:\n\
             %s"
            (econstr_to_string gl x));
       raise (Mebi_proof_ConclIsNot_Exists (sigma, (ty, tys)))
     | Mebi_proof_CouldNotGetStateM (sigma, (ty, tys)) ->
       Log.warning
         (Printf.sprintf
            "Mebi_proof.concl_get_eexists Mebi_proof_CouldNotGetStateM");
       raise (Mebi_proof_ConclIsNot_Exists (sigma, (ty, tys)))
     | Rocq_utils.Rocq_utils_EConstrIsNot_Atomic (sigma, x, k) ->
       Log.warning
         (Printf.sprintf
            "Mebi_proof.concl_get_eexists Rocq_utils_EConstrIsNot_Atomic");
       raise (Mebi_proof_ConclIsNot_Exists (sigma, (ty, tys)))
     | Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)) ->
       Log.warning
         (Printf.sprintf
            "Mebi_proof.concl_get_eexists Mebi_proof_TyDoesNotMatchTheories");
       raise (Mebi_proof_ConclIsNot_Exists (sigma, (ty, tys))))
  | _ ->
    Log.warning
      (Printf.sprintf
         "Mebi_proof.concl_get_eexists array not [wk_trans;wk_sim]");
    raise (Mebi_proof_ConclIsNot_Exists (sigma, (ty, tys)))
;;

let hyp_is_invertible (sigma : Evd.evar_map) : Rocq_utils.hyp -> bool =
  Log.trace "Mebi_proof.hyp_is_invertible";
  fun (x : Rocq_utils.hyp) ->
    try Invertible.hyp_is_a sigma x with
    | Hyp.Mebi_proof_Hypothesis_Hyp _ -> false
;;

let hyps_get_invertibles (sigma : Evd.evar_map)
  : Rocq_utils.hyp list -> Invertible.t list
  =
  Log.trace "Mebi_proof.hyps_get_invertibles";
  List.filter_map (fun (x : Rocq_utils.hyp) ->
    try Some (Invertible.of_hyp sigma x) with
    | Hyp.Mebi_proof_Hypothesis_Hyp _ -> None)
;;

let hyps_get_invertible (invertibles : Invertible.t list) : Invertible.t =
  Log.trace "Mebi_proof.hyps_get_invertible";
  let fully_invertibles : Invertible.t list =
    List.filter
      (function ({ kind = Full; _ } : Invertible.t) -> true | _ -> false)
      invertibles
  in
  List.hd
    (if List.is_empty fully_invertibles then invertibles else fully_invertibles)
;;

let do_hyp_inversion (gl : Proofview.Goal.t) : tactic =
  Log.trace "Mebi_proof.do_inversion";
  (hyps_get_invertibles (Proofview.Goal.sigma gl) (Proofview.Goal.hyps gl)
   |> hyps_get_invertible)
    .tactic
;;

let get_ngoto
      (mgoto : State.t)
      ({ from = nfrom; label = nlabel; _ } : Transition_opt.t)
  : State.t
  =
  Log.trace "Mebi_proof.get_ngoto";
  let nactions : States.t Actions.t = Edges.find (nfsm ()).edges nfrom in
  Model.get_action_labelled nlabel nactions
  |> Actions.find nactions
  |> States.inter (Model.get_bisim_states mgoto (the_bisim_states ()))
  |> States.min_elt
;;

let do_ex_intro (gl : Proofview.Goal.t) (ngoto : State.t) : tactic =
  Log.trace "Mebi_proof.do_ex_intro";
  let ngoto : EConstr.t = get_decoding ngoto.enc in
  let ngoto_bindings = Tactypes.ImplicitBindings [ ngoto ] in
  tactic
    ~msg:(Printf.sprintf "(exists %s)" (econstr_to_string gl ngoto))
    (Mebi_theories.tactics
       [ Tactics.constructor_tac true None 1 ngoto_bindings
       ; Tactics.split Tactypes.NoBindings
       ])
;;

exception Mebi_proof_ExIntro_NEqStateM of (State.t * State.t option)
exception Mebi_proof_ExIntro_NEqLabel of (Transition_opt.t * Transition_opt.t)
exception Mebi_proof_ExIntro_NotBisimilar of (State.t * State.t)

exception
  Mebi_proof_ExIntro_Transitions of (Transition_opt.t * Transition_opt.t)

let do_eexists_transition (gl : Proofview.Goal.t) : tactic =
  log_trace_hyps gl "Mebi_proof.do_eexists_transition";
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let mtransition : Transition_opt.t =
    get_hyp_transition gl (mfsm ()) (Proofview.Goal.hyps gl)
  in
  Log.debug
    (Printf.sprintf
       "Mebi_proof.do_eexists_transition, mtransition:\n%s"
       (Transition_opt.to_string mtransition));
  let ((ntransition, mstate) : Transition_opt.t * State.t) =
    Proofview.Goal.concl gl
    |> Rocq_utils.econstr_to_atomic sigma
    |> concl_get_eexists gl
  in
  (* ERR: if labels are not consistent *)
  if Bool.not (Label.equal mtransition.label ntransition.label)
  then raise (Mebi_proof_ExIntro_NEqLabel (mtransition, ntransition));
  (* ERR: if from states are not bisimilar *)
  if Bool.not (are_states_bisimilar mtransition.from ntransition.from)
  then
    raise (Mebi_proof_ExIntro_NotBisimilar (mtransition.from, ntransition.from));
  (* ERR: unless Some mgoto and None ngoto *)
  match mtransition.goto, ntransition.goto with
  | Some mgoto, None ->
    (* ERR: if states are not consistent *)
    if Bool.not (State.equal mstate mgoto)
    then raise (Mebi_proof_ExIntro_NEqStateM (mstate, Some mgoto));
    the_proof_state := GoalTransition mtransition;
    get_ngoto mgoto ntransition |> do_ex_intro gl
  | _, _ -> raise (Mebi_proof_ExIntro_Transitions (mtransition, ntransition))
;;

(***********************************************************************)

let hyps_has_cofix (sigma : Evd.evar_map) (concl : EConstr.t)
  : Rocq_utils.hyp list -> bool
  =
  Log.trace "Mebi_proof.hyps_has_cofix";
  List.exists (fun (h : Rocq_utils.hyp) ->
    Mebi_setup.Eq.econstr sigma concl (Context.Named.Declaration.get_type h))
;;

let hyps_has_invertible (sigma : Evd.evar_map) : Rocq_utils.hyp list -> bool =
  Log.trace "Mebi_proof.hyps_has_invertible";
  List.exists (hyp_is_invertible sigma)
;;

(***********************************************************************)
(*** Handle Proof States ***********************************************)
(***********************************************************************)

exception Mebi_proof_NewProof of unit
exception Mebi_proof_NewWeakSim of unit
exception Mebi_proof_NewCofix of unit
exception Mebi_proof_GoalTransition of unit
(* exception Mebi_proof_ApplyConstructors of unit *)

(** [handle_new_proof gl] checks if the [hyps] of [gl] are empty before moving to state [NewWeakSim]
*)
let rec handle_new_proof (gl : Proofview.Goal.t) : tactic =
  Log.trace "Mebi_proof.handle_new_proof";
  if hyps_is_empty gl
  then (
    the_proof_state := NewWeakSim;
    handle_proof_state gl)
  else (
    Log.warning "New Proof: Expected Hypothesis to be empty";
    raise (Mebi_proof_NewProof ()))

and handle_new_weak_sim (gl : Proofview.Goal.t) : tactic =
  log_trace_concl gl "Mebi_proof.handle_new_weak_sim";
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let the_concl : EConstr.t = Proofview.Goal.concl gl in
  let concltyp : Rocq_utils.kind_pair =
    Rocq_utils.econstr_to_atomic sigma the_concl
  in
  (* NOTE: if concl is [weak_sim] and hyp has corresponding [cofix] then [auto]. *)
  if typ_is_weak_sim sigma concltyp
  then
    if hyps_has_cofix sigma the_concl (Proofview.Goal.hyps gl)
    then do_solve_cofix gl (* NOTE: sets [the_proof_state := ... ] *)
    else (
      the_proof_state := NewCofix;
      do_new_cofix gl)
  else if typ_is_exists sigma concltyp
  then (
    the_proof_state := NewCofix;
    handle_proof_state gl)
  else (
    Log.warning
      (Printf.sprintf
         "Mebi_proof.handle_new_weak_sim, concl is not weak_sim or has eexists.\n\
          %s"
         (concl_to_string gl));
    raise (Mebi_proof_NewWeakSim ()))

and handle_new_cofix (gl : Proofview.Goal.t) : tactic =
  Log.trace "Mebi_proof.handle_new_cofix";
  try
    if hyps_has_invertible (Proofview.Goal.sigma gl) (Proofview.Goal.hyps gl)
    then do_hyp_inversion gl
    else do_eexists_transition gl
  with
  | Mebi_proof_CouldNotFindHypTransition (sigma, fsm, hyps) ->
    Log.warning
      (Printf.sprintf
         "Could not find any transitions in Hyps: %s"
         (hyps_to_string gl));
    raise (Mebi_proof_NewCofix ())
  | Mebi_proof_CouldNotDecodeTransition (sigma, x, fsm) ->
    Log.warning
      (Printf.sprintf "Could not decode state: %s" (econstr_to_string gl x));
    raise (Mebi_proof_NewCofix ())
  | Mebi_proof_ConclIsNot_Exists (sigma, (ty, tys)) ->
    Log.warning
      (Printf.sprintf
         "Concl is not eexists: %s\n%s"
         (econstr_to_string gl ty)
         (Utils.Strfy.array
            (fun ?(args : style_args = style_args ()) x ->
              econstr_to_string gl x)
            tys));
    raise (Mebi_proof_NewCofix ())
  | Mebi_proof_ExIntro_NEqStateM (mstate, mfrom) ->
    Log.warning "Mebi_proof_ExIntro_NEqStateM";
    raise (Mebi_proof_NewCofix ())
  | Mebi_proof_ExIntro_NEqLabel (mtransition, ntransition) ->
    Log.warning "Mebi_proof_ExIntro_NEqLabel";
    raise (Mebi_proof_NewCofix ())

and handle_goal_transition (gl : Proofview.Goal.t) (mtrans : Transition_opt.t)
  : tactic
  =
  Log.trace "Mebi_proof.handle_goal_transition";
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
      if are_states_bisimilar mtrans.from ngoto
      then
        if State.equal nfrom ngoto
        then
          (* NOTE: sets [the_proof_state := NewWeakSim] *)
          do_refl_none gl
        else (* NOTE: sets [the_proof_state := ApplyConstructors(...)] *)
          do_constructor_transition
            gl
            nfrom
            nlabel
            (if Label.is_silent nlabel
             then do_apply_wk_none gl
             else do_eapply_wk_some gl)
      else
        raise
          (Mebi_proof_StatesNotBisimilar
             (mtrans.from, ngoto, the_bisim_states ()))
    | _, _ -> raise (Mebi_proof_GoalTransition ())
  with
  | Mebi_proof_CouldNotDecodeTransition (sigma, x, fsm) ->
    raise (Mebi_proof_GoalTransition ())
  | Mebi_proof_StatesNotBisimilar (mstate, nstate, pi) ->
    raise (Mebi_proof_GoalTransition ())

and handle_apply_constructors (gl : Proofview.Goal.t)
  : PState.applicable_constructors -> tactic
  =
  Log.trace "Mebi_proof.handle_apply_constructors";
  function
  | { annotation = []; tactics } ->
    if PState.empty_tactics tactics
    then (
      Log.debug "A";
      the_proof_state := NewWeakSim;
      do_eapply_rt1n_refl gl)
    else (
      Log.debug "B";
      (* tactic_chain [ do_simplify gl; do_eapply_rt1n_refl gl ] *)
      do_constructor_tactic gl [] tactics)
  | { annotation; tactics } -> do_constructor_tactic gl annotation tactics

(* | { annotation; tactics = None } ->
    Log.debug "C";
    do_build_constructor_tactics gl annotation
  | { annotation; tactics = Some [] } ->
    Log.debug "D";
    tactic_chain [ do_simplify gl; do_build_constructor_tactics gl annotation ]
  | { annotation; tactics = Some (h :: tl) } ->
    Log.debug "E";
    the_proof_state := ApplyConstructors { annotation; tactics = Some tl };
    h *)

and handle_proof_state (gl : Proofview.Goal.t) : tactic =
  Log.trace "Mebi_proof.handle_proof_state";
  match !the_proof_state with
  | NewProof -> handle_new_proof gl
  | NewWeakSim -> handle_new_weak_sim gl
  | NewCofix -> handle_new_cofix gl
  | GoalTransition mtransition -> handle_goal_transition gl mtransition
  | ApplyConstructors napplicable_constructors ->
    Log.debug (PState.to_string ~short:false !the_proof_state);
    handle_apply_constructors gl napplicable_constructors
  | DetectState -> detect_proof_state gl

and detect_proof_state (gl : Proofview.Goal.t) : tactic =
  Log.trace "Mebi_proof.detect_proof_state";
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
  Log.trace "Mebi_proof.step";
  Mebi_theories.tactics
    [ Proofview.Goal.enter (fun gl -> get_tactic (handle_proof_state gl))
    ; Mebi_tactics.simplify_and_subst_all ()
    ]
;;

let solve (upper_bound : int) (pstate : Declare.Proof.t) : Declare.Proof.t =
  Log.trace "Mebi_proof.solve";
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
