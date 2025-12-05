open Logging
open Mebi_wrapper
open Model
open Debug
module Hyp = Mebi_hypothesis

(* the prefix *)
let trace_enabled : bool = true
let log_trace (x : string) : unit = if trace_enabled then Log.trace x else ()

let log_tracex (xs : string list) : unit =
  log_trace (Utils.Strfy.list Utils.Strfy.string xs)
;;

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
    { annotation : Note.annotation option
    ; tactics : tactic list option
    }

  (* and tactic_to_apply = unit -> unit Proofview.tactic *)

  let empty_tactics : tactic list option -> bool = function
    | None -> true
    | Some [] -> true
    | Some (_ :: _) -> false
  ;;

  let to_string ?(short : bool = true) : t -> string = function
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
             ":\nAnnotation: %s\nTactics: %s"
             (Option.cata Note.annotation_to_string "None" annotation)
             (Option.cata
                (fun xs ->
                  Printf.sprintf
                    "%s (Tactics: %i)"
                    (List.fold_left
                       (fun acc { msg; _ } ->
                         Printf.sprintf
                           "%s; %s"
                           acc
                           (Option.cata (fun y -> y) "None" msg))
                       ""
                       xs)
                    (List.length xs))
                "None"
                tactics))
    | DetectState -> "DetectState"
  ;;
end

let default_proof_state : PState.t = PState.NewProof
let the_proof_state : PState.t ref = ref default_proof_state

(** used to keep track of all the proof states grouped by step *)
let the_old_proof_states : PState.t list list ref = ref []

let reset_the_proof_state () : unit =
  the_proof_state := default_proof_state;
  the_old_proof_states := []
;;

let the_old_proof_states_to_string ?(short : bool = true) () : string =
  let open Utils.Strfy in
  list
    (list (fun ?(args : style_args = style_args ()) p ->
       PState.to_string ~short p))
    !the_old_proof_states
;;

let update_old_proof_states (x : PState.t) : unit =
  match !the_old_proof_states with
  | [] -> the_old_proof_states := [ [ x ] ]
  | [ [] ] -> the_old_proof_states := [ [ x ] ]
  | h :: tl -> the_old_proof_states := (x :: h) :: tl
;;

let set_the_proof_state ?(short : bool = true) (funstr : string) (x : PState.t)
  : unit
  =
  log_trace (Printf.sprintf "%s %s" funstr (PState.to_string ~short x));
  update_old_proof_states !the_proof_state;
  the_proof_state := x
;;

let _debug_proof_state ?(short : bool = true) () : unit =
  Log.debug
    (Printf.sprintf
       "Current: %s\n\n\
        Newest: (empty after MewWeakSim indicates Inversion)%s(Oldest)\n"
       (PState.to_string ~short !the_proof_state)
       (the_old_proof_states_to_string ~short ()))
;;

let get_tactic ?(short : bool = true) ?(state : bool = true)
  : tactic -> unit Proofview.tactic
  = function
  | { msg = None; x } -> x
  | { msg = Some msg; x } ->
    if state then _debug_proof_state ~short ();
    (* NOTE: we pad so that a next iteration is group separately *)
    the_old_proof_states := [] :: !the_old_proof_states;
    Log.notice (Printf.sprintf "%s." msg);
    x
;;

(***********************************************************************)
(*** Bisimilarity Result ***********************************************)
(***********************************************************************)

let the_result () : Algorithms.Bisimilar.result =
  Algorithms.Bisimilar.get_the_result ()
;;

let mfsm ?(saturated : bool = false) () : Fsm.t =
  if saturated
  then (the_result ()).the_fsm_1.saturated
  else (the_result ()).the_fsm_1.original
;;

let nfsm ?(saturated : bool = false) () : Fsm.t =
  if saturated
  then (the_result ()).the_fsm_2.saturated
  else (the_result ()).the_fsm_2.original
;;

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

let get_bisim_states (x : State.t) : States.t =
  Model.get_bisim_states x (the_bisim_states ())
;;

let are_states_bisimilar (m : State.t) (n : State.t) : bool =
  get_bisim_states m |> States.mem n
;;

exception Mebi_proof_StatesNotBisimilar of (State.t * State.t * Partition.t)

let assert_states_bisimilar (m : State.t) (n : State.t) : unit =
  if are_states_bisimilar m n
  then ()
  else raise (Mebi_proof_StatesNotBisimilar (m, n, the_bisim_states ()))
;;

exception
  Mebi_proof_TransitionOptStatesNotBisimilar of
    (State.t * State.t * Partition.t)

let _are_transition_opt_states_bisimilar
  : Transition_opt.t -> Transition_opt.t -> bool
  = function
  | { from = mfrom; goto = None; _ } ->
    (function { from = nfrom; _ } -> are_states_bisimilar mfrom nfrom)
  | { from = mfrom; goto = Some mgoto; _ } ->
    (function
      | { from = nfrom; goto = None; _ } -> are_states_bisimilar mfrom nfrom
      | { from = nfrom; goto = Some ngoto; _ } ->
        are_states_bisimilar mfrom nfrom && are_states_bisimilar mgoto ngoto)
;;

let assert_transition_opt_states_bisimilar
  : Transition_opt.t -> Transition_opt.t -> unit
  =
  try
    function
    | { from = mfrom; goto = None; _ } ->
      (function { from = nfrom; _ } -> assert_states_bisimilar mfrom nfrom)
    | { from = mfrom; goto = Some mgoto; _ } ->
      (function
        | { from = nfrom; goto = None; _ } ->
          assert_states_bisimilar mfrom nfrom
        | { from = nfrom; goto = Some ngoto; _ } ->
          assert_states_bisimilar mfrom nfrom;
          assert_states_bisimilar mgoto ngoto)
  with
  | Mebi_proof_StatesNotBisimilar (mstate, nstate, bisim_states) ->
    raise
      (Mebi_proof_TransitionOptStatesNotBisimilar (mstate, nstate, bisim_states))
;;

let get_naction
      ?(annotated : bool = false)
      (nfrom : State.t)
      (nlabel : Label.t)
      (nfsm : Fsm.t)
  : Action.t
  =
  log_trace __FUNCTION__;
  let prefix : string -> string = Printf.sprintf "%s %s" __FUNCTION__ in
  Debug.thing (prefix "nfrom") nfrom (A State.to_string);
  Debug.thing (prefix "nlabel") nlabel (A Label.to_string);
  (* Edges.find (nfsm ()).edges nfrom |> Model.get_action_labelled nlabel *)
  let x = Model.get_action_labelled_from ~annotated nfrom nlabel nfsm.edges in
  Debug.thing (prefix "@@ naction") x (A Action.to_string);
  x
;;

let get_constructor_annotation (nfrom : State.t) (nlabel : Label.t)
  : Note.annotation
  =
  log_trace __FUNCTION__;
  try
    get_naction ~annotated:false nfrom nlabel (nfsm ~saturated:false ())
    |> Model.get_shortest_annotation_from nfrom
  with
  | Model_NoActionLabelledFrom (annotated, from, label, edges) ->
    get_naction ~annotated:true nfrom nlabel (nfsm ~saturated:true ())
    |> Model.get_shortest_annotation_from nfrom
;;

let get_annotation_constructor (nfrom : State.t) (nlabel : Label.t)
  : Tree.node list
  =
  log_trace __FUNCTION__;
  get_naction ~annotated:true nfrom nlabel (nfsm ~saturated:false ())
  |> Model.get_shortest_constructor
;;

(* try
   get_naction ~annotated:true nfrom nlabel (nfsm ~saturated:false ())
   |> Model.get_shortest_constructor
   with
   | Model_NoActionLabelledFrom (annotated, from, label, edges) ->
   get_naction ~annotated:true nfrom nlabel (nfsm ~saturated:false ())
   |> Model.get_shortest_constructor *)

(***********************************************************************)

exception Mebi_proof_FailIfNothing of unit

let _do_nothing () : tactic =
  Log.warning "mebi_proof, case just does nothing";
  (* tactic ~msg:"((do_nothing))" (Proofview.tclUNIT ()) *)
  raise (Mebi_proof_FailIfNothing ())
;;

exception
  Mebi_proof_TyDoesNotMatchTheories of (Evd.evar_map * Rocq_utils.kind_pair)

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
  log_trace __FUNCTION__;
  tactic
    ~msg:"apply rt1n_refl"
    (Mebi_theories.tactics
       [ Mebi_tactics.apply ~gl (Mebi_theories.c_rt1n_refl ()) ])
;;

let do_eapply_rt1n_refl (gl : Proofview.Goal.t) : tactic =
  log_trace __FUNCTION__;
  tactic
    ~msg:"eapply rt1n_refl"
    (Mebi_theories.tactics
       [ Mebi_tactics.eapply ~gl (Mebi_theories.c_rt1n_refl ()) ])
;;

let _do_apply_rt1n_trans (gl : Proofview.Goal.t) : tactic =
  log_trace __FUNCTION__;
  tactic
    ~msg:"apply rt1n_trans"
    (Mebi_theories.tactics
       [ Mebi_tactics.apply ~gl (Mebi_theories.c_rt1n_trans ()) ])
;;

let do_eapply_rt1n_trans (gl : Proofview.Goal.t) : tactic =
  log_trace __FUNCTION__;
  tactic
    ~msg:"eapply rt1n_trans"
    (Mebi_theories.tactics
       [ Mebi_tactics.eapply ~gl (Mebi_theories.c_rt1n_trans ()) ])
;;

let do_rt1n_via (gl : Proofview.Goal.t) (via : Label.t) : tactic =
  log_trace __FUNCTION__;
  tactic_chain
    [ (if Label.is_silent via
       then do_eapply_rt1n_trans gl
       else do_eapply_rt1n_refl gl)
    ; do_simplify gl
    ]
;;

let do_solve_cofix (gl : Proofview.Goal.t) : tactic =
  log_trace __FUNCTION__;
  set_the_proof_state __FUNCTION__ DetectState;
  tactic ~msg:"(do_solve)" (Auto.gen_trivial ~debug:Hints.Info [] None)
;;

let do_apply_wk_none (gl : Proofview.Goal.t) : tactic =
  log_trace __FUNCTION__;
  tactic
    ~msg:"apply wk_none"
    (Mebi_tactics.apply ~gl (Mebi_theories.c_wk_none ()))
;;

let do_unfold_silent (gl : Proofview.Goal.t) : tactic =
  log_trace __FUNCTION__;
  tactic
    ~msg:"unfold silent"
    (Mebi_tactics.unfold_econstr gl (Mebi_theories.c_silent ()))
;;

let do_eapply_wk_some (gl : Proofview.Goal.t) : tactic =
  log_trace __FUNCTION__;
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
  log_trace __FUNCTION__;
  let prefix : string -> string = Printf.sprintf "%s %s" __FUNCTION__ in
  let annotation : Note.annotation = get_constructor_annotation nfrom nlabel in
  Debug.thing (prefix "annotation") annotation (A Note.annotation_to_string);
  set_the_proof_state
    __FUNCTION__
    (ApplyConstructors { annotation = Some annotation; tactics = None });
  tactic_chain [ htactic; do_unfold_silent gl ]
;;

let _do_refl_none (gl : Proofview.Goal.t) : tactic =
  set_the_proof_state __FUNCTION__ NewWeakSim;
  tactic_chain
    [ do_apply_wk_none gl; do_unfold_silent gl; do_apply_rt1n_refl gl ]
;;

(* let do_weak_silent_transition
   (gl : Proofview.Goal.t)
   (from : State.t)
   (goto : State.t)
   : tactic
   =
   log_trace "do_weak_silent_transition";
   if State.equal from goto
   then (
   set_the_proof_state __FUNCTION__  NewWeakSim;
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
   log_trace "do_weak_visible_transition";
   if are_states_bisimilar mfrom ngoto
   then do_constructor_transition gl nfrom nlabel (do_eapply_wk_some gl)
   else raise (Mebi_proof_StatesNotBisimilar (mfrom, ngoto, the_bisim_states ()))
   ;; *)

let get_constructor_tactic ((enc, index) : Tree.node) : tactic =
  log_trace __FUNCTION__;
  let index : int = index + 1 in
  tactic
    ~msg:(Printf.sprintf "constructor %i" index)
    (Tactics.one_constructor index Tactypes.NoBindings)
;;

let rec get_constructor_tactics_to_apply : Tree.node list -> tactic list =
  log_trace __FUNCTION__;
  function
  | [] -> []
  | h :: tl -> get_constructor_tactic h :: get_constructor_tactics_to_apply tl
;;

let do_build_constructor_tactics (gl : Proofview.Goal.t)
  : Note.annotation -> tactic
  =
  log_trace __FUNCTION__;
  let prefix : string -> string = Printf.sprintf "%s %s" __FUNCTION__ in
  function
  | { this = { from; via }; next } ->
    Debug.thing (prefix "from") from (A State.to_string);
    Debug.thing (prefix "via") via (A Label.to_string);
    Debug.option (prefix "next") next (A Note.annotation_to_string);
    let constructors : Tree.node list = get_annotation_constructor from via in
    let tactics : tactic list = get_constructor_tactics_to_apply constructors in
    Debug.option (prefix "next annotation") next (A Note.annotation_to_string);
    set_the_proof_state
      __FUNCTION__
      (ApplyConstructors { annotation = next; tactics = Some tactics });
    do_rt1n_via gl via
;;

(* exception Mebi_proof_TacticsNotEmpty of tactic list option *)

(* let handle_maybe_empty_tactics
   (gl : Proofview.Goal.t)
   (annotation : Note.annotation)
   : tactic list option -> tactic
   = function
   | None ->
   log_tracex [ __FUNCTION__; "tactics None" ];
   do_build_constructor_tactics gl annotation
   | Some [] ->
   log_tracex [ __FUNCTION__; "tactics Some []" ];
   tactic_chain [ do_simplify gl; do_build_constructor_tactics gl annotation ]
   | non_empty_tactics -> raise (Mebi_proof_TacticsNotEmpty non_empty_tactics)
   ;; *)

(* let do_constructor_tactic (gl : Proofview.Goal.t) (annotation : Note.annotation option)
  : tactic list option -> tactic
  =
  log_trace __FUNCTION__;
  function
  | Some (h :: tl) ->
    log_tracex [ __FUNCTION__; "tactics Some (h::t)" ];
    set_the_proof_state
      __FUNCTION__
      (ApplyConstructors { annotation; tactics = Some tl });
    h
  | maybe_empty_tactics ->
    handle_maybe_empty_tactics gl annotation maybe_empty_tactics
;; *)

(***********************************************************************)

exception Mebi_proof_CouldNotDecodeEConstr of (Evd.evar_map * EConstr.t)

let try_decode (sigma : Evd.evar_map) (x : EConstr.t) : Enc.t option =
  log_trace __FUNCTION__;
  if EConstr.isRel sigma x
  then None
  else (
    try get_encoding_opt x with
    | Not_found -> raise (Mebi_proof_CouldNotDecodeEConstr (sigma, x)))
;;

let typ_is_exists (sigma : Evd.evar_map) ((ty, _) : Rocq_utils.kind_pair) : bool
  =
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_ex ())
;;

let typ_is_weak_sim (sigma : Evd.evar_map) ((ty, _) : Rocq_utils.kind_pair)
  : bool
  =
  log_trace __FUNCTION__;
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_weak_sim ())
;;

let typ_is_weak_transition
      (sigma : Evd.evar_map)
      ((ty, _) : Rocq_utils.kind_pair)
  : bool
  =
  log_trace __FUNCTION__;
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_weak ())
;;

let typ_is_silent_transition
      (sigma : Evd.evar_map)
      ((ty, _) : Rocq_utils.kind_pair)
  : bool
  =
  log_trace __FUNCTION__;
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_silent ())
;;

let typ_is_silent1_transition
      (sigma : Evd.evar_map)
      ((ty, _) : Rocq_utils.kind_pair)
  : bool
  =
  log_trace __FUNCTION__;
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_silent1 ())
;;

let typ_is_lts_transition
      (sigma : Evd.evar_map)
      ((ty, _) : Rocq_utils.kind_pair)
  : bool
  =
  log_trace __FUNCTION__;
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_LTS ())
;;

(***********************************************************************)

let try_find_state (sigma : Evd.evar_map) (x : EConstr.t) (states : States.t)
  : State.t option
  =
  log_trace __FUNCTION__;
  Option.map (fun (y : Enc.t) -> decode_state y states) (try_decode sigma x)
;;

exception
  Mebi_proof_CouldNotDecodeState of (Evd.evar_map * EConstr.t * States.t)

let find_state (sigma : Evd.evar_map) (x : EConstr.t) (states : States.t)
  : State.t
  =
  log_trace __FUNCTION__;
  try
    match try_find_state sigma x states with
    | Some s -> s
    | None -> raise (Mebi_proof_CouldNotDecodeState (sigma, x, states))
  with
  | Model_CannotDecodeState (enc, states) ->
    Log.debug
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
  log_trace __FUNCTION__;
  let prefix : string -> string = Printf.sprintf "%s %s" __FUNCTION__ in
  Debug.thing (prefix "Labels") labels (A alphabet_to_string);
  try
    Option.map
      (fun (y : Enc.t) ->
        try Model.find_label_of_enc y labels with
        | Not_found -> raise (Mebi_proof_CouldNotDecodeLabel (sigma, x, labels)))
      (try_decode sigma x)
  with
  | Mebi_proof_CouldNotDecodeLabel (sigma, x, labels) ->
    raise (Mebi_proof_CouldNotDecodeLabel (sigma, x, labels))
;;

let find_label (sigma : Evd.evar_map) (x : EConstr.t) (labels : Alphabet.t)
  : Label.t
  =
  log_trace __FUNCTION__;
  match try_find_label sigma x labels with
  | Some s -> s
  | None -> raise (Mebi_proof_CouldNotDecodeLabel (sigma, x, labels))
;;

(***********************************************************************)

exception
  Mebi_proof_CouldNotDecodeTransitionState of (Evd.evar_map * EConstr.t * Fsm.t)

exception
  Mebi_proof_CouldNotDecodeTransitionLabel of (Evd.evar_map * EConstr.t * Fsm.t)

let get_transition
      (sigma : Evd.evar_map)
      (fromty : EConstr.t)
      (labelty : EConstr.t)
      (gototy : EConstr.t)
      (fsm : Fsm.t)
  : Transition_opt.t
  =
  log_trace __FUNCTION__;
  let prefix : string -> string = Printf.sprintf "%s %s" __FUNCTION__ in
  try
    let from : State.t = find_state sigma fromty fsm.states in
    Debug.thing (prefix "From") from (A State.to_string);
    Debug.thing (prefix "Alphabet") fsm.alphabet (A alphabet_to_string);
    let label : Label.t = find_label sigma labelty fsm.alphabet in
    Debug.thing (prefix "Label") label (A Label.to_string);
    let goto : State.t option = try_find_state sigma gototy fsm.states in
    Debug.option (prefix "Goto") goto (A State.to_string);
    let actions : States.t Actions.t = Edges.find fsm.edges from in
    Debug.thing (prefix "Actions") actions (A action_labels_to_string);
    let { annotations; constructor_trees; _ } : Action.t =
      get_action_labelled ~annotated:true label (Edges.find fsm.edges from)
    in
    Transition_opt.create from label goto annotations constructor_trees
  with
  | Mebi_proof_CouldNotDecodeState (sigma, ty, states) ->
    raise (Mebi_proof_CouldNotDecodeTransitionState (sigma, ty, fsm))
  | Mebi_proof_CouldNotDecodeLabel (sigma, ty, alphabet) ->
    raise (Mebi_proof_CouldNotDecodeTransitionLabel (sigma, ty, fsm))
;;

let get_lts_transition
      (sigma : Evd.evar_map)
      (fsm : Fsm.t)
      ((ty, tys) : Rocq_utils.kind_pair)
  : Transition_opt.t
  =
  log_trace __FUNCTION__;
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
  log_trace __FUNCTION__;
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  (* NOTE: returns [Some t] if [x:hyp] can be made into a [Transition_opt.t] *)
  let f : Rocq_utils.hyp -> Transition_opt.t option =
    fun (x : Rocq_utils.hyp) ->
    try
      let ty, tys = Rocq_utils.hyp_to_atomic sigma x in
      Some (get_transition sigma tys.(0) tys.(1) tys.(2) fsm)
    with
    | _ -> None
  in
  match List.filter_map f (Proofview.Goal.hyps gl) with
  | [] -> raise (Mebi_proof_CouldNotFindHypTransition (sigma, fsm, hyps))
  | h :: [] -> h
  | h :: _tl ->
    Log.debug
      (Printf.sprintf
         "Mebi_proof.get_hyp_lts_transition, multiple transitions found (%i)"
         (List.length (h :: _tl)));
    h
;;

let get_mtransition (gl : Proofview.Goal.t) : Transition_opt.t =
  log_trace __FUNCTION__;
  get_hyp_transition gl (mfsm ()) (Proofview.Goal.hyps gl)
;;

let get_weak_transition
      (sigma : Evd.evar_map)
      (fsm : Fsm.t)
      ((ty, tys) : Rocq_utils.kind_pair)
  : Transition_opt.t
  =
  log_trace __FUNCTION__;
  if typ_is_weak_transition sigma (ty, tys)
  then get_transition sigma tys.(3) tys.(5) tys.(4) fsm
  else raise (Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)))
;;

let get_weak_ntransition (sigma : Evd.evar_map) (wk_trans : EConstr.t)
  : Transition_opt.t
  =
  log_trace __FUNCTION__;
  Rocq_utils.econstr_to_atomic sigma wk_trans
  |> get_weak_transition sigma (nfsm ~saturated:true ())
;;

let _get_silent_transition
      (sigma : Evd.evar_map)
      (fsm : Fsm.t)
      ((ty, tys) : Rocq_utils.kind_pair)
  : Transition_opt.t
  =
  log_trace __FUNCTION__;
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
  log_trace __FUNCTION__;
  if typ_is_silent1_transition sigma (ty, tys)
  then get_transition sigma tys.(2) tys.(4) tys.(3) fsm
  else raise (Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)))
;;

let _get_concl_ntransition
      (sigma : Evd.evar_map)
      ((ty, tys) : Rocq_utils.kind_pair)
  : Transition_opt.t
  =
  log_trace __FUNCTION__;
  Log.debug "trying weak";
  try get_weak_transition sigma (nfsm ~saturated:true ()) (ty, tys) with
  | Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)) ->
    Log.debug "trying silent";
    (try _get_silent_transition sigma (nfsm ~saturated:true ()) (ty, tys) with
     | Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)) ->
       Log.debug "trying silent1";
       _get_silent1_transition sigma (nfsm ~saturated:true ()) (ty, tys))
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
    try find_state sigma tys.(5) (mfsm ()).states with
    | Mebi_proof_CouldNotDecodeState (sigma, statety, states) ->
      raise (Mebi_proof_CouldNotGetStateM (sigma, (ty, tys))))
  else raise (Mebi_proof_CouldNotGetStateM (sigma, (ty, tys)))
;;

let get_mstate (sigma : Evd.evar_map) (wk_sim : EConstr.t) : State.t =
  log_trace __FUNCTION__;
  weak_sim_get_m_state sigma (Rocq_utils.econstr_to_atomic sigma wk_sim)
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
    try Some (of_hty sigma p) with Hyp.Mebi_proof_Hypothesis_HTy _ -> None
  ;;

  let hty_is_a (sigma : Evd.evar_map) (p : Rocq_utils.kind_pair) : bool =
    Option.has_some (opt_of_hty sigma p)
  ;;

  let of_hyp (sigma : Evd.evar_map) (h : Rocq_utils.hyp) : t =
    try
      { kind = of_hty sigma (Rocq_utils.hyp_to_atomic sigma h)
      ; tactic = do_inversion h
      }
    with
    | Hyp.Mebi_proof_Hypothesis_HTy (sigma, p) ->
      raise (Hyp.Mebi_proof_Hypothesis_Hyp (sigma, h, p))
  ;;

  let opt_of_hyp (sigma : Evd.evar_map) (h : Rocq_utils.hyp) : t option =
    try Some (of_hyp sigma h) with Hyp.Mebi_proof_Hypothesis_Hyp _ -> None
  ;;

  let hyp_is_a (sigma : Evd.evar_map) (h : Rocq_utils.hyp) : bool =
    Option.has_some (opt_of_hyp sigma h)
  ;;
end

module TransOpt : Hyp.HYP_TYPE = Hyp.Make (struct
    type t = Transition_opt.t

    let of_hty (sigma : Evd.evar_map) ((ty, tys) : Rocq_utils.kind_pair) : t =
      try get_lts_transition sigma (mfsm ()) (ty, tys) with
      | Mebi_proof_CouldNotDecodeTransitionState (sigma, x, fsm) ->
        raise (Hyp.Mebi_proof_Hypothesis_HTy (sigma, (ty, tys)))
      | Mebi_proof_CouldNotDecodeTransitionLabel (sigma, x, fsm) ->
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
  log_trace "hyp_is_something";
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
  log_trace __FUNCTION__;
  let _, _, constr = Rocq_utils.econstr_to_lambda sigma tys.(1) in
  let _, apptys = Rocq_utils.econstr_to_app sigma constr in
  match Array.to_list apptys with
  | [ wk_trans; wk_sim ] ->
    (try
       let ntransition : Transition_opt.t =
         get_weak_ntransition sigma wk_trans
       in
       let mstate : State.t = get_mstate sigma wk_sim in
       ntransition, mstate
     with
     | Mebi_proof_CouldNotGetStateM (sigma, (ty, tys)) ->
       Log.debug
         (Printf.sprintf
            "Mebi_proof.concl_get_eexists Mebi_proof_CouldNotGetStateM");
       raise (Mebi_proof_ConclIsNot_Exists (sigma, (ty, tys)))
     | Rocq_utils.Rocq_utils_EConstrIsNot_Atomic (sigma, x, k) ->
       Log.debug
         (Printf.sprintf
            "Mebi_proof.concl_get_eexists Rocq_utils_EConstrIsNot_Atomic");
       raise (Mebi_proof_ConclIsNot_Exists (sigma, (ty, tys)))
     | Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)) ->
       Log.debug
         (Printf.sprintf
            "Mebi_proof.concl_get_eexists Mebi_proof_TyDoesNotMatchTheories");
       raise (Mebi_proof_ConclIsNot_Exists (sigma, (ty, tys))))
  | _ ->
    Log.debug
      (Printf.sprintf
         "Mebi_proof.concl_get_eexists array not [wk_trans;wk_sim]");
    raise (Mebi_proof_ConclIsNot_Exists (sigma, (ty, tys)))
;;

let hyp_is_invertible (sigma : Evd.evar_map) : Rocq_utils.hyp -> bool =
  fun (x : Rocq_utils.hyp) ->
  try Invertible.hyp_is_a sigma x with
  | Hyp.Mebi_proof_Hypothesis_Hyp _ -> false
;;

let hyps_get_invertibles (sigma : Evd.evar_map)
  : Rocq_utils.hyp list -> Invertible.t list
  =
  List.filter_map (fun (x : Rocq_utils.hyp) ->
    try Some (Invertible.of_hyp sigma x) with
    | Hyp.Mebi_proof_Hypothesis_Hyp _ -> None)
;;

let hyps_get_invertible (invertibles : Invertible.t list) : Invertible.t =
  let fully_invertibles : Invertible.t list =
    List.filter
      (function ({ kind = Full; _ } : Invertible.t) -> true | _ -> false)
      invertibles
  in
  List.hd
    (if List.is_empty fully_invertibles then invertibles else fully_invertibles)
;;

let do_hyp_inversion (gl : Proofview.Goal.t) : tactic =
  (hyps_get_invertibles (Proofview.Goal.sigma gl) (Proofview.Goal.hyps gl)
   |> hyps_get_invertible)
    .tactic
;;

(* let find_action_labelled (label:Label.t) (from:State.t) (fsm:Fsm.t) : Action.t =
   (Edges.find fsm.edges from)
   |> Model.get *)
(* 
let is_action_saturated : Action.t -> bool = function
  | { annotations; _ } -> Note.annotations_is_empty annotations
;; *)

exception Mebi_proof_NGotoNotInFsm of unit
exception Mebi_proof_NGoto_AlreadySome of State.t

let try_get_ngoto ?(saturated : bool = false) (mgoto : State.t)
  : Transition_opt.t -> State.t
  =
  log_trace __FUNCTION__;
  (* let prefix : string -> string = Printf.sprintf "%s %s" __FUNCTION__ in *)
  function
  | { from = nfrom; label = nlabel; goto = None; _ } ->
    (* Debug.thing (prefix "mgoto") mgoto (A State.to_string); *)
    (* Debug.thing (prefix "nfrom") nfrom (A State.to_string); *)
    (* Debug.thing (prefix "nlabel") nlabel (A Label.to_string); *)
    (try
       let nactions : States.t Actions.t =
         Edges.find (nfsm ~saturated ()).edges nfrom
       in
       Model.get_action_labelled ~annotated:saturated nlabel nactions
       |> Actions.find nactions
       |> States.inter (Model.get_bisim_states mgoto (the_bisim_states ()))
       |> States.min_elt
     with
     | Model_NoActionLabelled (annotated, label, actions) ->
       raise (Mebi_proof_NGotoNotInFsm ()))
  (* ! shouldn't happen *)
  | { goto = Some ngoto; _ } ->
    Log.debug
      (Printf.sprintf
         "Mebi_proof.try_get_goto, already have Some goto:\nis bisim: %b\n%s"
         (are_states_bisimilar mgoto ngoto)
         (State.to_string ngoto));
    raise (Mebi_proof_NGoto_AlreadySome ngoto)
;;

let get_ngoto (mgoto : State.t) (ntransition : Transition_opt.t) : State.t =
  log_trace __FUNCTION__;
  try try_get_ngoto ~saturated:false mgoto ntransition with
  | Mebi_proof_NGotoNotInFsm () ->
    log_tracex [ __FUNCTION__; "(saturated)" ];
    try_get_ngoto ~saturated:true mgoto ntransition
;;

let do_ex_intro (gl : Proofview.Goal.t) (ngoto : State.t) : tactic =
  log_trace __FUNCTION__;
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

let assert_states_consistent (mstate : State.t) : State.t option -> unit
  = function
  | None -> raise (Mebi_proof_ExIntro_NEqStateM (mstate, None))
  | Some mgoto ->
    if State.equal mstate mgoto
    then ()
    else raise (Mebi_proof_ExIntro_NEqStateM (mstate, Some mgoto))
;;

exception
  Mebi_proof_ExIntro_Transitions of (Transition_opt.t * Transition_opt.t)

exception Mebi_proof_ExIntro_NEqLabel of (Transition_opt.t * Transition_opt.t)

let assert_transition_opt_labels_eq
      (mtransition : Transition_opt.t)
      (ntransition : Transition_opt.t)
  : unit
  =
  if Label.equal mtransition.label ntransition.label
  then ()
  else raise (Mebi_proof_ExIntro_NEqLabel (mtransition, ntransition))
;;

exception Mebi_proof_ExIntro_NotBisimilar of (State.t * State.t)

let do_eexists_transition (gl : Proofview.Goal.t) : tactic =
  log_trace __FUNCTION__;
  let prefix : string -> string = Printf.sprintf "%s %s" __FUNCTION__ in
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  try
    let mtransition : Transition_opt.t = get_mtransition gl in
    (* TODO:

       if Label.is_silent mtrans.label
       then do_refl_none gl
       else
    *)
    let ((ntransition, mstate) : Transition_opt.t * State.t) =
      Proofview.Goal.concl gl
      |> Rocq_utils.econstr_to_atomic sigma
      |> concl_get_eexists sigma
    in
    Debug.thing (prefix "mtransition") mtransition (A Transition_opt.to_string);
    Debug.thing (prefix "ntransition") ntransition (A Transition_opt.to_string);
    assert_transition_opt_labels_eq mtransition ntransition;
    assert_transition_opt_states_bisimilar mtransition ntransition;
    assert_states_consistent mstate mtransition.goto;
    match mtransition.goto, ntransition.goto with
    | Some mgoto, None ->
      Debug.thing (prefix "mstate") mstate (A State.to_string);
      set_the_proof_state __FUNCTION__ (GoalTransition mtransition);
      get_ngoto mgoto ntransition |> do_ex_intro gl
    | _, _ -> raise (Mebi_proof_ExIntro_Transitions (mtransition, ntransition))
  with
  | Mebi_proof_StatesNotBisimilar (mstate, nstate, bisim_states) ->
    raise (Mebi_proof_ExIntro_NotBisimilar (mstate, nstate))
  | Mebi_proof_TransitionOptStatesNotBisimilar (mstate, nstate, bisim_states) ->
    raise (Mebi_proof_ExIntro_NotBisimilar (mstate, nstate))
;;

(***********************************************************************)

let hyps_has_cofix (sigma : Evd.evar_map) (concl : EConstr.t)
  : Rocq_utils.hyp list -> bool
  =
  log_trace __FUNCTION__;
  List.exists (fun (h : Rocq_utils.hyp) ->
    Mebi_setup.Eq.econstr sigma concl (Context.Named.Declaration.get_type h))
;;

let hyps_has_invertible (sigma : Evd.evar_map) : Rocq_utils.hyp list -> bool =
  log_trace __FUNCTION__;
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
  log_trace __FUNCTION__;
  if hyps_is_empty gl
  then (
    set_the_proof_state __FUNCTION__ NewWeakSim;
    handle_proof_state gl)
  else (
    Log.warning "New Proof: Expected Hypothesis to be empty";
    raise (Mebi_proof_NewProof ()))

and handle_new_weak_sim (gl : Proofview.Goal.t) : tactic =
  log_trace __FUNCTION__;
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let the_concl : EConstr.t = Proofview.Goal.concl gl in
  let concltyp : Rocq_utils.kind_pair =
    Rocq_utils.econstr_to_atomic sigma the_concl
  in
  (* NOTE: if concl is [weak_sim] and hyp has corresponding [cofix] then [auto]. *)
  if typ_is_weak_sim sigma concltyp
  then
    if hyps_has_cofix sigma the_concl (Proofview.Goal.hyps gl)
    then do_solve_cofix gl
    else (
      set_the_proof_state __FUNCTION__ NewCofix;
      do_new_cofix gl)
  else if typ_is_exists sigma concltyp
  then (
    set_the_proof_state __FUNCTION__ NewCofix;
    handle_proof_state gl)
  else (
    Log.warning
      (Printf.sprintf
         "Mebi_proof.handle_new_weak_sim, concl is not weak_sim or has eexists.\n\
          %s"
         (concl_to_string gl));
    raise (Mebi_proof_NewWeakSim ()))

and handle_new_cofix (gl : Proofview.Goal.t) : tactic =
  log_trace __FUNCTION__;
  let prefix : string -> string = Printf.sprintf "%s %s" __FUNCTION__ in
  try
    if hyps_has_invertible (Proofview.Goal.sigma gl) (Proofview.Goal.hyps gl)
    then do_hyp_inversion gl
    else do_eexists_transition gl
  with
  (* NOTE: exception handling: *)
  | Mebi_proof_CouldNotFindHypTransition (sigma, fsm, hyps) ->
    Log.warning
      (Printf.sprintf
         "Could not find any transitions in Hyps: %s"
         (hyps_to_string gl));
    raise (Mebi_proof_NewCofix ())
  | Mebi_proof_CouldNotDecodeTransitionState (sigma, x, fsm) ->
    Log.warning
      (Printf.sprintf
         "Could not decode transition state: %s"
         (econstr_to_string gl x));
    Debug.thing (prefix "fsm.states") fsm.states (A Model.states_to_string);
    raise (Mebi_proof_NewCofix ())
  | Mebi_proof_CouldNotDecodeTransitionLabel (sigma, x, fsm) ->
    Log.warning
      (Printf.sprintf
         "Could not decode transition label: %s"
         (econstr_to_string gl x));
    Debug.thing
      (prefix "fsm.alphabet")
      fsm.alphabet
      (A Model.alphabet_to_string);
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
    Debug.thing (prefix "ExIntro_NEqStateM, mstate") mstate (A State.to_string);
    Debug.option (prefix "ExIntro_NEqStateM, mfrom") mfrom (A State.to_string);
    raise (Mebi_proof_NewCofix ())
  | Mebi_proof_ExIntro_NEqLabel (mtransition, ntransition) ->
    Log.warning "Mebi_proof_ExIntro_NEqLabel";
    Debug.thing
      (prefix "ExIntro_NEqLabel, mtransition")
      mtransition.label
      (A Label.to_string);
    Debug.thing
      (prefix "ExIntro_NEqLabel, ntransition")
      ntransition.label
      (A Label.to_string);
    raise (Mebi_proof_NewCofix ())

and handle_goal_transition (gl : Proofview.Goal.t) (mtrans : Transition_opt.t)
  : tactic
  =
  log_trace __FUNCTION__;
  let prefix : string -> string = Printf.sprintf "%s %s" __FUNCTION__ in
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  Debug.thing (prefix "mtrans") mtrans (A Transition_opt.to_string);
  Debug.thing
    (prefix "concl")
    (Proofview.Goal.concl gl)
    (B (econstr_to_string gl));
  try
    let concltys =
      Proofview.Goal.concl gl |> Rocq_utils.econstr_to_atomic sigma
    in
    let { from = nfrom; label = nlabel; goto = ngoto; _ } : Transition_opt.t =
      try get_weak_transition sigma (nfsm ~saturated:true ()) concltys with
      | Mebi_proof_TyDoesNotMatchTheories _ ->
        _get_silent_transition sigma (nfsm ~saturated:true ()) concltys
    in
    (* let { from = nfrom; label = nlabel; goto = ngoto; _ } : Transition_opt.t =
      Proofview.Goal.concl gl
      |> Rocq_utils.econstr_to_atomic sigma
      |> get_weak_transition sigma (nfsm ~saturated:true ())
    in *)
    match mtrans.goto, ngoto with
    | Some mgoto, Some ngoto ->
      assert_states_bisimilar mtrans.from ngoto;
      Debug.thing (prefix "mgoto") mgoto (A State.to_string);
      Debug.thing (prefix "ngoto") ngoto (A State.to_string);
      Debug.thing (prefix "nlabel") nlabel (A Label.to_string);
      (if Label.is_silent nlabel
       then do_apply_wk_none gl
       else do_eapply_wk_some gl)
      |> do_constructor_transition gl nfrom nlabel
    | _, _ -> raise (Mebi_proof_GoalTransition ())
  with
  | Mebi_proof_CouldNotDecodeTransitionState (sigma, x, fsm) ->
    Log.warning
      (Printf.sprintf
         "Could not decode transition state: %s"
         (econstr_to_string gl x));
    Debug.thing (prefix "fsm.states") fsm.states (A Model.states_to_string);
    raise (Mebi_proof_GoalTransition ())
  | Mebi_proof_CouldNotDecodeTransitionLabel (sigma, x, fsm) ->
    Log.warning
      (Printf.sprintf
         "Could not decode transition label: %s"
         (econstr_to_string gl x));
    Debug.thing
      (prefix "fsm.alphabet")
      fsm.alphabet
      (A Model.alphabet_to_string);
    raise (Mebi_proof_GoalTransition ())
  | Mebi_proof_StatesNotBisimilar (mstate, nstate, pi) ->
    Debug.thing (prefix "StatesNotBisimilar, mfrom") mstate (A State.to_string);
    Debug.thing (prefix "StatesNotBisimilar, ngoto") nstate (A State.to_string);
    raise (Mebi_proof_GoalTransition ())
  | Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)) ->
    Debug.thing
      (prefix "Mebi_proof_TyDoesNotMatchTheories")
      ty
      (B (econstr_to_string gl));
    Array.iter
      (fun ty -> Debug.thing (prefix "tys arg") ty (B (econstr_to_string gl)))
      tys;
    raise (Mebi_proof_GoalTransition ())

and handle_apply_constructors (gl : Proofview.Goal.t)
  : PState.applicable_constructors -> tactic
  =
  log_trace __FUNCTION__;
  function
  | { annotation = None; tactics } ->
    log_tracex [ __FUNCTION__; "annotation None" ];
    (match tactics with
     | Some (h :: tl) ->
       log_tracex [ __FUNCTION__; "tactics Some (h::t)" ];
       set_the_proof_state
         __FUNCTION__
         (ApplyConstructors { annotation = None; tactics = Some tl });
       h
     | _ ->
       log_tracex [ __FUNCTION__; "tactics empty" ];
       set_the_proof_state __FUNCTION__ NewWeakSim;
       tactic_chain [ do_simplify gl; do_eapply_rt1n_refl gl ])
  | { annotation = Some annotation; tactics } ->
    log_tracex [ __FUNCTION__; "annotation Some" ];
    (match tactics with
     | None ->
       log_tracex [ __FUNCTION__; "tactics None" ];
       do_build_constructor_tactics gl annotation
     | Some [] ->
       log_tracex [ __FUNCTION__; "tactics Some []" ];
       tactic_chain
         [ do_simplify gl; do_build_constructor_tactics gl annotation ]
     | Some (h :: tl) ->
       log_tracex [ __FUNCTION__; "tactics Some (h::t)" ];
       set_the_proof_state
         __FUNCTION__
         (ApplyConstructors { annotation = Some annotation; tactics = Some tl });
       h)

and handle_proof_state (gl : Proofview.Goal.t) : tactic =
  log_trace __FUNCTION__;
  let prefix : string -> string = Printf.sprintf "%s %s" __FUNCTION__ in
  try
    match !the_proof_state with
    | NewProof -> handle_new_proof gl
    | NewWeakSim -> handle_new_weak_sim gl
    | NewCofix -> handle_new_cofix gl
    | GoalTransition mtransition -> handle_goal_transition gl mtransition
    | ApplyConstructors napplicable_constructors ->
      Log.debug (PState.to_string ~short:false !the_proof_state);
      handle_apply_constructors gl napplicable_constructors
    | DetectState -> detect_proof_state gl
  with
  | Mebi_proof_CouldNotDecodeEConstr (sigma, x) ->
    Debug.thing (prefix "econstr") x (B (econstr_to_string gl));
    raise (Mebi_proof_CouldNotDecodeEConstr (sigma, x))

and detect_proof_state (gl : Proofview.Goal.t) : tactic =
  log_trace __FUNCTION__;
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let the_concl : EConstr.t = Proofview.Goal.concl gl in
  let the_hyps : Rocq_utils.hyp list = Proofview.Goal.hyps gl in
  let concltyp : Rocq_utils.kind_pair =
    Rocq_utils.econstr_to_atomic sigma the_concl
  in
  if typ_is_weak_sim sigma concltyp && hyps_has_cofix sigma the_concl the_hyps
  then do_solve_cofix gl
  else (
    (* do_nothing () *)
    set_the_proof_state __FUNCTION__ NewCofix;
    handle_proof_state gl)
;;

(***********************************************************************)
(*** Solve Proof *******************************************************)
(***********************************************************************)

let step () : unit Proofview.tactic =
  log_trace __FUNCTION__;
  Mebi_theories.tactics
    [ Proofview.Goal.enter (fun gl ->
        get_tactic ~short:false (handle_proof_state gl))
    ; Mebi_tactics.simplify_and_subst_all ()
    ]
;;

let solve (upper_bound : int) (pstate : Declare.Proof.t) : Declare.Proof.t =
  log_trace __FUNCTION__;
  let rec iter_body (n : int) (pstate : Declare.Proof.t) : int * Declare.Proof.t
    =
    match Proof.is_done (Declare.Proof.get pstate), Int.compare n 0 with
    | true, _ ->
      Log.notice (Printf.sprintf "Solved in (%i) iterations." (upper_bound - n));
      n, pstate
    | false, -1 ->
      Log.notice
        (Printf.sprintf "Unsolved after (%i) iterations." (upper_bound - n - 1));
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
