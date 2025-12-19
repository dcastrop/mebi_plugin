open Mebi_wrapper
open Model

(* open Debug *)
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

(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.MkDefault ()

let () = Log.Config.configure_output Debug true
let () = Log.Config.configure_output Trace true
(***********************************************************************)

(* let fstr : string Logger.to_string = Args Utils.Strfy.string *)

let feconstr (gl : Proofview.Goal.t) : EConstr.t Logger.to_string =
  Of (econstr_to_string gl)
;;

let fhyp (gl : Proofview.Goal.t) : Rocq_utils.hyp Logger.to_string =
  Of (hyp_to_string gl)
;;

let fstate : State.t Logger.to_string = Args State.to_string
let fstates : States.t Logger.to_string = Args Model.states_to_string
let fpi : Partition.t Logger.to_string = Args Model.partition_to_string
let flabel : Label.t Logger.to_string = Args Label.to_string
let falphabet : Alphabet.t Logger.to_string = Args Model.alphabet_to_string
(* let faction : Action.t Logger.to_string = Args Action.to_string *)

(***********************************************************************)

(***********************************************************************)
(*** Proof State *******************************************************)
(***********************************************************************)

type tactic =
  { msg : string option
  ; x : unit Proofview.tactic
  }

let tactic ?(msg : string option) (x : unit Proofview.tactic) : tactic =
  Log.trace __FUNCTION__;
  { msg; x }
;;

let tactic_chain : tactic list -> tactic =
  Log.trace __FUNCTION__;
  function
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
    | DoRefl
    | GoalTransition of transitions
    | ApplyConstructors of applicable_constructors
    | DetectState

  (* TODO: maybe revert to just [mtrans:Transition.t]*)
  and transitions =
    { mtrans : Transition.t
    ; ntrans : Transition_opt.t
    }

  and applicable_constructors =
    { annotation : Note.annotation option
    ; tactics : tactic list option
    ; goto : State.t
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
    | DoRefl -> "DoRefl"
    | GoalTransition { mtrans; ntrans } ->
      Printf.sprintf
        "GoalTransition%s"
        (if short
         then ""
         else
           Printf.sprintf
             ":\nmtrans: %s\n\nntrans: %s\n"
             (Transition.to_string mtrans)
             (Transition_opt.to_string ntrans))
    | ApplyConstructors { annotation; tactics; goto } ->
      Printf.sprintf
        "ApplyConstructors%s"
        (if short
         then ""
         else
           Printf.sprintf
             ":\nGoto: %s\nAnnotation: %s\nTactics: %s"
             (State.to_string goto)
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
  Log.trace __FUNCTION__;
  the_proof_state := default_proof_state;
  the_old_proof_states := []
;;

let the_old_proof_states_to_string ?(short : bool = true) () : string =
  Log.trace __FUNCTION__;
  let open Utils.Strfy in
  list
    (list (fun ?(args : style_args = style_args ()) p ->
       PState.to_string ~short p))
    !the_old_proof_states
;;

let update_old_proof_states (x : PState.t) : unit =
  Log.trace __FUNCTION__;
  match !the_old_proof_states with
  | [] -> the_old_proof_states := [ [ x ] ]
  | [ [] ] -> the_old_proof_states := [ [ x ] ]
  | h :: tl -> the_old_proof_states := (x :: h) :: tl
;;

let set_the_proof_state
      ?(short : bool = true)
      ?(__FUNCTION__ : string = "")
      (x : PState.t)
  : unit
  =
  let f = PState.to_string ~short:true in
  let s : string = Printf.sprintf "(%s) -> (%s)" (f !the_proof_state) (f x) in
  Log.thing ~__FUNCTION__ Debug "new state" x (Of (PState.to_string ~short));
  Log.thing ~__FUNCTION__ Debug "change" s (Args Utils.Strfy.string);
  update_old_proof_states !the_proof_state;
  the_proof_state := x
;;

let _debug_proof_state ?(short : bool = true) () : unit =
  Log.trace __FUNCTION__;
  Log.debug
    (Printf.sprintf
       "Current: %s\n\n\
        Newest: (empty after MewWeakSim indicates Inversion)%s(Oldest)\n"
       (PState.to_string ~short !the_proof_state)
       (the_old_proof_states_to_string ~short ()))
;;

let get_tactic ?(short : bool = true) ?(state : bool = true)
  : tactic -> unit Proofview.tactic
  =
  Log.trace __FUNCTION__;
  function
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

let _warn_model_action_hasnoannotation (naction : Action.t) : unit =
  Log.warning
    (Printf.sprintf
       "Model_Action_HasNoAnnotations:\n%s"
       (Action.to_string naction))
;;

let warn_handle_new_weak_sim (gl : Proofview.Goal.t) : unit =
  Log.thing
    ~__FUNCTION__
    Warning
    "concl is not weak_sim or has eexists.\n"
    gl
    (Of concl_to_string)
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
  : Transition.t -> Transition_opt.t -> unit
  =
  Log.trace __FUNCTION__;
  try
    function
    | { from = mfrom; goto = mgoto; _ } ->
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

let get_actions
      ?(annotated : bool = false)
      (nfrom : State.t)
      (nlabel : Label.t)
      (nfsm : Fsm.t)
  : Action.t list
  =
  Log.trace __FUNCTION__;
  Log.thing ~__FUNCTION__ Debug "nfrom" nfrom (Args State.to_string);
  Log.thing ~__FUNCTION__ Debug "nlabel" nlabel (Args Label.to_string);
  Model.get_actions_labelled_from ~annotated nfrom nlabel nfsm.edges
;;

(* let _get_constructor_annotation (nfrom : State.t) (nlabel : Label.t)
   : Note.annotation
   =
   Log.trace __FUNCTION__;
   try
   get_naction ~annotated:false nfrom nlabel (nfsm ~saturated:false ())
   |> Model.get_shortest_annotation_from nfrom
   with
   | Model_NoActionLabelledFrom (annotated, from, label, edges) ->
   get_naction ~annotated:true nfrom nlabel (nfsm ~saturated:true ())
   |> Model.get_shortest_annotation_from nfrom
   ;;

   let _get_annotation_constructor (nfrom : State.t) (nlabel : Label.t)
   : Tree.node list
   =
   Log.trace __FUNCTION__;
   get_naction ~annotated:true nfrom nlabel (nfsm ~saturated:false ())
   |> Model.get_shortest_constructor
   ;; *)

(* let get_annotation_constructor
      ({ this = { from; via }; next } : Note.annotation)
  : Tree.node list
  =
  Log.trace __FUNCTION__;
  (* get_naction ~annotated:true nfrom nlabel (nfsm ~saturated:false ())
     |> Model.get_shortest_constructor *)
  match next with None -> [] | Some { this = { from = goto; _ }; _ } -> []
;; *)

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
    ; clear_old_hyps gl
    ; do_apply_In_sim gl
    ; do_apply_Pack_sim gl
    ; do_intros_all ()
    ]
;;

let do_inversion (h : Rocq_utils.hyp) : tactic =
  tactic ~msg:"(do_inversion)" (Mebi_tactics.do_inversion h)
;;

let do_simplify (gl : Proofview.Goal.t) : tactic =
  tactic ~msg:"(do_simplify)" (Mebi_tactics.simplify_and_subst_all ~gl ())
;;

let do_apply_rt1n_refl (gl : Proofview.Goal.t) : tactic =
  Log.trace __FUNCTION__;
  tactic
    ~msg:"apply rt1n_refl"
    (Mebi_theories.tactics
       [ Mebi_tactics.apply ~gl (Mebi_theories.c_rt1n_refl ()) ])
;;

let do_eapply_rt1n_refl (gl : Proofview.Goal.t) : tactic =
  Log.trace __FUNCTION__;
  tactic
    ~msg:"eapply rt1n_refl"
    (Mebi_theories.tactics
       [ Mebi_tactics.eapply ~gl (Mebi_theories.c_rt1n_refl ()) ])
;;

let _do_apply_rt1n_trans (gl : Proofview.Goal.t) : tactic =
  Log.trace __FUNCTION__;
  tactic
    ~msg:"apply rt1n_trans"
    (Mebi_theories.tactics
       [ Mebi_tactics.apply ~gl (Mebi_theories.c_rt1n_trans ()) ])
;;

let do_eapply_rt1n_trans (gl : Proofview.Goal.t) : tactic =
  Log.trace __FUNCTION__;
  tactic
    ~msg:"eapply rt1n_trans"
    (Mebi_theories.tactics
       [ Mebi_tactics.eapply ~gl (Mebi_theories.c_rt1n_trans ()) ])
;;

let do_rt1n_via (gl : Proofview.Goal.t) (via : Label.t) : tactic =
  Log.trace __FUNCTION__;
  tactic_chain
    [ do_simplify gl
    ; (if Label.is_silent via
       then do_eapply_rt1n_trans gl
       else do_eapply_rt1n_refl gl)
    ; do_simplify gl
    ]
;;

let do_solve_cofix (gl : Proofview.Goal.t) : tactic =
  Log.trace __FUNCTION__;
  set_the_proof_state ~__FUNCTION__ DetectState;
  tactic ~msg:"(do_solve)" (Auto.gen_trivial ~debug:Hints.Info [] None)
;;

let do_apply_wk_none (gl : Proofview.Goal.t) : tactic =
  Log.trace __FUNCTION__;
  tactic
    ~msg:"apply wk_none"
    (Mebi_tactics.apply ~gl (Mebi_theories.c_wk_none ()))
;;

let do_unfold_silent (gl : Proofview.Goal.t) : tactic =
  Log.trace __FUNCTION__;
  tactic
    ~msg:"unfold silent"
    (Mebi_tactics.unfold_econstr gl (Mebi_theories.c_silent ()))
;;

let do_eapply_wk_some (gl : Proofview.Goal.t) : tactic =
  Log.trace __FUNCTION__;
  tactic
    ~msg:"eapply wk_some"
    (Mebi_tactics.eapply ~gl (Mebi_theories.c_wk_some ()))
;;

exception Mebi_proof_NoActionFound of (State.t * Label.t * State.t * Fsm.t)

let get_action_to
      ?(annotated : bool = false)
      (from : State.t)
      (via : Label.t)
      (goto : State.t)
      (fsm : Fsm.t)
  : Action.t
  =
  Log.trace __FUNCTION__;
  let actionsfrom : States.t Actions.t = Edges.find fsm.edges from in
  let actions = get_actions ~annotated from via fsm in
  Log.things ~__FUNCTION__ Debug "actions" actions (Args Action.to_string);
  Log.thing ~__FUNCTION__ Debug "from" from (Args State.to_string);
  Log.thing ~__FUNCTION__ Debug "label" via (Args Label.to_string);
  Log.thing ~__FUNCTION__ Debug "goto" goto (Args State.to_string);
  match
    List.find_opt
      (fun (x : Action.t) ->
        let destinations = Actions.find actionsfrom x in
        Log.thing ~__FUNCTION__ Debug "action" x (Args Action.to_string);
        Log.thing
          ~__FUNCTION__
          Debug
          "destinations"
          destinations
          (Args Model.states_to_string);
        States.mem goto destinations)
      actions
  with
  | Some action -> action
  | None -> raise (Mebi_proof_NoActionFound (from, via, goto, fsm))
;;

(* let get_naction_to (from : State.t) (via : Label.t) (goto : State.t) : Action.t =
   Log.trace __FUNCTION__;
   try
   get_action_to ~annotated:false from via goto (nfsm ~saturated:false ())
   with
   | Model_NoActionLabelledFrom (annotated, from, label, edges) ->
   get_action_to ~annotated:true from via goto (nfsm ~saturated:true ())
   | Mebi_proof_NoActionFound (from, via, goto, fsm) ->
   get_action_to ~annotated:true from via goto (nfsm ~saturated:true ())
   ;; *)

let do_constructor_transition
      (gl : Proofview.Goal.t)
      (nfrom : State.t)
      (nlabel : Label.t)
      (goto : State.t)
  : tactic
  =
  Log.trace __FUNCTION__;
  let action : Action.t =
    get_action_to ~annotated:true nfrom nlabel goto (nfsm ~saturated:true ())
  in
  Log.thing ~__FUNCTION__ Debug "action" action (Args Action.to_string);
  let annotation : Note.annotation = Action.annotation action in
  Log.thing
    ~__FUNCTION__
    Debug
    "annotation"
    annotation
    (Args Note.annotation_to_string);
  set_the_proof_state
    ~__FUNCTION__
    (ApplyConstructors { annotation = Some annotation; tactics = None; goto });
  tactic_chain
    [ (if Label.is_silent nlabel
       then do_apply_wk_none gl
       else do_eapply_wk_some gl)
    ; do_unfold_silent gl
    ]
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
   set_the_proof_state ~__FUNCTION__  NewWeakSim;
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
  Log.trace __FUNCTION__;
  let index : int = index + 1 in
  tactic
    ~msg:(Printf.sprintf "constructor %i" index)
    (Tactics.one_constructor index Tactypes.NoBindings)
;;

let do_build_constructor_tactics
      (gl : Proofview.Goal.t)
      (destination : State.t)
      ({ this = { from; via; using; goto }; next } : Note.annotation)
  : tactic
  =
  Log.trace __FUNCTION__;
  Log.thing ~__FUNCTION__ Debug "from" from (Args State.to_string);
  Log.thing ~__FUNCTION__ Debug "via" via (Args Label.to_string);
  Log.thing ~__FUNCTION__ Debug "goto" goto (Args State.to_string);
  Log.thing ~__FUNCTION__ Debug "using" using (Args Tree.list_to_string);
  Log.option ~__FUNCTION__ Debug "next" next (Args Note.annotation_to_string);
  (* let action : Action.t =
    (* let goto : State.t =
      Option.cata
        (fun ({ this = { from = goto; _ }; _ } : Note.annotation) -> goto)
        goto 
        next
    in *)
    get_action_to from via goto (nfsm ~saturated:false ())
  in *)
  let constructor : Tree.node list = Tree.min using in
  let tactics : tactic list = List.map get_constructor_tactic constructor in
  set_the_proof_state
    ~__FUNCTION__
    (ApplyConstructors { annotation = next; tactics = Some tactics; goto });
  do_rt1n_via gl via
;;

(* let sss = function
  | { this = { from; via }; next } ->
    Log.thing ~__FUNCTION__ Debug ("from") from (Args State.to_string);
    Log.thing ~__FUNCTION__ Debug ("via") via (Args Label.to_string);
    Log.option ~__FUNCTION__ Debug ("next") next (Args Note.annotation_to_string);
    let constructors : Tree.node list =
      get_annotation_constructor { this = { from; via }; next }
    in
    (* Log.option ~__FUNCTION__ Debug ("constructors") constructors (Args Tree.list_to_string ); *)
    let tactics : tactic list = List.map get_constructor_tactic constructors in
    Log.option ~__FUNCTION__ Debug ("next annotation") next (Args Note.annotation_to_string);
    set_the_proof_state
      __FUNCTION__
      (ApplyConstructors { annotation = next; tactics = Some tactics; goto });
    do_rt1n_via gl via
;; *)

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
  Log.trace __FUNCTION__;
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
  Log.trace __FUNCTION__;
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
  Log.trace __FUNCTION__;
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_weak_sim ())
;;

let typ_is_weak_transition
      (sigma : Evd.evar_map)
      ((ty, _) : Rocq_utils.kind_pair)
  : bool
  =
  Log.trace __FUNCTION__;
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_weak ())
;;

let typ_is_silent_transition
      (sigma : Evd.evar_map)
      ((ty, _) : Rocq_utils.kind_pair)
  : bool
  =
  Log.trace __FUNCTION__;
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_silent ())
;;

let typ_is_silent1_transition
      (sigma : Evd.evar_map)
      ((ty, _) : Rocq_utils.kind_pair)
  : bool
  =
  Log.trace __FUNCTION__;
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_silent1 ())
;;

let typ_is_lts_transition
      (sigma : Evd.evar_map)
      ((ty, _) : Rocq_utils.kind_pair)
  : bool
  =
  Log.trace __FUNCTION__;
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_LTS ())
;;

(***********************************************************************)

let try_find_state (sigma : Evd.evar_map) (x : EConstr.t) (states : States.t)
  : State.t option
  =
  Log.trace __FUNCTION__;
  Option.map (fun (y : Enc.t) -> decode_state y states) (try_decode sigma x)
;;

exception
  Mebi_proof_CouldNotDecodeState of (Evd.evar_map * EConstr.t * States.t)

let find_state (sigma : Evd.evar_map) (x : EConstr.t) (states : States.t)
  : State.t
  =
  Log.trace __FUNCTION__;
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
  Log.trace __FUNCTION__;
  Log.thing ~__FUNCTION__ Debug "Labels" labels (Args alphabet_to_string);
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
  Log.trace __FUNCTION__;
  match try_find_label sigma x labels with
  | Some s -> s
  | None -> raise (Mebi_proof_CouldNotDecodeLabel (sigma, x, labels))
;;

(***********************************************************************)

exception
  Mebi_proof_CouldNotDecodeTransitionState of (Evd.evar_map * EConstr.t * Fsm.t)

exception
  Mebi_proof_CouldNotDecodeTransitionLabel of (Evd.evar_map * EConstr.t * Fsm.t)

exception
  Mebi_proof_CouldNotObtainAction of
    (State.t * Label.t * State.t option * Fsm.t)

let get_transition
      ?(need_action : bool = true)
      (sigma : Evd.evar_map)
      (fromty : EConstr.t)
      (labelty : EConstr.t)
      (gototy : EConstr.t)
      (fsm : Fsm.t)
  : Transition_opt.t
  =
  Log.trace __FUNCTION__;
  try
    let from : State.t = find_state sigma fromty fsm.states in
    Log.thing ~__FUNCTION__ Debug "From" from (Args State.to_string);
    Log.thing
      ~__FUNCTION__
      Debug
      "Alphabet"
      fsm.alphabet
      (Args alphabet_to_string);
    let label : Label.t = find_label sigma labelty fsm.alphabet in
    Log.thing ~__FUNCTION__ Debug "Label" label (Args Label.to_string);
    let goto : State.t option = try_find_state sigma gototy fsm.states in
    Log.option ~__FUNCTION__ Debug "Goto" goto (Args State.to_string);
    let actions : States.t Actions.t = Edges.find fsm.edges from in
    Log.thing
      ~__FUNCTION__
      Debug
      "Actions"
      actions
      (Args action_labels_to_string);
    let (annotation, constructor_trees) : Note.annotation option * Tree.t list =
      (* get_action_labelled ~annotated:true label (Edges.find fsm.edges from) *)
      Option.cata
        (fun (goto : State.t) ->
          let { annotation; constructor_trees; _ } : Action.t =
            get_action_to ~annotated:true from label goto fsm
          in
          annotation, constructor_trees)
        (if need_action
         then raise (Mebi_proof_CouldNotObtainAction (from, label, goto, fsm))
         else None, [])
        goto
    in
    Transition_opt.create from label goto ~annotation ~constructor_trees ()
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
  Log.trace __FUNCTION__;
  if typ_is_lts_transition sigma (ty, tys)
  then get_transition sigma tys.(0) tys.(1) tys.(2) fsm
  else raise (Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)))
;;

exception
  Mebi_proof_CouldNotFindHypTransition of
    (Evd.evar_map * Fsm.t * Rocq_utils.hyp list)

let get_hyp_transition (gl : Proofview.Goal.t) (fsm : Fsm.t) : Transition_opt.t =
  Log.trace __FUNCTION__;
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  (* NOTE: returns [Some t] if [x:hyp] can be made into a [Transition_opt.t] *)
  let f : Rocq_utils.hyp -> Transition_opt.t option =
    fun (x : Rocq_utils.hyp) ->
    try
      let ty, tys = Rocq_utils.hyp_to_atomic sigma x in
      Some (get_transition ~need_action:false sigma tys.(0) tys.(1) tys.(2) fsm)
    with
    | Mebi_proof_CouldNotDecodeTransitionState (sigma, ty, fsm) ->
      Log.thing ~__FUNCTION__ Debug "Could not decode State" ty (feconstr gl);
      None
    | Mebi_proof_CouldNotDecodeTransitionLabel (sigma, ty, fsm) ->
      Log.thing ~__FUNCTION__ Debug "Could not decode Label" ty (feconstr gl);
      None
  in
  match List.filter_map f (Proofview.Goal.hyps gl) with
  | [] ->
    raise
      (Mebi_proof_CouldNotFindHypTransition (sigma, fsm, Proofview.Goal.hyps gl))
  | h :: [] -> h
  | h :: _tl ->
    let len : int = List.length (h :: _tl) in
    Args Utils.Strfy.int
    |> Log.thing ~__FUNCTION__ Debug "multiple transitions found" len;
    h
;;

exception Mebi_proof_ExpectedMTransition_Some of unit

let get_mtransition (gl : Proofview.Goal.t) : Transition.t =
  Log.trace __FUNCTION__;
  let { from; label; goto; annotation; constructor_trees } : Transition_opt.t =
    get_hyp_transition gl (mfsm ~saturated:false ())
  in
  match goto with
  | Some goto -> { from; label; goto; annotation; constructor_trees }
  | None -> raise (Mebi_proof_ExpectedMTransition_Some ())
;;

let get_weak_transition
      (sigma : Evd.evar_map)
      (fsm : Fsm.t)
      ((ty, tys) : Rocq_utils.kind_pair)
  : Transition_opt.t
  =
  Log.trace __FUNCTION__;
  if typ_is_weak_transition sigma (ty, tys)
  then get_transition ~need_action:false sigma tys.(3) tys.(5) tys.(4) fsm
  else raise (Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)))
;;

let get_weak_ntransition (sigma : Evd.evar_map) (wk_trans : EConstr.t)
  : Transition_opt.t
  =
  Log.trace __FUNCTION__;
  Rocq_utils.econstr_to_atomic sigma wk_trans
  |> get_weak_transition sigma (nfsm ~saturated:true ())
;;

let _get_silent_transition
      (sigma : Evd.evar_map)
      (fsm : Fsm.t)
      ((ty, tys) : Rocq_utils.kind_pair)
  : Transition_opt.t
  =
  Log.trace __FUNCTION__;
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
  Log.trace __FUNCTION__;
  if typ_is_silent1_transition sigma (ty, tys)
  then get_transition sigma tys.(2) tys.(4) tys.(3) fsm
  else raise (Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)))
;;

let _get_concl_ntransition
      (sigma : Evd.evar_map)
      ((ty, tys) : Rocq_utils.kind_pair)
  : Transition_opt.t
  =
  Log.trace __FUNCTION__;
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

  let of_hty (gl : Proofview.Goal.t) ((ty, tys) : Rocq_utils.kind_pair) : k =
    Log.trace __FUNCTION__;
    let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
    try
      if Mebi_theories.is_var sigma tys.(2)
      then (
        Log.trace ~__FUNCTION__ "Full";
        Full)
      else if Mebi_theories.is_var sigma tys.(1)
      then (
        Log.trace ~__FUNCTION__ "Layer";
        Layer)
      else (
        Log.trace ~__FUNCTION__ "(else)";
        raise (Hyp.Mebi_proof_Hypothesis_HTy (sigma, (ty, tys))))
    with
    | Invalid_argument _ ->
      Log.trace ~__FUNCTION__ "Invalid_argument";
      raise (Hyp.Mebi_proof_Hypothesis_HTy (sigma, (ty, tys)))
  ;;

  let opt_of_hty (gl : Proofview.Goal.t) (p : Rocq_utils.kind_pair) : k option =
    Log.trace __FUNCTION__;
    try Some (of_hty gl p) with Hyp.Mebi_proof_Hypothesis_HTy _ -> None
  ;;

  let hty_is_a (gl : Proofview.Goal.t) (p : Rocq_utils.kind_pair) : bool =
    Log.trace __FUNCTION__;
    Option.has_some (opt_of_hty gl p)
  ;;

  let of_hyp (gl : Proofview.Goal.t) (h : Rocq_utils.hyp) : t =
    Log.trace __FUNCTION__;
    let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
    try
      { kind = of_hty gl (Rocq_utils.hyp_to_atomic sigma h)
      ; tactic = do_inversion h
      }
    with
    | Hyp.Mebi_proof_Hypothesis_HTy (sigma, p) ->
      raise (Hyp.Mebi_proof_Hypothesis_Hyp (sigma, h, p))
  ;;

  let opt_of_hyp (gl : Proofview.Goal.t) (h : Rocq_utils.hyp) : t option =
    Log.trace __FUNCTION__;
    try Some (of_hyp gl h) with Hyp.Mebi_proof_Hypothesis_Hyp _ -> None
  ;;

  let hyp_is_a (gl : Proofview.Goal.t) (h : Rocq_utils.hyp) : bool =
    Log.trace __FUNCTION__;
    Log.thing ~__FUNCTION__ Debug "hyp" h (fhyp gl);
    Option.has_some (opt_of_hyp gl h)
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
      | Mebi_proof_CouldNotObtainAction (from, label, goto, fsm) ->
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
let hyp_is_something (gl : Proofview.Goal.t) (h : Rocq_utils.hyp) : bool =
  Log.trace __FUNCTION__;
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  try
    let p : Rocq_utils.kind_pair = Rocq_utils.hyp_to_atomic sigma h in
    if Cofix.hty_is_a sigma p
    then true
    else if Invertible.hty_is_a gl p
    then true
    else if TransOpt.hty_is_a sigma p
    then true
    else false
  with
  | Rocq_utils.Rocq_utils_HypIsNot_Atomic _ -> false
;;

(** is [true] if none of the hyps appear to be mid-way through a proof. *)
let hyps_is_essentially_empty (gl : Proofview.Goal.t) : bool =
  Bool.not
    (List.for_all
       (fun (h : Rocq_utils.hyp) -> hyp_is_something gl h)
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

(* type concl_wk_sim =
  { m : State.t
  ; n : State.t
  } *)

type concl_conj =
  { wk_trans : EConstr.t
  ; wk_sim : EConstr.t
  }

exception Mebi_proof_ConclIsNot_Conj of (Evd.evar_map * Rocq_utils.kind_pair)

let concl_wk_sim (gl : Proofview.Goal.t) : concl_conj =
  Log.trace __FUNCTION__;
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let ty, tys = Rocq_utils.econstr_to_atomic sigma (Proofview.Goal.concl gl) in
  let _, _, constr = Rocq_utils.econstr_to_lambda sigma tys.(1) in
  let _, apptys = Rocq_utils.econstr_to_app sigma constr in
  match Array.to_list apptys with
  | [ wk_trans; wk_sim ] -> { wk_trans; wk_sim }
  | _ -> raise (Mebi_proof_ConclIsNot_Conj (sigma, (ty, tys)))
;;

(***********************************************************************)

exception Mebi_proof_CouldNotGetWkSimState of unit

type wk_sim_state =
  | M of concl_conj
  | N of concl_conj

let wk_sim_state (sigma : Evd.evar_map) : wk_sim_state -> EConstr.t =
  Log.trace __FUNCTION__;
  let f (c : Evd.evar_map -> Rocq_utils.kind_pair -> bool) wk =
    let x : Rocq_utils.kind_pair = Rocq_utils.econstr_to_atomic sigma wk in
    if c sigma x then snd x else raise (Mebi_proof_CouldNotGetWkSimState ())
  in
  function
  | M { wk_sim; _ } -> (f typ_is_weak_sim wk_sim).(5)
  | N { wk_trans; _ } -> (f typ_is_weak_transition wk_trans).(3)
;;

let wk_conj_get_state
      (sigma : Evd.evar_map)
      (stateof : wk_sim_state)
      (fsm : Fsm.t)
  : State.t
  =
  Log.trace __FUNCTION__;
  try find_state sigma (wk_sim_state sigma stateof) fsm.states with
  | Mebi_proof_CouldNotDecodeState (sigma, statety, states) ->
    Log.thing ~__FUNCTION__ Debug "states" states (Args Model.states_to_string);
    raise (Mebi_proof_CouldNotGetWkSimState ())
;;

exception Mebi_proof_CouldNotGetWkSimStateM of (Evd.evar_map * concl_conj)

let get_mstate (sigma : Evd.evar_map) (conj : concl_conj) : State.t =
  Log.trace __FUNCTION__;
  try wk_conj_get_state sigma (M conj) (mfsm ()) with
  | Mebi_proof_CouldNotGetWkSimState () ->
    raise (Mebi_proof_CouldNotGetWkSimStateM (sigma, conj))
;;

exception Mebi_proof_CouldNotGetWkSimStateN of (Evd.evar_map * concl_conj)

let get_nstate (sigma : Evd.evar_map) (conj : concl_conj) : State.t =
  Log.trace __FUNCTION__;
  try wk_conj_get_state sigma (N conj) (nfsm ()) with
  | Mebi_proof_CouldNotGetWkSimState () ->
    raise (Mebi_proof_CouldNotGetWkSimStateN (sigma, conj))
;;

(***********************************************************************)

let hyp_is_invertible (gl : Proofview.Goal.t) : Rocq_utils.hyp -> bool =
  fun (x : Rocq_utils.hyp) ->
  try Invertible.hyp_is_a gl x with Hyp.Mebi_proof_Hypothesis_Hyp _ -> false
;;

let hyps_get_invertibles (gl : Proofview.Goal.t) : Invertible.t list =
  List.filter_map
    (fun (x : Rocq_utils.hyp) ->
      try Some (Invertible.of_hyp gl x) with
      | Hyp.Mebi_proof_Hypothesis_Hyp _ -> None)
    (Proofview.Goal.hyps gl)
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
  (hyps_get_invertibles gl |> hyps_get_invertible).tactic
;;

(* let find_action_labelled (label:Label.t) (from:State.t) (fsm:Fsm.t) : Action.t =
   (Edges.find fsm.edges from)
   |> Model.get *)
(* 
let is_action_saturated : Action.t -> bool = function
  | { annotation; _ } -> Note.annotation_is_empty annotation
;; *)

exception Mebi_proof_NGotoNotInFsm of unit
exception Mebi_proof_NGoto_AlreadySome of State.t

let try_get_ngoto ?(saturated : bool = false) (mgoto : State.t)
  : Transition_opt.t -> State.t
  =
  Log.trace __FUNCTION__;
  (* *)
  function
  | { from = nfrom; label = nlabel; goto = None; _ } ->
    (* Log.thing ~__FUNCTION__ Debug ("mgoto") mgoto (Args State.to_string); *)
    (* Log.thing ~__FUNCTION__ Debug ("nfrom") nfrom (Args State.to_string); *)
    (* Log.thing ~__FUNCTION__ Debug ("nlabel") nlabel (Args Label.to_string); *)
    (try
       let nactions : States.t Actions.t =
         Edges.find (nfsm ~saturated ()).edges nfrom
       in
       let actions : Action.t list =
         Model.get_actions_labelled ~annotated:saturated nlabel nactions
       in
       match List.find_opt (fun x -> Actions.mem nactions x) actions with
       | None -> raise (Mebi_proof_NGotoNotInFsm ())
       | Some action ->
         Actions.find nactions action
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
  Log.trace __FUNCTION__;
  try try_get_ngoto ~saturated:false mgoto ntransition with
  | Mebi_proof_NGotoNotInFsm () ->
    Log.trace ~__FUNCTION__ "(saturated)";
    try_get_ngoto ~saturated:true mgoto ntransition
;;

let do_ex_intro (gl : Proofview.Goal.t) (ngoto : State.t) : tactic =
  Log.trace __FUNCTION__;
  let ngoto : EConstr.t = get_decoding ngoto.enc in
  let ngoto_bindings = Tactypes.ImplicitBindings [ ngoto ] in
  tactic
    ~msg:(Printf.sprintf "(exists %s)" (econstr_to_string gl ngoto))
    (Mebi_theories.tactics
       [ Tactics.constructor_tac true None 1 ngoto_bindings
       ; Tactics.split Tactypes.NoBindings
       ])
;;

(* let do_nnone (gl : Proofview.Goal.t) : tactic =
   Log.trace __FUNCTION__;

   let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
   let nstate : State.t = concl_wk_sim gl |> get_nstate sigma in
   Log.thing ~__FUNCTION__ Debug ("nstate") nstate (Args State.to_string);
   do_ex_intro gl nstate
   ;; *)

exception Mebi_proof_ExIntro_NEqStateM of (State.t * State.t option)

let assert_states_consistent (mstate : State.t) : State.t -> unit = function
  | mgoto ->
    if State.equal mstate mgoto
    then ()
    else raise (Mebi_proof_ExIntro_NEqStateM (mstate, Some mgoto))
;;

exception Mebi_proof_ExIntro_Transitions of (Transition.t * Transition_opt.t)
exception Mebi_proof_ExIntro_NEqLabel of (Transition.t * Transition_opt.t)

let assert_transition_opt_labels_eq
      (mtransition : Transition.t)
      (ntransition : Transition_opt.t)
  : unit
  =
  if Label.equal mtransition.label ntransition.label
  then ()
  else raise (Mebi_proof_ExIntro_NEqLabel (mtransition, ntransition))
;;

exception Mebi_proof_ExIntro_NotBisimilar of (State.t * State.t)

let do_nsome (gl : Proofview.Goal.t) (mtrans : Transition.t) : tactic =
  Log.trace __FUNCTION__;
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let { wk_trans; wk_sim } : concl_conj = concl_wk_sim gl in
  let mstate : State.t = get_mstate sigma { wk_trans; wk_sim } in
  Log.thing ~__FUNCTION__ Debug "mstate" mstate (Args State.to_string);
  let ntrans : Transition_opt.t = get_weak_ntransition sigma wk_trans in
  set_the_proof_state ~__FUNCTION__ (GoalTransition { mtrans; ntrans });
  Log.thing ~__FUNCTION__ Debug "mtransition" mtrans (Args Transition.to_string);
  Log.thing
    ~__FUNCTION__
    Debug
    "ntransition"
    ntrans
    (Args Transition_opt.to_string);
  assert_transition_opt_labels_eq mtrans ntrans;
  assert_transition_opt_states_bisimilar mtrans ntrans;
  assert_states_consistent mstate mtrans.goto;
  match ntrans.goto with
  | None ->
    let ngoto : State.t = get_ngoto mtrans.goto ntrans in
    Log.thing ~__FUNCTION__ Debug "mgoto" mtrans.goto (Args State.to_string);
    Log.thing ~__FUNCTION__ Debug "ngoto" ngoto (Args State.to_string);
    do_ex_intro gl ngoto
  | _ -> raise (Mebi_proof_ExIntro_Transitions (mtrans, ntrans))
;;

let do_nnone (gl : Proofview.Goal.t) (nstate : State.t) : tactic =
  Log.trace __FUNCTION__;
  set_the_proof_state ~__FUNCTION__ DoRefl;
  do_ex_intro gl nstate
;;

let can_do_nrefl (gl : Proofview.Goal.t) (mtransition : Transition.t)
  : State.t option
  =
  Log.trace __FUNCTION__;
  if Label.is_silent mtransition.label
  then (
    let nstate = concl_wk_sim gl |> (Proofview.Goal.sigma gl |> get_nstate) in
    if are_states_bisimilar mtransition.goto nstate then Some nstate else None)
  else None
;;

exception Mebi_proof_ConclIsNot_Exists of unit

let do_eexists_transition (gl : Proofview.Goal.t) : tactic =
  Log.trace __FUNCTION__;
  let mtransition : Transition.t = get_mtransition gl in
  Log.thing
    ~__FUNCTION__
    Debug
    "mtransition"
    mtransition
    (Args Transition.to_string);
  try
    match can_do_nrefl gl mtransition with
    | None -> do_nsome gl mtransition
    | Some nstate -> do_nnone gl nstate
  with
  | Mebi_proof_StatesNotBisimilar (mstate, nstate, bisim_states) ->
    Log.thing ~__FUNCTION__ Debug "bisim-states" (the_bisim_states ()) fpi;
    raise (Mebi_proof_ExIntro_NotBisimilar (mstate, nstate))
  | Mebi_proof_TransitionOptStatesNotBisimilar (mstate, nstate, bisim_states) ->
    raise (Mebi_proof_ExIntro_NotBisimilar (mstate, nstate))
  | Mebi_proof_CouldNotGetWkSimStateM (sigma, conj) ->
    Log.debug
      (Printf.sprintf
         "Mebi_proof.concl_get_eexists Mebi_proof_CouldNotGetWkSimStateM");
    Log.thing
      ~__FUNCTION__
      Debug
      "wk_sim"
      conj.wk_sim
      (Of (econstr_to_string gl));
    raise (Mebi_proof_ConclIsNot_Exists ())
  | Mebi_proof_CouldNotGetWkSimStateN (sigma, conj) ->
    Log.debug
      (Printf.sprintf
         "Mebi_proof.concl_get_eexists Mebi_proof_CouldNotGetWkSimStateN");
    Log.thing
      ~__FUNCTION__
      Debug
      "wk_trans"
      conj.wk_trans
      (Of (econstr_to_string gl));
    raise (Mebi_proof_ConclIsNot_Exists ())
  | Rocq_utils.Rocq_utils_EConstrIsNot_Atomic (sigma, x, k) ->
    Log.debug
      (Printf.sprintf
         "Mebi_proof.concl_get_eexists Rocq_utils_EConstrIsNot_Atomic");
    raise (Mebi_proof_ConclIsNot_Exists ())
  | Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)) ->
    Log.debug
      (Printf.sprintf
         "Mebi_proof.concl_get_eexists Mebi_proof_TyDoesNotMatchTheories");
    raise (Mebi_proof_ConclIsNot_Exists ())
;;

(***********************************************************************)

let hyps_has_cofix (sigma : Evd.evar_map) (concl : EConstr.t)
  : Rocq_utils.hyp list -> bool
  =
  Log.trace __FUNCTION__;
  List.exists (fun (h : Rocq_utils.hyp) ->
    Mebi_setup.Eq.econstr sigma concl (Context.Named.Declaration.get_type h))
;;

let hyps_has_invertible (gl : Proofview.Goal.t) : bool =
  Log.trace __FUNCTION__;
  List.exists (hyp_is_invertible gl) (Proofview.Goal.hyps gl)
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
  Log.trace __FUNCTION__;
  if hyps_is_empty gl
  then (
    set_the_proof_state ~__FUNCTION__ NewWeakSim;
    handle_proof_state gl)
  else (
    Log.warning "New Proof: Expected Hypothesis to be empty";
    raise (Mebi_proof_NewProof ()))

and handle_new_weak_sim (gl : Proofview.Goal.t) : tactic =
  Log.trace __FUNCTION__;
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let the_concl : EConstr.t = Proofview.Goal.concl gl in
  let concltyp : Rocq_utils.kind_pair =
    Rocq_utils.econstr_to_atomic sigma the_concl
  in
  (* NOTE: [auto] when concl is [weak_sim] and hyp has corresponding [cofix] *)
  if typ_is_weak_sim sigma concltyp
  then (
    Log.trace ~__FUNCTION__ "(concl is weak sim)";
    if hyps_has_cofix sigma the_concl (Proofview.Goal.hyps gl)
    then do_solve_cofix gl
    else (
      set_the_proof_state ~__FUNCTION__ NewCofix;
      do_new_cofix gl))
  else if typ_is_exists sigma concltyp
  then (
    Log.trace ~__FUNCTION__ "(concl is exists)";
    set_the_proof_state ~__FUNCTION__ NewCofix;
    handle_proof_state gl)
  else (
    warn_handle_new_weak_sim gl;
    raise (Mebi_proof_NewWeakSim ()))

and handle_new_cofix (gl : Proofview.Goal.t) : tactic =
  Log.trace __FUNCTION__;
  try
    if hyps_has_invertible gl
    then do_hyp_inversion gl
    else do_eexists_transition gl
  with
  (* NOTE: exception handling: *)
  | Mebi_proof_CouldNotFindHypTransition (sigma, fsm, hyps) ->
    Log.thing
      ~__FUNCTION__
      Warning
      "Could not find any transitions in Hyps"
      (hyps_to_string gl)
      (Args Utils.Strfy.string);
    raise (Mebi_proof_NewCofix ())
  | Mebi_proof_CouldNotObtainAction (from, label, goto, fsm) ->
    Log.warning "Mebi_proof_CouldNotObtainAction";
    raise (Mebi_proof_NewCofix ())
  | Mebi_proof_CouldNotDecodeTransitionState (sigma, x, fsm) ->
    Log.thing
      ~__FUNCTION__
      Warning
      "Could not decode transition state"
      (econstr_to_string gl x)
      (Args Utils.Strfy.string);
    Log.thing
      ~__FUNCTION__
      Debug
      "fsm.states"
      fsm.states
      (Args Model.states_to_string);
    raise (Mebi_proof_NewCofix ())
  | Mebi_proof_CouldNotDecodeTransitionLabel (sigma, x, fsm) ->
    Log.thing
      ~__FUNCTION__
      Warning
      "Could not decode transition label"
      (econstr_to_string gl x)
      (Args Utils.Strfy.string);
    Log.thing Debug "fsm.alphabet" fsm.alphabet (Args Model.alphabet_to_string);
    raise (Mebi_proof_NewCofix ())
  | Mebi_proof_ConclIsNot_Exists () -> raise (Mebi_proof_NewCofix ())
  | Mebi_proof_ExIntro_NEqStateM (mstate, mfrom) ->
    Log.warning "Mebi_proof_ExIntro_NEqStateM";
    Log.thing ~__FUNCTION__ Debug "mstate" mstate (Args State.to_string);
    Log.option ~__FUNCTION__ Debug "mfrom" mfrom (Args State.to_string);
    raise (Mebi_proof_NewCofix ())
  | Mebi_proof_ExIntro_NEqLabel (mtrans, ntrans) ->
    Log.warning "Mebi_proof_ExIntro_NEqLabel";
    Log.thing ~__FUNCTION__ Debug "mtrans" mtrans.label (Args Label.to_string);
    Log.thing ~__FUNCTION__ Debug "ntrans" ntrans.label (Args Label.to_string);
    raise (Mebi_proof_NewCofix ())

and handle_dorefl (gl : Proofview.Goal.t) : tactic =
  Log.trace __FUNCTION__;
  set_the_proof_state ~__FUNCTION__ NewWeakSim;
  tactic_chain
    [ do_apply_wk_none gl; do_unfold_silent gl; do_apply_rt1n_refl gl ]

and handle_goal_transition
      (gl : Proofview.Goal.t)
      ({ mtrans; ntrans } : PState.transitions)
  : tactic
  =
  Log.trace __FUNCTION__;
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  Log.thing ~__FUNCTION__ Debug "mtrans" mtrans (Args Transition.to_string);
  Log.thing ~__FUNCTION__ Debug "ntrans" ntrans (Args Transition_opt.to_string);
  Log.thing
    ~__FUNCTION__
    Debug
    "concl"
    (Proofview.Goal.concl gl)
    (Of (econstr_to_string gl));
  try
    let { from = nfrom; label = nlabel; goto = ngoto; _ } : Transition_opt.t =
      Proofview.Goal.concl gl
      |> Rocq_utils.econstr_to_atomic sigma
      |> get_weak_transition sigma (nfsm ~saturated:true ())
    in
    match ngoto with
    | Some ngoto ->
      assert_states_bisimilar mtrans.goto ngoto;
      Log.thing ~__FUNCTION__ Debug "mgoto" mtrans.goto fstate;
      Log.thing ~__FUNCTION__ Debug "ngoto" ngoto fstate;
      Log.thing ~__FUNCTION__ Debug "nlabel" nlabel flabel;
      do_constructor_transition gl nfrom nlabel ngoto
    | _ -> raise (Mebi_proof_GoalTransition ())
  with
  | Mebi_proof_CouldNotDecodeTransitionState (sigma, x, fsm) ->
    Log.thing ~__FUNCTION__ Debug "transition state" x (feconstr gl);
    Log.thing ~__FUNCTION__ Debug "states" fsm.states fstates;
    raise (Mebi_proof_GoalTransition ())
  | Mebi_proof_CouldNotObtainAction (from, label, goto, fsm) ->
    Log.warning "Mebi_proof_CouldNotObtainAction";
    raise (Mebi_proof_GoalTransition ())
  | Mebi_proof_CouldNotDecodeTransitionLabel (sigma, x, fsm) ->
    Log.thing ~__FUNCTION__ Debug "transition label" x (feconstr gl);
    Log.thing ~__FUNCTION__ Debug "alphabet" fsm.alphabet falphabet;
    raise (Mebi_proof_GoalTransition ())
  | Mebi_proof_StatesNotBisimilar (mstate, nstate, pi) ->
    Log.thing ~__FUNCTION__ Debug "non-bisimilar" (the_bisim_states ()) fpi;
    Log.thing ~__FUNCTION__ Debug "mfrom" mstate fstate;
    Log.thing ~__FUNCTION__ Debug "ngoto" nstate fstate;
    raise (Mebi_proof_GoalTransition ())
  | Mebi_proof_TyDoesNotMatchTheories (sigma, (ty, tys)) ->
    Log.thing ~__FUNCTION__ Debug "ty" ty (feconstr gl);
    Log.things ~__FUNCTION__ Debug "args" (Array.to_list tys) (feconstr gl);
    raise (Mebi_proof_GoalTransition ())

and handle_apply_constructors (gl : Proofview.Goal.t)
  : PState.applicable_constructors -> tactic
  =
  Log.trace __FUNCTION__;
  Log.thing ~__FUNCTION__ Debug "concl" gl (Of concl_to_string);
  function
  | { annotation; tactics = Some (h :: tl); goto } ->
    Log.trace ~__FUNCTION__ "apply tactic";
    set_the_proof_state
      ~__FUNCTION__
      (ApplyConstructors { annotation; tactics = Some tl; goto });
    tactic_chain [ h; do_simplify gl ]
  | { annotation = Some annotation; tactics; goto } ->
    Log.trace ~__FUNCTION__ "make new tactics";
    do_build_constructor_tactics gl goto annotation
  | { annotation = None; tactics; goto } ->
    Log.trace ~__FUNCTION__ "finished applying constructors";
    set_the_proof_state ~__FUNCTION__ NewWeakSim;
    tactic_chain [ do_simplify gl; do_eapply_rt1n_refl gl; do_simplify gl ]

and handle_proof_state (gl : Proofview.Goal.t) : tactic =
  Log.trace __FUNCTION__;
  try
    match !the_proof_state with
    | NewProof -> handle_new_proof gl
    | NewWeakSim -> handle_new_weak_sim gl
    | NewCofix -> handle_new_cofix gl
    | DoRefl -> handle_dorefl gl
    | GoalTransition transitions -> handle_goal_transition gl transitions
    | ApplyConstructors napplicable_constructors ->
      Log.debug (PState.to_string ~short:false !the_proof_state);
      handle_apply_constructors gl napplicable_constructors
    | DetectState -> detect_proof_state gl
  with
  | Mebi_proof_CouldNotDecodeEConstr (sigma, x) ->
    Log.thing ~__FUNCTION__ Debug "econstr" x (Of (econstr_to_string gl));
    raise (Mebi_proof_CouldNotDecodeEConstr (sigma, x))

and detect_proof_state (gl : Proofview.Goal.t) : tactic =
  Log.trace __FUNCTION__;
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
    set_the_proof_state ~__FUNCTION__ NewCofix;
    handle_proof_state gl)
;;

(***********************************************************************)
(*** Solve Proof *******************************************************)
(***********************************************************************)

let step () : unit Proofview.tactic =
  Log.trace __FUNCTION__;
  Mebi_theories.tactics
    [ Proofview.Goal.enter (fun gl ->
        get_tactic ~short:false (handle_proof_state gl))
    ; Mebi_tactics.simplify_and_subst_all ()
    ]
;;

let solve (upper_bound : int) (pstate : Declare.Proof.t) : Declare.Proof.t =
  Log.trace __FUNCTION__;
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
