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

let concl_to_atomic (gl : Proofview.Goal.t) : EConstr.t Rocq_utils.kind_pair =
  Rocq_utils.econstr_to_atomic
    (Proofview.Goal.sigma gl)
    (Proofview.Goal.concl gl)
;;

exception Mebi_proof_TheoryNoneNotEncoded of unit

let get_theory_none_enc () : Enc.t =
  match runkeep (Mebi_utils.get_none_enc_opt ()) with
  | None -> raise (Mebi_proof_TheoryNoneNotEncoded ())
  | Some x -> x
;;

let _get_theory_none_dec () : EConstr.t =
  runkeep (decode (get_theory_none_enc ()))
;;

(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.MkDefault ()

let () = Log.Config.configure_output Debug false
let () = Log.Config.configure_output Trace false
(***********************************************************************)

(* let fstr : string Utils.Strfy.to_string = Args Utils.Strfy.string *)

let feconstr (gl : Proofview.Goal.t) : EConstr.t Utils.Strfy.to_string =
  Of (econstr_to_string gl)
;;

let feconstr_opt (gl : Proofview.Goal.t)
  : EConstr.t option Utils.Strfy.to_string
  =
  Of (Utils.Strfy.option (feconstr gl))
;;

let _fconstr (gl : Proofview.Goal.t) : Constr.t Utils.Strfy.to_string =
  Of (Rocq_utils.Strfy.constr (Proofview.Goal.env gl) (Proofview.Goal.sigma gl))
;;

let _feconstrkinds (gl : Proofview.Goal.t) : EConstr.t Utils.Strfy.to_string =
  Of
    (Rocq_utils.Strfy.econstr_kind
       (Proofview.Goal.env gl)
       (Proofview.Goal.sigma gl))
;;

(* let fisty (gl : Proofview.Goal.t) : EConstr.t Utils.Strfy.to_string =
   Of
   (fun (x : EConstr.t) ->
   Utils.Strfy.bool
   (* (Constr.is_Type (EConstr.to_constr (Proofview.Goal.sigma gl) x)) *)
   (EConstr.isType (Proofview.Goal.sigma gl) x))
   ;; *)

(* let fistheory (gl : Proofview.Goal.t) : EConstr.t Utils.Strfy.to_string =
   Of
   (fun (x : EConstr.t) ->
   Utils.Strfy.bool (Mebi_theories.is_theory (Proofview.Goal.sigma gl) x))
   ;; *)

let _feconstrarr (gl : Proofview.Goal.t) : EConstr.t array Utils.Strfy.to_string
  =
  Of (Utils.Strfy.array (Of (econstr_to_string gl)))
;;

let fhyp (gl : Proofview.Goal.t) : Rocq_utils.hyp Utils.Strfy.to_string =
  Of (hyp_to_string gl)
;;

let _fekind (gl : Proofview.Goal.t) : EConstr.t Utils.Strfy.to_string =
  Of
    (Rocq_utils.Strfy.econstr_kind
       (Proofview.Goal.env gl)
       (Proofview.Goal.sigma gl))
;;

let fstate : State.t Utils.Strfy.to_string = Args State.to_string
let fstates : States.t Utils.Strfy.to_string = Args Model.states_to_string
let fpi : Partition.t Utils.Strfy.to_string = Args Model.partition_to_string

(* let flabel : Label.t Utils.Strfy.to_string = Args Label.to_string *)
let falphabet : Alphabet.t Utils.Strfy.to_string = Args Model.alphabet_to_string
(* let faction : Action.t Utils.Strfy.to_string = Args Action.to_string *)

(***********************************************************************)

let log_econstr
      ?(__FUNCTION__ : string = "")
      (s : string)
      (x : EConstr.t)
      (gl : Proofview.Goal.t)
  : unit
  =
  Log.thing ~__FUNCTION__ Debug s x (feconstr gl)
;;

let log_concl ?(__FUNCTION__ : string = "") (s : string) (gl : Proofview.Goal.t)
  : unit
  =
  Log.thing
    ~__FUNCTION__
    Debug
    s
    (Proofview.Goal.concl gl)
    (Of (econstr_to_string gl))
;;

let log_hyps
      ?(__FUNCTION__ : string = "")
      (s : string)
      (hyps : Rocq_utils.hyp list)
      (gl : Proofview.Goal.t)
  : unit
  =
  Log.things ~__FUNCTION__ Debug s hyps (Of (hyp_to_string gl))
;;

let _log_fsm ?(__FUNCTION__ : string = "") (s : string) (x : Fsm.t) : unit =
  Log.thing ~__FUNCTION__ Debug s x (Args Model.Fsm.to_string)
;;

let _log_states ?(__FUNCTION__ : string = "") (s : string) (x : States.t) : unit
  =
  Log.thing ~__FUNCTION__ Debug s x (Args Model.states_to_string)
;;

let _log_alphabet ?(__FUNCTION__ : string = "") (s : string) (x : Alphabet.t)
  : unit
  =
  Log.thing ~__FUNCTION__ Debug s x (Args Model.alphabet_to_string)
;;

let log_label ?(__FUNCTION__ : string = "") (s : string) (x : Label.t) : unit =
  Log.thing ~__FUNCTION__ Debug s x (Args Label.to_string)
;;

let log_label_opt
      ?(__FUNCTION__ : string = "")
      (s : string)
      (x : Label.t option)
  : unit
  =
  Log.option ~__FUNCTION__ Debug s x (Args Label.to_string)
;;

let _log_action ?(__FUNCTION__ : string = "") (s : string) (x : Action.t) : unit
  =
  Log.thing ~__FUNCTION__ Debug s x (Args Action.to_string)
;;

let _log_state ?(__FUNCTION__ : string = "") (s : string) (x : State.t) : unit =
  Log.thing ~__FUNCTION__ Debug s x (Args State.to_string)
;;

let log_transition ?(__FUNCTION__ : string = "") (s : string) (x : Transition.t)
  : unit
  =
  Log.thing ~__FUNCTION__ Debug s x (Args Transition.to_string)
;;

let log_transition_opt
      ?(__FUNCTION__ : string = "")
      (s : string)
      (x : Transition_opt.t)
  : unit
  =
  Log.thing ~__FUNCTION__ Debug s x (Args Transition_opt.to_string)
;;

(***********************************************************************)

let econstr_eq (gl : Proofview.Goal.t) : EConstr.t -> EConstr.t -> bool =
  Mebi_setup.Eq.econstr (Proofview.Goal.sigma gl)
;;

let econstr_eq_concl (gl : Proofview.Goal.t) : EConstr.t -> bool =
  econstr_eq gl (Proofview.Goal.concl gl)
;;

(***********************************************************************)

let acc_econstrs (gl : Proofview.Goal.t) (acc : EConstr.t list) (x : EConstr.t)
  : EConstr.t list
  =
  match List.find_opt (econstr_eq gl x) acc with
  | Some _ -> acc
  | None -> x :: acc
;;

let merge_econstr_lists (gl : Proofview.Goal.t)
  : EConstr.t list -> EConstr.t list -> EConstr.t list
  =
  List.fold_left (acc_econstrs gl)
;;

let acc_econstr_to_unfold
      (gl : Proofview.Goal.t)
      (acc : EConstr.t list)
      (x : EConstr.t)
  : EConstr.t list
  =
  if Mebi_theories.is_constr (Proofview.Goal.sigma gl) x
  then acc_econstrs gl acc x
  else acc
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

exception Mebi_proof_ChainEmptyTactics of unit

let tactic_chain ?(nonempty : bool = false) : tactic list -> tactic =
  Log.trace __FUNCTION__;
  let f : string option * string option -> string option = function
    | None, None -> None
    | Some xmsg, None -> Some xmsg
    | None, Some ymsg -> Some ymsg
    | Some xmsg, Some ymsg -> Some (Printf.sprintf "%s; %s" xmsg ymsg)
  in
  let g =
    List.fold_left (fun { msg = xmsg; x } { msg = ymsg; x = y } ->
      { msg = f (xmsg, ymsg); x = Mebi_theories.tactics [ x; y ] })
  in
  function
  | [] ->
    if nonempty
    then raise (Mebi_proof_ChainEmptyTactics ())
    else tactic (Proofview.tclUNIT ())
  | h :: [] -> g h []
  | h :: tl -> g h tl
;;

module ApplicableConstructors = struct
  type t =
    { current : Tree.Node.t list
    ; annotation : Note.annotation option
    ; destination : State.t
    }

  let to_string ({ current; annotation; destination } : t) : string =
    let current : string =
      Utils.Strfy.list (Args Tree.Node.to_string) current
    in
    let annotation : string =
      Utils.Strfy.option (Args Note.annotation_to_string) annotation
    in
    let destination : string = State.to_string destination in
    Utils.Strfy.record
      [ "current", current
      ; "annotation", annotation
      ; "destination", destination
      ]
  ;;

  (* TODO: inchworm *)
  (* and tactics_to_apply =
    { this : tactic
    ; next : Tree.Node.t list
    } *)

  exception Mebi_proof_CannotGetConstructorInfo_None of unit
  exception Mebi_proof_CannotFindConstructorInfo_OfLTS of Enc.t
  exception Mebi_proof_CannotFindConstructorInfo_OfIndex of int

  type constructor_binding_args =
    | Raw of EConstr.t constructor_binding_triple
    | Enc of Enc.t constructor_binding_triple

  and 'a constructor_binding_triple = 'a * 'a option * 'a option

  let make_constructor_bindings
        (gl : Proofview.Goal.t)
        (bindings : Rocq_bindings.t)
    : constructor_binding_args -> EConstr.t Tactypes.bindings
    =
    Log.trace __FUNCTION__;
    let env = Proofview.Goal.env gl in
    let sigma = Proofview.Goal.sigma gl in
    let f ((x, y, z) : EConstr.t * EConstr.t option * EConstr.t option)
      : EConstr.t Tactypes.bindings
      =
      Log.thing ~__FUNCTION__ Debug "x" x (feconstr gl);
      Log.thing ~__FUNCTION__ Debug "y" y (feconstr_opt gl);
      Log.thing ~__FUNCTION__ Debug "z" z (feconstr_opt gl);
      Rocq_bindings.get env sigma x y z bindings
    in
    function
    | Raw args ->
      Log.trace ~__FUNCTION__ "Raw args";
      f args
    | Enc (the_from, the_action, the_goto) ->
      Log.trace ~__FUNCTION__ "Enc args";
      let the_from : EConstr.t = runkeep (decode the_from) in
      let fopt =
        fun (xopt : Enc.t option) ->
        Option.cata
          (fun (x : Enc.t) -> runkeep (Mebi_wrapper.decode_opt x))
          None
          xopt
      in
      let the_action : EConstr.t option = fopt the_action in
      let the_goto : EConstr.t option = fopt the_goto in
      f (the_from, the_action, the_goto)
  ;;

  let get_constructor_bindings
        (gl : Proofview.Goal.t)
        ((lts_enc, constructor_index) : Tree.Node.t)
    : constructor_binding_args -> EConstr.t Tactypes.bindings
    =
    Log.trace __FUNCTION__;
    (* NOTE: assuming this is for nfsm *)
    match (nfsm ()).info.rocq_info with
    | None -> raise (Mebi_proof_CannotGetConstructorInfo_None ())
    | Some info ->
      let finfo = fun ({ enc; _ } : Info.rocq_info) -> Enc.equal enc lts_enc in
      (match List.find_opt finfo info with
       | None -> raise (Mebi_proof_CannotFindConstructorInfo_OfLTS lts_enc)
       | Some x ->
         let fconstructor =
           fun ({ index; _ } : Rocq_bindings.constructor) ->
           Int.equal index constructor_index
         in
         (match List.find_opt fconstructor x.constructors with
          | None ->
            raise
              (Mebi_proof_CannotFindConstructorInfo_OfIndex constructor_index)
          | Some x ->
            Log.thing
              ~__FUNCTION__
              Debug
              "constructor"
              x
              (Args Mebi_utils.Strfy.rocq_constructor_to_string);
            make_constructor_bindings gl x.bindings))
  ;;

  let get_constructor_tactic
        (gl : Proofview.Goal.t)
        ((enc, index) : Tree.Node.t)
    : constructor_binding_args option -> tactic
    =
    Log.trace __FUNCTION__;
    (* NOTE: constructors index from 1 *)
    let index : int = index + 1 in
    let msg : string = Printf.sprintf "constructor %i" index in
    let f (xs : EConstr.t Tactypes.bindings) : tactic =
      tactic ~msg (Tactics.one_constructor index xs)
    in
    function
    | None -> f NoBindings
    | Some args -> f (get_constructor_bindings gl (enc, index) args)
  ;;
end

module PState = struct
  type t =
    | NewProof
    | NewWeakSim
    | NewCofix
    | DoRefl
    | GoalTransition of transitions
    | ApplyConstructors of ApplicableConstructors.t
    | DetectState

  (* TODO: maybe revert to just [mtrans:Transition.t]*)
  and transitions =
    { mtrans : Transition.t
    ; ntrans : Transition_opt.t
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
    | ApplyConstructors x ->
      Printf.sprintf
        "ApplyConstructors%s"
        (if short then "" else ApplicableConstructors.to_string x)
    | DetectState -> "DetectState"
  ;;
end

(* TODO: make this part of a functor, so we always have a starting state, etc.*)
(* TODO: also, this is where we can "pre-process" the different constructors, checking if they need to be applied with "explicit bindings" -- unless we stash this in the Mebi_info *)

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
  list (Args (list (Of (PState.to_string ~short)))) !the_old_proof_states
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
   : Tree.Node.t list
   =
   Log.trace __FUNCTION__;
   get_naction ~annotated:true nfrom nlabel (nfsm ~saturated:false ())
   |> Model.get_shortest_constructor
   ;; *)

(* let get_annotation_constructor
      ({ this = { from; via }; next } : Note.annotation)
  : Tree.Node.t list
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

exception Mebi_proof_TyDoesNotMatchTheories of EConstr.t Rocq_utils.kind_pair

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

let do_unfold (gl : Proofview.Goal.t) (x : EConstr.t) : tactic =
  Log.trace __FUNCTION__;
  tactic
    ~msg:(Printf.sprintf "unfold %s" (econstr_to_string gl x))
    (Mebi_tactics.unfold_econstr gl x)
;;

exception Mebi_proof_NothingToUnfold of unit

let chain_do_unfold (gl : Proofview.Goal.t) : EConstr.t list -> tactic =
  Log.trace __FUNCTION__;
  function
  | [] -> raise (Mebi_proof_NothingToUnfold ())
  | x :: xs ->
    List.fold_left
      (fun (acc : tactic) (x : EConstr.t) ->
        tactic_chain [ acc; do_unfold gl x ])
      (do_unfold gl x)
      xs
;;

let do_unfold_in_hyp
      (gl : Proofview.Goal.t)
      (h : Rocq_utils.hyp)
      (x : EConstr.t)
  : tactic
  =
  Log.trace __FUNCTION__;
  tactic_chain [ tactic (Mebi_tactics.unfold_in_hyp gl x h); do_unfold gl x ]
;;

let chain_do_unfold_in_hyp (gl : Proofview.Goal.t) (h : Rocq_utils.hyp)
  : EConstr.t list -> tactic
  =
  Log.trace __FUNCTION__;
  function
  | [] -> raise (Mebi_proof_NothingToUnfold ())
  | x :: [] -> do_unfold_in_hyp gl h x
  | x :: xs ->
    List.fold_left
      (fun (acc : tactic) (x : EConstr.t) ->
        tactic_chain [ acc; do_unfold_in_hyp gl h x ])
      (do_unfold_in_hyp gl h x)
      xs
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
  let actions : Action.t list = get_actions ~annotated from via fsm in
  Log.thing ~__FUNCTION__ Debug "from" from (Args State.to_string);
  Log.thing ~__FUNCTION__ Debug "label" via (Args Label.to_string);
  Log.thing ~__FUNCTION__ Debug "goto" goto (Args State.to_string);
  Log.things ~__FUNCTION__ Debug "actions" actions (Args Action.to_string);
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
    (ApplyConstructors
       { annotation = Some annotation; current = []; destination = goto });
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

(* let do_build_constructor_tactics'
      (gl : Proofview.Goal.t) : 
      ApplicableConstructors.t -> tactic =
      function |
      {tactics=None;_}
      {tactics=Some {this;next};_}


  ;; *)

(* let do_build_constructor_tactics
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
  let constructor : Tree.Node.t list = Tree.min using in
  set_the_proof_state
    ~__FUNCTION__
    (ApplyConstructors
       (ApplicableConstructors.create
          { from; label = via; goto }
          next
          constructor));
  (* match ApplicableConstructors.try_get_next { from; label = via; goto } with *)
  (* | None -> *)
  (* let tactics : tactic list =
    try
      List.map (get_constructor_tactic { from; label = via; goto }) constructor
    with
    | Mebi_utils.Mebi_utils_BindingInstruction_NEQ (x, y) ->
      Log.warning ~__FUNCTION__ "Mebi_utils_BindingInstruction_NEQ";
      Log.thing ~__FUNCTION__ Debug "x" x (feconstr gl);
      Log.thing ~__FUNCTION__ Debug "y" y (fconstr gl);
      raise (Mebi_utils.Mebi_utils_BindingInstruction_NEQ (x, y))
    | Mebi_utils.Mebi_utils_BindingInstruction_NotApp x ->
      Log.warning ~__FUNCTION__ "Mebi_utils_BindingInstruction_NotApp";
      Log.thing ~__FUNCTION__ Debug "x" x (feconstr gl);
      raise (Mebi_utils.Mebi_utils_BindingInstruction_NotApp x)
    | Mebi_utils.Mebi_utils_BindingInstruction_Undefined (x, y) ->
      Log.warning ~__FUNCTION__ "Mebi_utils_BindingInstruction_Undefined";
      Log.thing ~__FUNCTION__ Debug "x" x (feconstr gl);
      Log.thing ~__FUNCTION__ Debug "y" y (feconstr gl);
      raise (Mebi_utils.Mebi_utils_BindingInstruction_Undefined (x, y))
    | Mebi_utils.Mebi_utils_BindingInstruction_IndexOutOfBounds (x, i) ->
      Log.warning ~__FUNCTION__ "Mebi_utils_BindingInstruction_IndexOutOfBounds";
      Log.thing ~__FUNCTION__ Debug "x" x (feconstr gl);
      Log.thing ~__FUNCTION__ Debug "y" i (Args Utils.Strfy.int);
      raise (Mebi_utils.Mebi_utils_BindingInstruction_IndexOutOfBounds (x, i))
  in
  set_the_proof_state
    ~__FUNCTION__
    (* TODO: cannot store entire constructor tactics at once, need to "inch worm" along *)
    (ApplyConstructors { annotation = next; tactics = Some tactics; goto }); *)
  do_rt1n_via gl via
;; *)

(***********************************************************************)

exception Mebi_proof_CouldNotDecodeEConstr of EConstr.t

let try_decode (gl : Proofview.Goal.t) (x : EConstr.t) : Enc.t option =
  (* Log.trace __FUNCTION__; *)
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  if EConstr.isRel sigma x
  then None
  else (
    try get_encoding_opt x with
    | Not_found ->
      Log.thing ~__FUNCTION__ Debug "could not decode" x (feconstr gl);
      raise (Mebi_proof_CouldNotDecodeEConstr x))
;;

let typ_is_exists
      (gl : Proofview.Goal.t)
      ((ty, _) : EConstr.t Rocq_utils.kind_pair)
  : bool
  =
  Log.trace __FUNCTION__;
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_ex ())
;;

let typ_is_weak_sim
      (gl : Proofview.Goal.t)
      ((ty, _) : EConstr.t Rocq_utils.kind_pair)
  : bool
  =
  Log.trace __FUNCTION__;
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_weak_sim ())
;;

let typ_is_weak_transition
      (gl : Proofview.Goal.t)
      ((ty, _) : EConstr.t Rocq_utils.kind_pair)
  : bool
  =
  Log.trace __FUNCTION__;
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_weak ())
;;

let typ_is_tau
      (gl : Proofview.Goal.t)
      ((ty, _) : EConstr.t Rocq_utils.kind_pair)
  : bool
  =
  Log.trace __FUNCTION__;
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_tau ())
;;

let typ_is_silent_transition
      (gl : Proofview.Goal.t)
      ((ty, _) : EConstr.t Rocq_utils.kind_pair)
  : bool
  =
  Log.trace __FUNCTION__;
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_silent ())
;;

let typ_is_silent1_transition
      (gl : Proofview.Goal.t)
      ((ty, _) : EConstr.t Rocq_utils.kind_pair)
  : bool
  =
  Log.trace __FUNCTION__;
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_silent1 ())
;;

let typ_is_lts_transition
      (gl : Proofview.Goal.t)
      ((ty, _) : EConstr.t Rocq_utils.kind_pair)
  : bool
  =
  Log.trace __FUNCTION__;
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_LTS ())
;;

let typ_is_fsm_constructor
      (gl : Proofview.Goal.t)
      ((ty, _) : EConstr.t Rocq_utils.kind_pair)
  : Fsm.t -> bool
  =
  (* Log.trace __FUNCTION__; *)
  (* Log.thing ~__FUNCTION__ Debug "ty" ty (feconstr gl); *)
  function
  | { info = { rocq_info = None; _ }; _ } -> false
  | { info = { rocq_info = Some xs; _ }; _ } ->
    (* Log.thing ~__FUNCTION__ Debug "tykind" ty (fekind gl); *)
    if Mebi_theories.is_theory (Proofview.Goal.sigma gl) ty
    then false
    else (
      match try_decode gl ty with
      | None -> false
      | Some y ->
        List.exists (fun ({ enc = x; _ } : Info.rocq_info) -> Enc.equal x y) xs)
;;

(***********************************************************************)

exception Mebi_proof_CannotDecodeNeededTerm of EConstr.t
exception Mebi_proof_CheckTermCanBeUnfolded of (Rocq_utils.hyp * EConstr.t)

let try_find_state
      ?(needed : bool = false)
      (gl : Proofview.Goal.t)
      (x : EConstr.t)
      (states : States.t)
  : State.t option
  =
  Log.trace __FUNCTION__;
  match try_decode gl x with
  | None ->
    Log.debug ~__FUNCTION__ "None";
    (* NOTE: this allows us to catch some terms that need to be unfolded *)
    if needed then raise (Mebi_proof_CannotDecodeNeededTerm x) else None
  | Some x ->
    Log.thing ~__FUNCTION__ Debug "enc" x (Of Enc.to_string);
    Some (decode_state x states)
;;

exception Mebi_proof_CouldNotDecodeState of (EConstr.t * States.t)

let find_state (gl : Proofview.Goal.t) (x : EConstr.t) (states : States.t)
  : State.t
  =
  Log.trace __FUNCTION__;
  try
    match try_find_state gl x states with
    | Some s -> s
    | None -> raise (Mebi_proof_CouldNotDecodeState (x, states))
  with
  | Model_CannotDecodeState (enc, states) ->
    Log.trace ~__FUNCTION__ "Exception: Model_CannotDecodeState";
    raise (Mebi_proof_CouldNotDecodeState (x, states))
;;

(***********************************************************************)

exception Mebi_proof_CouldNotDecodeLabel of (EConstr.t * Alphabet.t)

let try_find_label
      ?(needed : bool = false)
      (gl : Proofview.Goal.t)
      (x : EConstr.t)
      (labels : Alphabet.t)
  : Label.t option
  =
  Log.trace __FUNCTION__;
  Log.thing ~__FUNCTION__ Debug "To Find" x (feconstr gl);
  Log.thing ~__FUNCTION__ Debug "Labels" labels (Args alphabet_to_string);
  let y : Enc.t option =
    Utils.try_seq_opt
      x
      [ try_decode gl
      ; Mebi_utils.try_get_none_enc_opt
      ; Mebi_utils.try_get_some_enc_opt
      ]
  in
  match Option.map (fun (z : Enc.t) -> decode_label z labels) y with
  | None -> if needed then raise (Mebi_proof_CannotDecodeNeededTerm x) else None
  | z -> z
;;

let _find_label (gl : Proofview.Goal.t) (x : EConstr.t) (labels : Alphabet.t)
  : Label.t
  =
  Log.trace __FUNCTION__;
  try
    match try_find_label gl x labels with
    | Some s -> s
    | None -> raise (Mebi_proof_CouldNotDecodeLabel (x, labels))
  with
  | Model_CannotDecodeLabel (enc, labels) ->
    Log.trace ~__FUNCTION__ "Exception: Model_CannotDecodeLabel";
    raise (Mebi_proof_CouldNotDecodeLabel (x, labels))
;;

(***********************************************************************)

exception Mebi_proof_CouldNotDecodeTransitionState of (EConstr.t * Fsm.t)
exception Mebi_proof_CouldNotDecodeTransitionLabel of (EConstr.t * Fsm.t)

exception
  Mebi_proof_CouldNotObtainAction of
    (State.t * Label.t option * State.t option * Fsm.t)

let get_transition
      ?(need_action : bool = true)
      ?(need_goto : bool = false)
      (gl : Proofview.Goal.t)
      (fromty : EConstr.t)
      (labelty : EConstr.t)
      (gototy : EConstr.t)
      (fsm : Fsm.t)
  : Transition_opt.t
  =
  Log.trace __FUNCTION__;
  try
    let from : State.t = find_state gl fromty fsm.states in
    Log.thing ~__FUNCTION__ Debug "From" from (Args State.to_string);
    (* log_alphabet ~__FUNCTION__ "Alphabet" fsm.alphabet; *)
    Log.thing ~__FUNCTION__ Debug "Gototy" gototy (feconstr gl);
    let goto : State.t option =
      try_find_state ~needed:need_goto gl gototy fsm.states
    in
    let label : Label.t option =
      try_find_label ~needed:need_action gl labelty fsm.alphabet
    in
    (* Log.thing ~__FUNCTION__ Debug "Label" label (Args Label.to_string); *)
    Log.option ~__FUNCTION__ Debug "Goto" goto (Args State.to_string);
    (* let actions : States.t Actions.t = Edges.find fsm.edges from in *)
    (* log_actions ~__FUNCTION__ "Actions" actions; *)
    match goto with
    | None ->
      Log.trace ~__FUNCTION__ "goto:None";
      if need_action
      then raise (Mebi_proof_CouldNotObtainAction (from, label, goto, fsm))
      else Transition_opt.create from label goto ()
    | Some the_goto ->
      Log.trace ~__FUNCTION__ "goto:Some";
      (match label with
       | None ->
         Log.trace ~__FUNCTION__ "label:None";
         if need_action
         then raise (Mebi_proof_CouldNotObtainAction (from, label, goto, fsm))
         else Transition_opt.create from label goto ()
       | Some label ->
         Log.trace ~__FUNCTION__ "label:Some";
         let { annotation; constructor_trees; _ } : Action.t =
           get_action_to ~annotated:true from label the_goto fsm
         in
         Transition_opt.create
           from
           (Some label)
           goto
           ~annotation
           ~constructor_trees
           ())
  with
  | Mebi_proof_CouldNotDecodeState (ty, states) ->
    Log.trace ~__FUNCTION__ "Exception: Mebi_proof_CouldNotDecodeState";
    Log.thing ~__FUNCTION__ Debug "Could not decode State" ty (feconstr gl);
    raise (Mebi_proof_CouldNotDecodeTransitionState (ty, fsm))
  | Mebi_proof_CouldNotDecodeLabel (ty, alphabet) ->
    Log.trace ~__FUNCTION__ "Exception: Mebi_proof_CouldNotDecodeLabel";
    Log.thing ~__FUNCTION__ Debug "Could not decode Label" ty (feconstr gl);
    raise (Mebi_proof_CouldNotDecodeTransitionLabel (ty, fsm))
;;

let get_lts_transition
      (gl : Proofview.Goal.t)
      (fsm : Fsm.t)
      ((ty, tys) : EConstr.t Rocq_utils.kind_pair)
  : Transition_opt.t
  =
  Log.trace __FUNCTION__;
  if typ_is_lts_transition gl (ty, tys)
  then get_transition gl tys.(0) tys.(1) tys.(2) fsm
  else raise (Mebi_proof_TyDoesNotMatchTheories (ty, tys))
;;

exception Mebi_proof_CouldNotFindHypTransition of (Fsm.t * Rocq_utils.hyp list)

let get_hyp_transition (gl : Proofview.Goal.t) (fsm : Fsm.t) : Transition_opt.t =
  Log.trace __FUNCTION__;
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  (* NOTE: returns [Some t] if [x:hyp] can be made into a [Transition_opt.t] *)
  let f (x : Rocq_utils.hyp) : Transition_opt.t option =
    Log.trace __FUNCTION__;
    try
      let ty, tys = Rocq_utils.hyp_to_atomic sigma x in
      Log.thing ~__FUNCTION__ Debug "ty" ty (feconstr gl);
      Log.things ~__FUNCTION__ Debug "tys" (Array.to_list tys) (feconstr gl);
      if typ_is_fsm_constructor gl (ty, tys) fsm
      then (
        Log.trace ~__FUNCTION__ "is fsm constructor => TRUE";
        Some
          (get_transition
             ~need_action:false
             ~need_goto:true
             gl
             tys.(0)
             tys.(1)
             tys.(2)
             fsm))
      else (
        Log.trace ~__FUNCTION__ "is fsm constructor => false";
        None)
    with
    | Mebi_proof_CannotDecodeNeededTerm y ->
      Log.trace ~__FUNCTION__ "Exception: Mebi_proof_CannotDecodeNeededTerm";
      Log.thing ~__FUNCTION__ Debug "Could not decode needed" y (feconstr gl);
      raise (Mebi_proof_CheckTermCanBeUnfolded (x, y))
    | Mebi_proof_CouldNotDecodeTransitionState (ty, fsm) ->
      Log.trace
        ~__FUNCTION__
        "Exception: Mebi_proof_CouldNotDecodeTransitionState";
      Log.thing ~__FUNCTION__ Debug "Could not decode State" ty (feconstr gl);
      None
    | Mebi_proof_CouldNotDecodeTransitionLabel (ty, fsm) ->
      Log.trace
        ~__FUNCTION__
        "Exception: Mebi_proof_CouldNotDecodeTransitionLabel";
      Log.thing ~__FUNCTION__ Debug "Could not decode Label" ty (feconstr gl);
      None
  in
  match List.filter_map f (Proofview.Goal.hyps gl) with
  | [] ->
    raise (Mebi_proof_CouldNotFindHypTransition (fsm, Proofview.Goal.hyps gl))
  | h :: [] -> h
  | h :: _tl ->
    let len : int = List.length (h :: _tl) in
    Args Utils.Strfy.int
    |> Log.thing ~__FUNCTION__ Debug "multiple transitions found" len;
    h
;;

exception Mebi_proof_ExpectedMTransition_Some_Label of Transition_opt.t
exception Mebi_proof_ExpectedMTransition_Some_Goto of Transition_opt.t

let get_mtransition (gl : Proofview.Goal.t) : Transition.t =
  Log.trace __FUNCTION__;
  let mtrans_opt : Transition_opt.t =
    get_hyp_transition gl (mfsm ~saturated:false ())
  in
  let { from; label; goto; annotation; constructor_trees } : Transition_opt.t =
    mtrans_opt
  in
  let label =
    Option.cata
      (fun x -> x)
      (raise (Mebi_proof_ExpectedMTransition_Some_Label mtrans_opt))
      label
  in
  let goto =
    Option.cata
      (fun x -> x)
      (raise (Mebi_proof_ExpectedMTransition_Some_Goto mtrans_opt))
      goto
  in
  { from; label; goto; annotation; constructor_trees }
;;

let get_weak_transition
      (gl : Proofview.Goal.t)
      (fsm : Fsm.t)
      ((ty, tys) : EConstr.t Rocq_utils.kind_pair)
  : Transition_opt.t
  =
  Log.trace __FUNCTION__;
  if typ_is_weak_transition gl (ty, tys)
  then get_transition ~need_action:false gl tys.(3) tys.(5) tys.(4) fsm
  else raise (Mebi_proof_TyDoesNotMatchTheories (ty, tys))
;;

let get_weak_ntransition (gl : Proofview.Goal.t) (wk_trans : EConstr.t)
  : Transition_opt.t
  =
  Log.trace __FUNCTION__;
  Rocq_utils.econstr_to_atomic (Proofview.Goal.sigma gl) wk_trans
  |> get_weak_transition gl (nfsm ~saturated:true ())
;;

let _get_silent_transition
      (gl : Proofview.Goal.t)
      (fsm : Fsm.t)
      ((ty, tys) : EConstr.t Rocq_utils.kind_pair)
  : Transition_opt.t
  =
  Log.trace __FUNCTION__;
  if typ_is_silent_transition gl (ty, tys)
  then get_transition gl tys.(2) tys.(4) tys.(3) fsm
  else raise (Mebi_proof_TyDoesNotMatchTheories (ty, tys))
;;

let _get_silent1_transition
      (gl : Proofview.Goal.t)
      (fsm : Fsm.t)
      ((ty, tys) : EConstr.t Rocq_utils.kind_pair)
  : Transition_opt.t
  =
  Log.trace __FUNCTION__;
  if typ_is_silent1_transition gl (ty, tys)
  then get_transition gl tys.(2) tys.(4) tys.(3) fsm
  else raise (Mebi_proof_TyDoesNotMatchTheories (ty, tys))
;;

(* let get_tau_transition
   (gl : Proofview.Goal.t)
   (fsm : Fsm.t)
   ((ty, tys) : EConstr.t Rocq_utils.kind_pair)
   : Transition_opt.t
   =
   Log.trace __FUNCTION__;
   if typ_is_tau_transition gl (ty, tys)
   then get_transition gl tys.(0) tys.(1) tys.(2) fsm
   else raise (Mebi_proof_TyDoesNotMatchTheories (ty, tys))
   ;; *)

let _get_concl_ntransition
      (gl : Proofview.Goal.t)
      ((ty, tys) : EConstr.t Rocq_utils.kind_pair)
  : Transition_opt.t
  =
  Log.trace __FUNCTION__;
  Log.debug "trying weak";
  try get_weak_transition gl (nfsm ~saturated:true ()) (ty, tys) with
  | Mebi_proof_TyDoesNotMatchTheories (ty, tys) ->
    Log.debug "trying silent";
    (try _get_silent_transition gl (nfsm ~saturated:true ()) (ty, tys) with
     | Mebi_proof_TyDoesNotMatchTheories (ty, tys) ->
       Log.debug "trying silent1";
       _get_silent1_transition gl (nfsm ~saturated:true ()) (ty, tys))
;;

(***********************************************************************)

let get_econstrs_to_unfold (gl : Proofview.Goal.t) (x : EConstr.t)
  : EConstr.t list
  =
  Log.trace __FUNCTION__;
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let rec loop (acc : EConstr.t list) (x : EConstr.t) : EConstr.t list =
    let acc : EConstr.t list = acc_econstr_to_unfold gl acc x in
    try
      let ty, tys = Rocq_utils.econstr_to_atomic sigma x in
      let acc : EConstr.t list = acc_econstr_to_unfold gl acc ty in
      Array.fold_left
        (fun (acc : EConstr.t list) (y : EConstr.t) -> loop acc y)
        acc
        tys
    with
    | Rocq_utils.Rocq_utils_EConstrIsNotA_Type _ -> acc
  in
  loop [] x
;;

let do_any_unfold_concl ?(enforce : bool = false) (gl : Proofview.Goal.t)
  : tactic
  =
  Log.trace __FUNCTION__;
  let the_concl : EConstr.t = Proofview.Goal.concl gl in
  try get_econstrs_to_unfold gl the_concl |> chain_do_unfold gl with
  | Mebi_proof_NothingToUnfold x ->
    if enforce
    then raise (Mebi_proof_NothingToUnfold x)
    else tactic (Proofview.tclUNIT ())
;;

let do_any_unfold_hyp_pair
      (gl : Proofview.Goal.t)
      ((ty, tys) : EConstr.t Rocq_utils.kind_pair)
  : EConstr.t list
  =
  Log.trace __FUNCTION__;
  try get_econstrs_to_unfold gl ty with
  | Mebi_proof_NothingToUnfold () ->
    Log.trace ~__FUNCTION__ "Exception: Mebi_proof_NothingToUnfold";
    Array.fold_left
      (fun (acc : EConstr.t list) (x : EConstr.t) ->
        get_econstrs_to_unfold gl x |> merge_econstr_lists gl acc)
      []
      tys
;;

(* TODO: remove this, replace with just [do_any_unfold] which throws exception if nothing to unfold is found. *)
let check_if_can_unfold (gl : Proofview.Goal.t) : EConstr.t -> bool =
  Log.trace __FUNCTION__;
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let rec loop (x : EConstr.t) : bool =
    (* Log.thing ~__FUNCTION__ Trace "checking x" x (feconstr gl); *)
    (* Log.thing ~__FUNCTION__ Debug "(is_ty)" x (fisty gl); *)
    (* Log.thing ~__FUNCTION__ Debug "(is_theory)" x (fistheory gl); *)
    (* Log.thing ~__FUNCTION__ Debug "(kinds)" x (feconstrkinds gl); *)
    if Mebi_theories.is_constr sigma x
    then
      (* Log.thing ~__FUNCTION__ Debug "(is_constr)" x (feconstr gl); *)
      true
    else if Mebi_theories.is_var sigma x
    then
      (* Log.thing ~__FUNCTION__ Debug "(is_var)" x (feconstr gl); *)
      false
    else (
      (* Log.thing ~__FUNCTION__ Debug "(else)" x (feconstr gl); *)
      try
        let ty, tys = Rocq_utils.econstr_to_atomic sigma x in
        (* Log.thing ~__FUNCTION__ Debug "(else ty)" ty (feconstr gl); *)
        (* Log.thing ~__FUNCTION__ Debug "(else ty kinds)" ty (feconstrkinds gl); *)
        (* Log.things
          ~__FUNCTION__
          Debug
          "(exists) ToUnfold?"
          (Array.to_list tys)
          (feconstr gl); *)
        Array.fold_left
          (fun (acc : bool) (x : EConstr.t) -> acc || loop x)
          (if Mebi_theories.is_app sigma x && Mebi_theories.is_constr sigma ty
           then true
           else false)
          tys
      with
      | Rocq_utils.Rocq_utils_EConstrIsNotA_Type _ ->
        (* Log.thing ~__FUNCTION__ Debug "(not a type)" x (feconstr gl); *)
        false)
  in
  loop
;;

let _can_unfold_concl (gl : Proofview.Goal.t) : bool =
  Log.trace __FUNCTION__;
  let the_concl : EConstr.t = Proofview.Goal.concl gl in
  Log.thing ~__FUNCTION__ Debug "concl" the_concl (feconstr gl);
  check_if_can_unfold gl the_concl
;;

let can_unfold_hyp_pair
      (gl : Proofview.Goal.t)
      ((ty, tys) : EConstr.t Rocq_utils.kind_pair)
  : bool
  =
  Log.trace __FUNCTION__;
  (* check_if_can_unfold gl ty || Array.exists (check_if_can_unfold gl) tys *)
  Log.thing ~__FUNCTION__ Trace "ty" ty (feconstr gl);
  if check_if_can_unfold gl ty
  then true
  else (
    Log.trace ~__FUNCTION__ "(else can unfold in tys)";
    Log.things ~__FUNCTION__ Trace "tys" (Array.to_list tys) (feconstr gl);
    (* Array.exists (check_if_can_unfold gl) tys *)
    Array.fold_left
      (fun (acc : bool) (x : EConstr.t) ->
        Log.thing ~__FUNCTION__ Trace "(can unfold in tys)" x (feconstr gl);
        check_if_can_unfold gl x || acc)
      false
      tys)
;;

(***********************************************************************)
(*** Hypothesis ********************************************************)
(***********************************************************************)

module Cofix_HTy : Hyp.HTY_S = struct
  type t =
    { _m : State.t
    ; _n : State.t
    }

  let of_hty
        (gl : Proofview.Goal.t)
        ((ty, tys) : EConstr.t Rocq_utils.kind_pair)
    : t
    =
    let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
    if Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_weak_sim ())
    then (
      let _m : State.t = decode_state (get_encoding tys.(5)) (mfsm ()).states in
      let _n : State.t = decode_state (get_encoding tys.(6)) (nfsm ()).states in
      { _m; _n })
    else raise (Hyp.Mebi_proof_Hypothesis_HTy (ty, tys))
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
    | ToUnfold of EConstr.t list

  let to_string : t -> string = function
    | { kind = Full; _ } -> "Full"
    | { kind = Layer; _ } -> "Layer"
    | { kind = ToUnfold _; _ } -> "ToUnfold"
  ;;

  let of_hty
        (gl : Proofview.Goal.t)
        ((ty, tys) : EConstr.t Rocq_utils.kind_pair)
    : k
    =
    Log.trace __FUNCTION__;
    let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
    try
      (* TODO: just try to invert and then catch error if nothing to invert *)
      if can_unfold_hyp_pair gl (ty, tys)
      then (
        Log.thing ~__FUNCTION__ Debug "ToUnfold (inhyp)" ty (feconstr gl);
        ToUnfold (do_any_unfold_hyp_pair gl (ty, tys)))
      else if Mebi_theories.is_var sigma tys.(2)
      then (
        Log.thing ~__FUNCTION__ Debug "Full" tys.(2) (feconstr gl);
        Full)
      else if Mebi_theories.is_var sigma tys.(1)
      then (
        Log.thing ~__FUNCTION__ Debug "Layer" tys.(1) (feconstr gl);
        Layer)
      else (
        Log.thing ~__FUNCTION__ Debug "(else)" ty (feconstr gl);
        raise (Hyp.Mebi_proof_Hypothesis_HTy (ty, tys)))
    with
    | Invalid_argument _ ->
      (* NOTE: handles "Index out of bounds" for accessing [tys] array above. *)
      Log.trace ~__FUNCTION__ "Exception: Invalid_argument";
      raise (Hyp.Mebi_proof_Hypothesis_HTy (ty, tys))
    | Mebi_proof_NothingToUnfold () ->
      (* NOTE: *)
      Log.trace ~__FUNCTION__ "Exception: Mebi_proof_NothingToUnfold";
      raise (Hyp.Mebi_proof_Hypothesis_HTy (ty, tys))
  ;;

  let opt_of_hty (gl : Proofview.Goal.t) (p : EConstr.t Rocq_utils.kind_pair)
    : k option
    =
    Log.trace __FUNCTION__;
    try Some (of_hty gl p) with Hyp.Mebi_proof_Hypothesis_HTy _ -> None
  ;;

  let hty_is_a (gl : Proofview.Goal.t) (p : EConstr.t Rocq_utils.kind_pair)
    : bool
    =
    Log.trace __FUNCTION__;
    Option.has_some (opt_of_hty gl p)
  ;;

  let of_hyp (gl : Proofview.Goal.t) (h : Rocq_utils.hyp) : t =
    Log.trace __FUNCTION__;
    let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
    Log.thing ~__FUNCTION__ Debug "hyp" h (fhyp gl);
    let p = Rocq_utils.hyp_to_atomic sigma h in
    Log.things ~__FUNCTION__ Debug "tys" (snd p |> Array.to_list) (feconstr gl);
    try
      let kind : k = of_hty gl p in
      match kind with
      | ToUnfold xs ->
        Log.things ~__FUNCTION__ Debug "ToUnfold" xs (feconstr gl);
        { kind; tactic = chain_do_unfold_in_hyp gl h xs }
      | _ -> { kind; tactic = do_inversion h }
    with
    | Hyp.Mebi_proof_Hypothesis_HTy p ->
      Log.trace ~__FUNCTION__ "Exception: Mebi_proof_Hypothesis_HTy";
      raise (Hyp.Mebi_proof_Hypothesis_Hyp (h, p))
    | Mebi_proof_NothingToUnfold () ->
      Log.trace ~__FUNCTION__ "Exception: Mebi_proof_NothingToUnfold";
      raise (Hyp.Mebi_proof_Hypothesis_Hyp (h, p))
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

    let of_hty
          (gl : Proofview.Goal.t)
          ((ty, tys) : EConstr.t Rocq_utils.kind_pair)
      : t
      =
      try get_lts_transition gl (mfsm ()) (ty, tys) with
      | Mebi_proof_CouldNotDecodeTransitionState (x, fsm) ->
        raise (Hyp.Mebi_proof_Hypothesis_HTy (ty, tys))
      | Mebi_proof_CouldNotDecodeTransitionLabel (x, fsm) ->
        raise (Hyp.Mebi_proof_Hypothesis_HTy (ty, tys))
      | Mebi_proof_CouldNotObtainAction (from, label, goto, fsm) ->
        raise (Hyp.Mebi_proof_Hypothesis_HTy (ty, tys))
      | Mebi_proof_TyDoesNotMatchTheories (ty, tys) ->
        raise (Hyp.Mebi_proof_Hypothesis_HTy (ty, tys))
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
    let p : EConstr.t Rocq_utils.kind_pair = Rocq_utils.hyp_to_atomic sigma h in
    if Cofix.hty_is_a gl p
    then true
    else if Invertible.hty_is_a gl p
    then true
    else if TransOpt.hty_is_a gl p
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

exception
  Mebi_proof_ConclIsNot_Conj of (Evd.evar_map * EConstr.t Rocq_utils.kind_pair)

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

let wk_sim_state (gl : Proofview.Goal.t) : wk_sim_state -> EConstr.t =
  Log.trace __FUNCTION__;
  let f (c : Proofview.Goal.t -> EConstr.t Rocq_utils.kind_pair -> bool) wk =
    let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
    let x : EConstr.t Rocq_utils.kind_pair =
      Rocq_utils.econstr_to_atomic sigma wk
    in
    if c gl x then snd x else raise (Mebi_proof_CouldNotGetWkSimState ())
  in
  function
  | M { wk_sim; _ } -> (f typ_is_weak_sim wk_sim).(5)
  | N { wk_trans; _ } -> (f typ_is_weak_transition wk_trans).(3)
;;

let wk_conj_get_state
      (gl : Proofview.Goal.t)
      (stateof : wk_sim_state)
      (fsm : Fsm.t)
  : State.t
  =
  Log.trace __FUNCTION__;
  try find_state gl (wk_sim_state gl stateof) fsm.states with
  | Mebi_proof_CouldNotDecodeState (statety, states) ->
    Log.thing ~__FUNCTION__ Debug "states" states (Args Model.states_to_string);
    raise (Mebi_proof_CouldNotGetWkSimState ())
;;

exception Mebi_proof_CouldNotGetWkSimStateM of concl_conj

let get_mstate (gl : Proofview.Goal.t) (conj : concl_conj) : State.t =
  Log.trace __FUNCTION__;
  try wk_conj_get_state gl (M conj) (mfsm ()) with
  | Mebi_proof_CouldNotGetWkSimState () ->
    raise (Mebi_proof_CouldNotGetWkSimStateM conj)
;;

exception Mebi_proof_CouldNotGetWkSimStateN of concl_conj

let get_nstate (gl : Proofview.Goal.t) (conj : concl_conj) : State.t =
  Log.trace __FUNCTION__;
  try wk_conj_get_state gl (N conj) (nfsm ()) with
  | Mebi_proof_CouldNotGetWkSimState () ->
    raise (Mebi_proof_CouldNotGetWkSimStateN conj)
;;

(***********************************************************************)

let hyps_has_cofix (gl : Proofview.Goal.t) : bool =
  Log.trace __FUNCTION__;
  List.exists
    (fun (h : Rocq_utils.hyp) ->
      econstr_eq_concl gl (Context.Named.Declaration.get_type h))
    (Proofview.Goal.hyps gl)
;;

let hyp_is_invertible (gl : Proofview.Goal.t) : Rocq_utils.hyp -> bool =
  Log.trace __FUNCTION__;
  fun (x : Rocq_utils.hyp) ->
    try Invertible.hyp_is_a gl x with Hyp.Mebi_proof_Hypothesis_Hyp _ -> false
;;

let _hyps_has_invertible (gl : Proofview.Goal.t) : bool =
  Log.trace __FUNCTION__;
  (* List.exists (hyp_is_invertible gl) (Proofview.Goal.hyps gl) *)
  List.fold_left
    (fun (acc : bool) (x : Rocq_utils.hyp) -> acc || hyp_is_invertible gl x)
    false
    (Proofview.Goal.hyps gl)
;;

let hyps_get_invertibles (gl : Proofview.Goal.t) : Invertible.t list =
  Log.trace __FUNCTION__;
  List.filter_map
    (fun (x : Rocq_utils.hyp) ->
      try Some (Invertible.of_hyp gl x) with
      | Hyp.Mebi_proof_Hypothesis_Hyp _ -> None)
    (Proofview.Goal.hyps gl)
;;

let hyps_invertibles_get_full (xs : Invertible.t list) : Invertible.t option =
  let f = function ({ kind = Full; _ } : Invertible.t) -> true | _ -> false in
  match List.filter f xs with [] -> None | h :: _ -> Some h
;;

let hyps_invertibles_get_layer (xs : Invertible.t list) : Invertible.t option =
  let f = function
    | ({ kind = Layer; _ } : Invertible.t) -> true
    | _ -> false
  in
  match List.filter f xs with [] -> None | h :: _ -> Some h
;;

let hyps_invertibles_get_tounfold (xs : Invertible.t list) : Invertible.t option
  =
  let f = function
    | ({ kind = ToUnfold _; _ } : Invertible.t) -> true
    | _ -> false
  in
  match List.filter f xs with [] -> None | h :: _ -> Some h
;;

exception Mebi_proof_NoInvertibleHyps of Invertible.t list

let hyps_get_invertible (gl : Proofview.Goal.t) : Invertible.t =
  Log.trace __FUNCTION__;
  let invertibles : Invertible.t list = hyps_get_invertibles gl in
  let f : Invertible.t Utils.Strfy.to_string = Of Invertible.to_string in
  Log.things ~__FUNCTION__ Debug "invertibles" invertibles f;
  match hyps_invertibles_get_tounfold invertibles with
  | Some x -> x
  | None ->
    (match hyps_invertibles_get_full invertibles with
     | Some x -> x
     | None ->
       (match hyps_invertibles_get_layer invertibles with
        | Some x -> x
        | None -> raise (Mebi_proof_NoInvertibleHyps invertibles)))
;;

let do_hyp_inversion (gl : Proofview.Goal.t) : tactic =
  Log.trace __FUNCTION__;
  (hyps_get_invertible gl).tactic
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
exception Mebi_proof_NLabel_IsNone of unit

let try_get_ngoto ?(saturated : bool = false) (mgoto : State.t)
  : Transition_opt.t -> State.t
  =
  Log.trace __FUNCTION__;
  (* *)
  function
  | { label = None; _ } -> raise (Mebi_proof_NLabel_IsNone ())
  | { from = nfrom; label = Some nlabel; goto = None; _ } ->
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
exception Mebi_proof_ExIntro_NLabelIsNone of (Transition.t * Transition_opt.t)

let assert_transition_opt_labels_eq (m : Transition.t) (n : Transition_opt.t)
  : unit
  =
  let nlabel =
    Option.cata
      (fun x -> x)
      (raise (Mebi_proof_ExIntro_NLabelIsNone (m, n)))
      n.label
  in
  if Label.equal m.label nlabel
  then ()
  else raise (Mebi_proof_ExIntro_NEqLabel (m, n))
;;

exception Mebi_proof_ExIntro_NotBisimilar of (State.t * State.t)

let do_nsome (gl : Proofview.Goal.t) (mtrans : Transition.t) : tactic =
  Log.trace __FUNCTION__;
  let { wk_trans; wk_sim } : concl_conj = concl_wk_sim gl in
  let mstate : State.t = get_mstate gl { wk_trans; wk_sim } in
  Log.thing ~__FUNCTION__ Debug "mstate" mstate (Args State.to_string);
  let ntrans : Transition_opt.t = get_weak_ntransition gl wk_trans in
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
    let nstate = concl_wk_sim gl |> get_nstate gl in
    if are_states_bisimilar mtransition.goto nstate then Some nstate else None)
  else None
;;

exception Mebi_proof_ConclIsNot_Exists of unit

let do_eexists_transition (gl : Proofview.Goal.t) : tactic =
  Log.trace __FUNCTION__;
  try
    (* NOTE: there must be a Hyp for the current m transition *)
    let mtransition : Transition.t = get_mtransition gl in
    log_transition ~__FUNCTION__ "mtransition" mtransition;
    match can_do_nrefl gl mtransition with
    | None -> do_nsome gl mtransition
    | Some nstate -> do_nnone gl nstate
  with
  | Mebi_proof_ExpectedMTransition_Some_Label mtrans_opt ->
    Log.trace ~__FUNCTION__ "Mebi_proof_ExpectedMTransition_Some_Label";
    log_transition_opt ~__FUNCTION__ "mtrans_opt" mtrans_opt;
    raise (Mebi_proof_ExpectedMTransition_Some_Label mtrans_opt)
  | Mebi_proof_ExpectedMTransition_Some_Goto mtrans_opt ->
    Log.trace ~__FUNCTION__ "Mebi_proof_ExpectedMTransition_Some_Goto";
    log_transition_opt ~__FUNCTION__ "mtrans_opt" mtrans_opt;
    raise (Mebi_proof_ExpectedMTransition_Some_Goto mtrans_opt)
  | Mebi_proof_StatesNotBisimilar (mstate, nstate, bisim_states) ->
    Log.trace ~__FUNCTION__ "Mebi_proof_StatesNotBisimilar";
    Log.thing ~__FUNCTION__ Debug "bisim-states" (the_bisim_states ()) fpi;
    raise (Mebi_proof_ExIntro_NotBisimilar (mstate, nstate))
  | Mebi_proof_TransitionOptStatesNotBisimilar (mstate, nstate, bisim_states) ->
    Log.trace ~__FUNCTION__ "Mebi_proof_TransitionOptStatesNotBisimilar";
    raise (Mebi_proof_ExIntro_NotBisimilar (mstate, nstate))
  | Mebi_proof_CouldNotGetWkSimStateM conj ->
    Log.trace ~__FUNCTION__ "Mebi_proof_CouldNotGetWkSimStateM";
    log_econstr ~__FUNCTION__ "wk_sim" conj.wk_sim gl;
    raise (Mebi_proof_ConclIsNot_Exists ())
  | Mebi_proof_CouldNotGetWkSimStateN conj ->
    Log.trace ~__FUNCTION__ "Mebi_proof_CouldNotGetWkSimStateN";
    log_econstr ~__FUNCTION__ "wk_trans" conj.wk_trans gl;
    raise (Mebi_proof_ConclIsNot_Exists ())
  | Rocq_utils.Rocq_utils_EConstrIsNot_Atomic (sigma, x, k) ->
    Log.trace ~__FUNCTION__ "Rocq_utils_EConstrIsNot_Atomic";
    raise (Mebi_proof_ConclIsNot_Exists ())
  | Mebi_proof_TyDoesNotMatchTheories (ty, tys) ->
    Log.trace ~__FUNCTION__ "Mebi_proof_TyDoesNotMatchTheories";
    raise (Mebi_proof_ConclIsNot_Exists ())
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
    Log.warning ~__FUNCTION__ "Expected Hypothesis to be empty";
    raise (Mebi_proof_NewProof ()))

and handle_new_weak_sim (gl : Proofview.Goal.t) : tactic =
  Log.trace __FUNCTION__;
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let the_concl : EConstr.t = Proofview.Goal.concl gl in
  let concltyp : EConstr.t Rocq_utils.kind_pair =
    Rocq_utils.econstr_to_atomic sigma the_concl
  in
  (* NOTE: [auto] when concl is [weak_sim] and hyp has corresponding [cofix] *)
  if typ_is_weak_sim gl concltyp
  then (
    Log.trace ~__FUNCTION__ "(concl is weak sim)";
    (* TODO: just try to invert and then catch error if nothing to invert *)
    try do_any_unfold_concl ~enforce:true gl with
    | Mebi_proof_NothingToUnfold _ ->
      if hyps_has_cofix gl
      then do_solve_cofix gl
      else (
        set_the_proof_state ~__FUNCTION__ NewCofix;
        do_new_cofix gl))
  else if typ_is_exists gl concltyp
  then (
    Log.trace ~__FUNCTION__ "(concl is exists)";
    set_the_proof_state ~__FUNCTION__ NewCofix;
    handle_proof_state gl)
  else (
    warn_handle_new_weak_sim gl;
    raise (Mebi_proof_NewWeakSim ()))

and handle_new_cofix (gl : Proofview.Goal.t) : tactic =
  Log.trace __FUNCTION__;
  try do_hyp_inversion gl with
  | Mebi_proof_NoInvertibleHyps hyps ->
    (try do_eexists_transition gl with
     | Mebi_proof_CheckTermCanBeUnfolded (h, x) ->
       Log.trace ~__FUNCTION__ "Mebi_proof_CheckTermCanBeUnfolded";
       log_econstr ~__FUNCTION__ "trying to unfold" x gl;
       (try get_econstrs_to_unfold gl x |> chain_do_unfold_in_hyp gl h with
        | Mebi_proof_NothingToUnfold () ->
          Log.trace ~__FUNCTION__ "Mebi_proof_NothingToUnfold";
          log_econstr ~__FUNCTION__ "Could not handle (or unfold) term" x gl;
          raise (Mebi_proof_NewCofix ()))
     | Mebi_proof_CouldNotFindHypTransition (fsm, hyps) ->
       Log.trace ~__FUNCTION__ "Mebi_proof_CouldNotFindHypTransition";
       log_hyps ~__FUNCTION__ "Could not find any transitions in Hyps" hyps gl;
       (* log_fsm ~__FUNCTION__ "(the fsm)" fsm; *)
       raise (Mebi_proof_NewCofix ())
     | Mebi_proof_CouldNotObtainAction (from, label, goto, fsm) ->
       Log.trace ~__FUNCTION__ "Mebi_proof_CouldNotObtainAction";
       raise (Mebi_proof_NewCofix ())
     | Mebi_proof_CouldNotDecodeTransitionState (x, fsm) ->
       Log.trace ~__FUNCTION__ "Mebi_proof_CouldNotDecodeTransitionState";
       log_econstr ~__FUNCTION__ "Could not decode transition state" x gl;
       (* log_states ~__FUNCTION__ "fsm.states" fsm.states; *)
       raise (Mebi_proof_NewCofix ())
     | Mebi_proof_CouldNotDecodeTransitionLabel (x, fsm) ->
       Log.trace ~__FUNCTION__ "Mebi_proof_CouldNotDecodeTransitionLabel";
       log_econstr ~__FUNCTION__ "Could not decode transition label" x gl;
       (* log_alphabet ~__FUNCTION__ "fsm.alphabet" fsm.alphabet; *)
       raise (Mebi_proof_NewCofix ())
     | Mebi_proof_ConclIsNot_Exists () ->
       Log.trace ~__FUNCTION__ "Mebi_proof_ConclIsNot_Exists";
       raise (Mebi_proof_NewCofix ())
     | Mebi_proof_ExIntro_NEqStateM (mstate, mfrom) ->
       Log.trace ~__FUNCTION__ "Mebi_proof_ExIntro_NEqStateM";
       Log.thing ~__FUNCTION__ Debug "mstate" mstate (Args State.to_string);
       Log.option ~__FUNCTION__ Debug "mfrom" mfrom (Args State.to_string);
       raise (Mebi_proof_NewCofix ())
     | Mebi_proof_ExIntro_NEqLabel (mtrans, ntrans) ->
       Log.trace ~__FUNCTION__ "Mebi_proof_ExIntro_NEqLabel";
       log_label ~__FUNCTION__ "mtrans.label" mtrans.label;
       log_label_opt ~__FUNCTION__ "ntrans.label" ntrans.label;
       raise (Mebi_proof_NewCofix ()))

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
  log_transition ~__FUNCTION__ "mtrans" mtrans;
  log_transition_opt ~__FUNCTION__ "ntrans" ntrans;
  log_concl ~__FUNCTION__ "concl" gl;
  try
    let { from = nfrom; label = nlabel; goto = ngoto; _ } : Transition_opt.t =
      Proofview.Goal.concl gl
      |> Rocq_utils.econstr_to_atomic sigma
      |> get_weak_transition gl (nfsm ~saturated:true ())
    in
    let nlabel =
      Option.cata
        (fun x -> x)
        (Log.trace ~__FUNCTION__ "Exception: nlabel is None";
         raise (Mebi_proof_GoalTransition ()))
        nlabel
    in
    let ngoto =
      Option.cata
        (fun x ->
          assert_states_bisimilar mtrans.goto x;
          x)
        (Log.trace ~__FUNCTION__ "Exception: ngoto is None";
         raise (Mebi_proof_GoalTransition ()))
        ngoto
    in
    Log.thing ~__FUNCTION__ Debug "mgoto" mtrans.goto fstate;
    Log.thing ~__FUNCTION__ Debug "ngoto" ngoto fstate;
    log_label ~__FUNCTION__ "ntrans.label" nlabel;
    do_constructor_transition gl nfrom nlabel ngoto
  with
  | Mebi_proof_CouldNotDecodeTransitionState (x, fsm) ->
    Log.trace ~__FUNCTION__ "Mebi_proof_CouldNotDecodeTransitionState";
    Log.thing ~__FUNCTION__ Debug "transition state" x (feconstr gl);
    Log.thing ~__FUNCTION__ Debug "states" fsm.states fstates;
    raise (Mebi_proof_GoalTransition ())
  | Mebi_proof_CouldNotObtainAction (from, label, goto, fsm) ->
    Log.trace ~__FUNCTION__ "Mebi_proof_CouldNotObtainAction";
    raise (Mebi_proof_GoalTransition ())
  | Mebi_proof_CouldNotDecodeTransitionLabel (x, fsm) ->
    Log.trace ~__FUNCTION__ "Mebi_proof_CouldNotDecodeTransitionLabel";
    Log.thing ~__FUNCTION__ Debug "transition label" x (feconstr gl);
    Log.thing ~__FUNCTION__ Debug "alphabet" fsm.alphabet falphabet;
    raise (Mebi_proof_GoalTransition ())
  | Mebi_proof_StatesNotBisimilar (mstate, nstate, pi) ->
    Log.trace ~__FUNCTION__ "Mebi_proof_StatesNotBisimilar";
    Log.thing ~__FUNCTION__ Debug "non-bisimilar" (the_bisim_states ()) fpi;
    Log.thing ~__FUNCTION__ Debug "mfrom" mstate fstate;
    Log.thing ~__FUNCTION__ Debug "ngoto" nstate fstate;
    raise (Mebi_proof_GoalTransition ())
  | Mebi_proof_TyDoesNotMatchTheories (ty, tys) ->
    Log.trace ~__FUNCTION__ "Mebi_proof_TyDoesNotMatchTheories";
    Log.thing ~__FUNCTION__ Debug "ty" ty (feconstr gl);
    Log.things ~__FUNCTION__ Debug "args" (Array.to_list tys) (feconstr gl);
    raise (Mebi_proof_GoalTransition ())

and handle_apply_constructors (gl : Proofview.Goal.t)
  : ApplicableConstructors.t -> tactic
  =
  Log.trace __FUNCTION__;
  Log.thing ~__FUNCTION__ Debug "concl" gl (Of concl_to_string);
  (* TODO: inchworm *)
  function
  | { current = h :: tl; annotation; destination } ->
    Log.trace ~__FUNCTION__ "applying current h";
    set_the_proof_state
      ~__FUNCTION__
      (ApplyConstructors { current = tl; annotation; destination });
    (* TODO: obtain [{from;label;goto}] terms from concl *)
    Log.debug ~__FUNCTION__ "TODO: obtain [{from;label;goto}] terms from concl";
    let conclpair = concl_to_atomic gl in
    let f args =
      tactic_chain
        [ ApplicableConstructors.get_constructor_tactic gl h args
        ; do_any_unfold_concl ~enforce:false gl
        ]
    in
    let fsm = nfsm () in
    if typ_is_tau gl conclpair
    then (
      Log.trace ~__FUNCTION__ "applying current h => is tau";
      let ty, tys = conclpair in
      Log.thing ~__FUNCTION__ Debug "conclpair ty" ty (feconstr gl);
      Log.things
        ~__FUNCTION__
        Debug
        "conclpair tys"
        (Array.to_list tys)
        (feconstr gl);
      try
        (* NOTE: index (3) since [tau lts x] => [tau (term * label) x] *)
        let from : Enc.t = (find_state gl tys.(3) fsm.states).enc in
        let label : Enc.t option = Some (get_theory_none_enc ()) in
        let goto : Enc.t option = None in
        (* TODO: get the info from the fsm, then use the cmaps to get the bindings *)
        (* FIXME: all of this needs to be optimized/streamlined*)
        f (Some (Enc (from, label, goto)))
      with
      | Mebi_proof_CouldNotDecodeState (ty, states) ->
        Log.trace ~__FUNCTION__ "Exception: Mebi_proof_CouldNotDecodeState";
        Log.thing ~__FUNCTION__ Debug "Could not decode State" ty (feconstr gl);
        raise (Mebi_proof_CouldNotDecodeTransitionState (ty, fsm))
      | Mebi_proof_CouldNotDecodeLabel (ty, alphabet) ->
        Log.trace ~__FUNCTION__ "Exception: Mebi_proof_CouldNotDecodeLabel";
        Log.thing ~__FUNCTION__ Debug "Could not decode Label" ty (feconstr gl);
        raise (Mebi_proof_CouldNotDecodeTransitionLabel (ty, fsm)))
    else (
      (* Log.debug ~__FUNCTION__ "ERR: concl not tau?"; *)
      (* _do_nothing () *)
      (* TODO: check if this only occurs *after* some top-level constructor (i.e., once we have already "filled in" any of the bindings) *)
      Log.trace ~__FUNCTION__ "applying current h => is lts transition";
      let ty, tys = conclpair in
      Log.thing ~__FUNCTION__ Debug "conclpair ty" ty (feconstr gl);
      Log.things
        ~__FUNCTION__
        Debug
        "conclpair tys"
        (Array.to_list tys)
        (feconstr gl);
      (* f None *)
      (* f (Some (Raw (tys.(0), Some tys.(1), None))) *)
      f (Some (Raw (tys.(0), None, None))))
  | { current = []; annotation = None; destination } ->
    Log.trace ~__FUNCTION__ "finished applying constructors";
    set_the_proof_state ~__FUNCTION__ NewWeakSim;
    tactic_chain [ do_simplify gl; do_eapply_rt1n_refl gl; do_simplify gl ]
  | { current = []
    ; annotation = Some { this = { from; via; using; goto }; next }
    ; destination
    } ->
    Log.trace ~__FUNCTION__ "updating current";
    set_the_proof_state
      ~__FUNCTION__
      (ApplyConstructors
         { current = Tree.min using; annotation = next; destination });
    tactic_chain [ do_any_unfold_concl ~enforce:false gl; do_rt1n_via gl via ]
(* let constructor,annotation : Note.annotation option =
      match annotation with
      | { this={ from; via; using; goto }; next = None } -> None
      | { this; next = Some next } -> Some next
    in *)

(* and handle_apply_constructors' (gl : Proofview.Goal.t)
  : ApplicableConstructors.t -> tactic
  =
  Log.trace __FUNCTION__;
  Log.thing ~__FUNCTION__ Debug "concl" gl (Of concl_to_string);
  (* TODO: inchworm *)
  function
  | { annotation; tactics = Some { this; next }; goto } ->
    (* | { annotation; tactics = Some (h :: tl); goto } -> *)
    (* TODO: need to update [get_constructor_tactic] so that *)
    Log.trace ~__FUNCTION__ "apply tactic";
    set_the_proof_state
      ~__FUNCTION__
      (ApplyConstructors { annotation; tactics = Some tl; goto });
    tactic_chain [ this; do_simplify gl ]
  | { annotation = Some annotation; tactics; goto } ->
    Log.trace ~__FUNCTION__ "make new tactics";
    do_build_constructor_tactics gl goto annotation
  | { annotation = None; tactics; goto } ->
    Log.trace ~__FUNCTION__ "finished applying constructors";
    set_the_proof_state ~__FUNCTION__ NewWeakSim;
    tactic_chain [ do_simplify gl; do_eapply_rt1n_refl gl; do_simplify gl ] *)

and handle_proof_state (gl : Proofview.Goal.t) : tactic =
  Log.trace __FUNCTION__;
  (* Log.thing ~__FUNCTION__ Debug "hyps" gl (Of hyps_to_string); *)
  (* Log.thing ~__FUNCTION__ Debug "concl" gl (Of concl_to_string); *)
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
  | Mebi_proof_CouldNotDecodeEConstr x ->
    Log.thing ~__FUNCTION__ Debug "econstr" x (Of (econstr_to_string gl));
    raise (Mebi_proof_CouldNotDecodeEConstr x)

and detect_proof_state (gl : Proofview.Goal.t) : tactic =
  Log.trace __FUNCTION__;
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  if
    Proofview.Goal.concl gl
    |> Rocq_utils.econstr_to_atomic sigma
    |> typ_is_weak_sim gl
    && hyps_has_cofix gl
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
  Log.debug "\n- - - - - - - - - -\n";
  Log.trace __FUNCTION__;
  Mebi_theories.tactics
    [ Proofview.Goal.enter (fun gl ->
        get_tactic ~short:false ~state:true (handle_proof_state gl))
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
      Log.thing
        ~__FUNCTION__
        Notice
        "iter"
        (upper_bound - n)
        (Of Utils.Strfy.int);
      Mebi_tactics.update_proof_by_tactic pstate (step ()) |> iter_body (n - 1)
  in
  let rem, pstate = iter_body upper_bound pstate in
  Log.notice
    (Printf.sprintf "Stopped after (%i) iterations." (upper_bound - rem));
  pstate
;;
