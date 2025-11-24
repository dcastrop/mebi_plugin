open Logging
open Mebi_wrapper
open Mebi_proof
open Model

type hyp_cofix =
  { m : State.t
  ; n : State.t
  }

type proof_state =
  | NewProof
  | NewWeakSim
  | NewCofix
  | NewTransition of Transition_opt.t
  | GoalTransition of Transition_opt.t
  | Constructors of
      (Note.annotations * (unit -> unit Proofview.tactic) list option)

let proof_state_to_string : proof_state -> string = function
  | NewProof -> "NewProo\n"
  | NewWeakSim -> "NewWeakSim\n"
  | NewCofix -> "NewCofix\n"
  | NewTransition transition ->
    Printf.sprintf "NewTransition:\n%s\n" (Transition_opt.to_string transition)
  | GoalTransition transition ->
    Printf.sprintf "GoalTransition:\n%s\n" (Transition_opt.to_string transition)
  | Constructors (annotations, tactics_opt) ->
    Printf.sprintf
      "Constructors, tactics to apply: %s\n%s\n"
      (Option.cata
         (fun tactics -> Printf.sprintf "Some |%i|" (List.length tactics))
         "None"
         tactics_opt)
      (Note.annotations_to_string annotations)
;;

let the_proof_state : proof_state ref = ref NewProof
let reset_the_proof_state () : unit = the_proof_state := NewProof

(***********)

let the_cached_result : Algorithms.Bisimilar.result ref option ref = ref None
let reset_the_cached_result () : unit = the_cached_result := None

exception MissingBisimResult of unit

let get_the_result () : Algorithms.Bisimilar.result ref =
  Log.trace "mebi_bisim.get_the_result";
  match !the_cached_result with
  | None -> raise (MissingBisimResult ()) (* missing_bisim_result () *)
  | Some r -> r
;;

(* let get_m () : Fsm.t = !(get_the_result ()).the_fsm_1 *)
let get_n () : Fsm.t = !(get_the_result ()).the_fsm_2
let get_bisim_states () : Partition.t = !(get_the_result ()).bisim_states

(* let get_non_bisim_states () : Partition.t =
   !(get_the_result ()).non_bisim_states
   ;; *)

let set_the_result (new_r : Algorithms.Bisimilar.result) : unit =
  match !the_cached_result with
  | None ->
    Log.trace "mebi_bisim.set_the_result (was None)";
    the_cached_result := Some (ref new_r)
  | Some _r ->
    Log.trace "mebi_bisim.set_the_result (was Some)";
    the_cached_result := Some (ref new_r)
;;

(********************************)

exception Enc_Of_EConstr_NotFound of (EConstr.t * States.t)
exception State_Of_Enc_NotFound of (Enc.t * States.t)
exception Error_Multiple_States_Of_Enc_Found of (Enc.t * States.t)

let find_state_of_enc (x : Enc.t) (s : States.t) : State.t =
  match
    List.filter
      (fun ({ enc = y; _ } : State.t) -> Mebi_setup.Enc.equal x y)
      (States.to_list s)
  with
  | h :: [] -> h
  | [] -> raise (State_Of_Enc_NotFound (x, s))
  | _ :: _ -> raise (Error_Multiple_States_Of_Enc_Found (x, s))
;;

let find_state_of_enc_opt (x : Enc.t) (s : States.t) : State.t option =
  (* match x with None -> None | Some x -> Some (find_state_of_enc x s) *)
  match
    List.filter
      (fun ({ enc = y; _ } : State.t) -> Mebi_setup.Enc.equal x y)
      (States.to_list s)
  with
  | h :: [] -> Some h
  | [] -> None
  | _ :: _ -> raise (Error_Multiple_States_Of_Enc_Found (x, s))
;;

exception Label_Of_Enc_NotFound of (Enc.t * Alphabet.t)
exception Error_Multiple_Labels_Of_Enc_Found of (Enc.t * Alphabet.t)

let find_label_of_enc (x : Enc.t) (s : Alphabet.t) : Label.t =
  match
    List.filter
      (fun ({ enc = y; _ } : Label.t) -> Mebi_setup.Enc.equal x y)
      (Alphabet.to_list s)
  with
  | h :: [] -> h
  | [] -> raise (Label_Of_Enc_NotFound (x, s))
  | _ :: _ -> raise (Error_Multiple_Labels_Of_Enc_Found (x, s))
;;

let econstr_to_enc : EConstr.t -> Enc.t =
  fun (x : EConstr.t) -> run ~keep_encoding:true ~fresh:false (get_encoding x)
;;

let run_econstr_to_enc_opt : EConstr.t -> Enc.t option =
  fun (x : EConstr.t) ->
  run ~keep_encoding:true ~fresh:false (get_encoding_opt x)
;;

let enc_to_econstr : Enc.t -> EConstr.t =
  fun (x : Enc.t) -> run ~keep_encoding:true ~fresh:false (get_decoding x)
;;

(* let econstr_to_enc_opt (sigma : Evd.evar_map) : EConstr.t -> Enc.t option =
   fun (x : EConstr.t) ->
   if EConstr.isRel sigma x then None else Some (econstr_to_enc x)
   ;; *)

let econstr_to_enc_opt (sigma : Evd.evar_map) : EConstr.t -> Enc.t option =
  fun (x : EConstr.t) ->
  if EConstr.isRel sigma x then None else run_econstr_to_enc_opt x
;;

let get_concl_transition
      ?(abort_on_failed_dest_enc : bool = false)
      (m : Fsm.t)
      sigma
      ((from_term, action_term, dest_term) : EConstr.t * EConstr.t * EConstr.t)
  : Transition_opt.t
  =
  Log.debug "mebi_bisim.get_concl_transition";
  let from : State.t = find_state_of_enc (econstr_to_enc from_term) m.states in
  let action : Action.t =
    Edges.find m.edges from
    |> get_action_labelled
         (find_label_of_enc (econstr_to_enc action_term) m.alphabet)
  in
  match econstr_to_enc_opt sigma dest_term with
  | None ->
    if abort_on_failed_dest_enc
    then raise (Enc_Of_EConstr_NotFound (dest_term, m.states))
    else
      Transition_opt.create
        from
        action.label
        None
        action.annotations
        action.constructor_trees
  | Some dest_enc ->
    Transition_opt.create
      from
      action.label
      (find_state_of_enc_opt dest_enc m.states)
      action.annotations
      action.constructor_trees
;;

let get_weak_transition m sigma tys : Transition_opt.t =
  Log.debug
    (Printf.sprintf "mebi_bisim.get_weak_transition, %i" (Array.length tys));
  get_concl_transition m sigma (tys.(3), tys.(5), tys.(4))
;;

let get_lts_transition m sigma tys : Transition_opt.t =
  Log.debug
    (Printf.sprintf "mebi_bisim.get_lts_transition, %i" (Array.length tys));
  get_concl_transition
    ~abort_on_failed_dest_enc:true
    m
    sigma
    (tys.(0), tys.(1), tys.(2))
;;

let get_hyp_transition (m : Fsm.t) env sigma (tys : EConstr.t array)
  : Transition_opt.t option
  =
  Log.debug
    (Printf.sprintf "mebi_bisim.get_hyp_transition, %i" (Array.length tys));
  match econstr_to_enc_opt sigma tys.(0) with
  | None -> None
  | Some from_enc ->
    Log.debug "mebi_bisim.get_hyp_transition, Some from enc";
    (match find_state_of_enc_opt from_enc m.states with
     | None -> None
     | Some from ->
       Log.debug "mebi_bisim.get_hyp_transition, Some from";
       (match econstr_to_enc_opt sigma tys.(1) with
        | None -> raise (Enc_Of_EConstr_NotFound (tys.(1), m.states))
        | Some action_enc ->
          Log.debug "mebi_bisim.get_hyp_transition, Some action enc";
          Log.debug
            (Printf.sprintf
               "mebi_bisim.get_hyp_transition, dest:\n%s"
               (Rocq_utils.Strfy.econstr env sigma tys.(2)));
          (match econstr_to_enc_opt sigma tys.(2) with
           | None -> None
           | Some dest_enc ->
             Log.debug "mebi_bisim.get_hyp_transition, Some dest enc";
             let action : Action.t =
               get_action_labelled
                 (find_label_of_enc action_enc m.alphabet)
                 (Edges.find m.edges from)
             in
             Some
               (Transition_opt.create
                  from
                  action.label
                  (find_state_of_enc_opt dest_enc m.states)
                  action.annotations
                  action.constructor_trees))))
;;

let get_cofix (m : Fsm.t) (n : Fsm.t) env sigma (tys : EConstr.t array)
  : hyp_cofix
  =
  Log.debug (Printf.sprintf "mebi_bisim.get_cofix, %i" (Array.length tys));
  { m = find_state_of_enc (econstr_to_enc tys.(5)) m.states
  ; n = find_state_of_enc (econstr_to_enc tys.(6)) n.states
  }
;;

exception Invalid_KindOf_EConstr_Expected_Atomic of EConstr.t

let get_atomic_type sigma x : EConstr.t * EConstr.t array =
  Log.debug "mebi_bisim.get_atomic_type";
  match EConstr.kind_of_type sigma x with
  | AtomicType (ty, tys) -> ty, tys
  | _ -> raise (Invalid_KindOf_EConstr_Expected_Atomic x)
;;

exception Invalid_KindOf_EConstr_Expected_Lambda of EConstr.t

let get_lambda sigma x
  : (Names.Name.t, Evd.erelevance) Context.pbinder_annot * EConstr.t * EConstr.t
  =
  Log.debug "mebi_bisim.get_lambda";
  match EConstr.kind sigma x with
  | Lambda (binder, types, constr) -> binder, types, constr
  | _ -> raise (Invalid_KindOf_EConstr_Expected_Lambda x)
;;

exception Invalid_KindOf_EConstr_Expected_App of EConstr.t

let get_app sigma x : EConstr.t * EConstr.t array =
  Log.debug "mebi_bisim.get_app";
  match EConstr.kind sigma x with
  | App (ty, tys) -> ty, tys
  | _ -> raise (Invalid_KindOf_EConstr_Expected_App x)
;;

exception UnhandledConcl of EConstr.t

type concl_transition =
  | Weak_Transition
  | Silent_Transition
  | Silent1_Transition
  | LTS_Transition

let try_get_weak_transition sigma n (ty, tys)
  : (concl_transition * Transition_opt.t) option
  =
  Log.debug
    (Printf.sprintf "mebi_bisim.try_get_weak_transition, %i" (Array.length tys));
  if Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_weak ())
  then
    Some
      (Weak_Transition, get_concl_transition n sigma (tys.(3), tys.(5), tys.(4)))
  else None
;;

let try_get_silent_transition sigma n (ty, tys)
  : (concl_transition * Transition_opt.t) option
  =
  Log.debug
    (Printf.sprintf
       "mebi_bisim.try_get_silent_transition, %i"
       (Array.length tys));
  if Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_silent ())
  then
    Some
      ( Silent_Transition
      , get_concl_transition n sigma (tys.(2), tys.(4), tys.(3)) )
  else if Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_silent1 ())
  then
    Some
      (LTS_Transition, get_concl_transition n sigma (tys.(2), tys.(4), tys.(3)))
  else None
;;

let try_get_lts_transition sigma n (ty, tys)
  : (concl_transition * Transition_opt.t) option
  =
  Log.debug
    (Printf.sprintf "mebi_bisim.try_get_lts_transition, %i" (Array.length tys));
  if Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_LTS ())
  then
    Some
      ( Silent_Transition
      , get_concl_transition n sigma (tys.(0), tys.(1), tys.(2)) )
  else None
;;

let try_get_concl_transition gl n x
  : (concl_transition * Transition_opt.t) option
  =
  Log.debug "mebi_bisim.try_to_get_concl_transition";
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let ty, tys = get_atomic_type sigma x in
  Log.debug
    (Printf.sprintf
       "mebi_bisim.try_to_get_concl_transition, %i"
       (Array.length tys));
  match Array.length tys with
  | 6 -> try_get_weak_transition sigma n (ty, tys)
  | 5 -> try_get_silent_transition sigma n (ty, tys)
  | 3 -> try_get_lts_transition sigma n (ty, tys)
  | _ -> None
;;

let try_get_weak_sim gl x : EConstr.t option =
  Log.debug "mebi_bisim.try_get_weak_sim";
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let ty, tys = get_atomic_type sigma x in
  if Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_weak_sim ())
  then Some x
  else None
;;

let try_get_m_state_weak_sim gl (m : Fsm.t) x : State.t option =
  Log.debug "mebi_bisim.try_get_m_state_weak_sim";
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let ty, tys = get_atomic_type sigma x in
  if Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_weak_sim ())
  then Some (find_state_of_enc (econstr_to_enc tys.(5)) m.states)
  else None
;;

let try_get_exists gl m n x : (Transition_opt.t * State.t) option =
  Log.debug "mebi_bisim.try_get_exists";
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let _ty, tys = get_atomic_type sigma x in
  Log.debug
    (Printf.sprintf
       "mebi_bisim.try_get_exists, x ty: %s"
       (Rocq_utils.Strfy.econstr (Proofview.Goal.env gl) sigma _ty));
  let _binder, _tys, constr = get_lambda sigma tys.(1) in
  let _ty, tys = get_app sigma constr in
  Log.debug
    (Printf.sprintf
       "mebi_bisim.try_get_exists, constr ty: %s"
       (Rocq_utils.Strfy.econstr (Proofview.Goal.env gl) sigma _ty));
  match Array.to_list tys with
  | [ wk_trans; wk_sim ] ->
    let nt = try_get_weak_transition sigma n (get_atomic_type sigma wk_trans) in
    let ms = try_get_m_state_weak_sim gl m wk_sim in
    (match nt, ms with
     | Some (Weak_Transition, nt), Some ms -> Some (nt, ms)
     | _, _ -> None)
  | _ -> None
;;

type concl_result =
  | New_Weak_Sim of EConstr.t
  | Exists of (Transition_opt.t * State.t)
  | Transition of (concl_transition * Transition_opt.t)
(* | Weak_Transition of transition *)
(* | LTS_Transition of transition *)

let concl_result_string : concl_result -> string = function
  | New_Weak_Sim _ -> "New_Weak_Sim"
  | Exists _ -> "Exists"
  | Transition _ -> "Transition"
;;

let handle_concl
      (gl : Proofview.Goal.t)
      ({ the_fsm_1 = m; the_fsm_2 = n; _ } : Algorithms.Bisimilar.result)
  : concl_result
  =
  Log.debug "mebi_bisim.handle_concl";
  let the_concl : EConstr.t = Proofview.Goal.concl gl in
  match try_get_concl_transition gl n the_concl with
  | Some t -> Transition t
  | None ->
    (match try_get_weak_sim gl the_concl with
     | Some x -> New_Weak_Sim x
     | None ->
       (match try_get_exists gl m n the_concl with
        | Some (n_transition, m_state) -> Exists (n_transition, m_state)
        | None -> raise (UnhandledConcl the_concl)))
;;

type hyp_transition =
  | Full
  | Layer

type hyp_kind =
  | Cofix of hyp_cofix
  | H_Inversion of (hyp_transition * unit Proofview.tactic)
  (* | H_Transition of (hyp_transition * transition) *)
  | H_Transition of Transition_opt.t
  | Pass

exception ExpectedOnlyOne_H_ToBeInverted of Rocq_utils.hyp list

let handle_hyp
      ({ the_fsm_1 = m; the_fsm_2 = n; _ } : Algorithms.Bisimilar.result)
      env
      sigma
      (h : Rocq_utils.hyp)
  : hyp_kind
  =
  Log.debug "mebi_bisim.handle_hyp";
  let h_ty : EConstr.t = Context.Named.Declaration.get_type h in
  let ty, tys = get_atomic_type sigma h_ty in
  if Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_weak_sim ())
  then Cofix (get_cofix m n env sigma tys)
  else if Array.length tys < 3
  then Pass
  else if Mebi_theories.is_var sigma tys.(2)
  then H_Inversion (Full, Mebi_tactics.do_inversion h)
  else if Mebi_theories.is_var sigma tys.(1)
  then H_Inversion (Layer, Mebi_tactics.do_inversion h)
  else (
    match get_hyp_transition m env sigma tys with
    | None ->
      Log.debug "mebi_bisim.handle_hyp, H_Transition -- Pass";
      Pass
    | Some t ->
      Log.debug "mebi_bisim.handle_hyp, H_Transition t";
      H_Transition t
      (* ((match t.dest with None -> To_Invert | Some _ -> Full), t) *))
;;

type hyp_result =
  | Do_Inversion of unit Proofview.tactic
  | H_Transition of Transition_opt.t
  | Cofixes of hyp_cofix list
  | Empty

let hyp_result_string : hyp_result -> string = function
  | Do_Inversion _ -> "Do_Inversion"
  | H_Transition _ -> "H_Transition"
  | Cofixes _ -> "Cofixes"
  | Empty -> "Empty"
;;

let warning_multiple_h_transitions_to_invert h tl =
  Log.warning
    (Printf.sprintf
       "mebi_bisim.handle_hyps, multiple H transitions returned? (only \
        returning head)\n\
        %s"
       "TODO"
       (* (Utils.Strfy.list
          (* (fun ((t, x) : hyp_transition * transition) -> *)
          (fun (x : transition) ->
          ccccc
          Utils.Strfy.list
          Utils.Strfy.string
          [ (* Utils.Strfy.tuple ~is_keyval:true ~indent:2 Utils.Strfy.str Utils.Strfy.str ("kind", match t with Full -> "Full" | Layer -> "Layer") ; *) Utils.Strfy.tuple Utils.Strfy.string State.to_string ("from", x.from) ; Utils.Strfy.tuple Utils.Strfy.string Action.to_string ("action", x.action) ; Utils.Strfy.tuple Utils.Strfy.string (Utils.Strfy.option State.to_string) ("dest", x.dest) ])
          (h :: tl)) *))
;;

let handle_the_hyps r env sigma hyps
  : (unit Proofview.tactic * bool) option
    * hyp_cofix list
    * Transition_opt.t list
  =
  Log.debug "mebi_bisim.handle_the_hyps";
  List.fold_left
    (fun (inversion, cofixes, hs) h ->
      Log.debug
        (Printf.sprintf
           "mebi_bisim.handle_the_hyps, _ %i %i"
           (List.length cofixes)
           (List.length hs));
      match handle_hyp r env sigma h with
      | Cofix x -> inversion, x :: cofixes, hs
      | H_Inversion (Layer, p) ->
        (match inversion with
         | None -> Some (p, false), cofixes, hs
         | Some (p', false) -> Some (p, false), cofixes, hs
         | Some (p', true) -> Some (p', true), cofixes, hs)
      | H_Inversion (Full, p) ->
        (match inversion with
         | None -> Some (p, true), cofixes, hs
         | Some (p', false) -> Some (p, true), cofixes, hs
         | Some (p', true) ->
           Log.warning
             "mebi_bisim.handle_the_hyps, handle_hyp returned H_Inversion when \
              one already found. (replacing)";
           Some (p, true), cofixes, hs)
      | H_Transition t -> inversion, cofixes, t :: hs
      | Pass -> inversion, cofixes, hs)
    (None, [], [])
    hyps
;;

let handle_hyps (gl : Proofview.Goal.t) (r : Algorithms.Bisimilar.result)
  : hyp_result
  =
  Log.debug "mebi_bisim.handle_hyps";
  let env : Environ.env = Proofview.Goal.env gl in
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let hyps : Rocq_utils.hyp list = Proofview.Goal.hyps gl in
  match handle_the_hyps r env sigma hyps with
  | Some (inv, _), _, _ -> Do_Inversion inv
  | None, _, h :: [] -> H_Transition h
  | None, _, h :: tl ->
    warning_multiple_h_transitions_to_invert h tl;
    H_Transition h
  | None, [], [] -> Empty
  | None, cofixes, [] -> Cofixes cofixes
;;

(* let temp_evar_econstr () : EConstr.t = *)

let get_all_cofix_names (gl : Proofview.Goal.t) : Names.Id.Set.t =
  Names.Id.Set.filter
    (fun (x : Names.Id.t) -> Mebi_theories.is_cofix x)
    (Context.Named.to_vars (Proofview.Goal.hyps gl))
;;

let find_cofix_opt (gl : Proofview.Goal.t) (x : EConstr.t)
  : (Names.Id.t * EConstr.t) option
  =
  let eq = Mebi_setup.Eq.econstr (Proofview.Goal.sigma gl) x in
  match
    List.find_opt
      (fun v -> eq (Context.Named.Declaration.get_type v))
      (Proofview.Goal.hyps gl)
  with
  | None -> None
  | Some h ->
    let name = Context.Named.Declaration.get_id h in
    let h_ty : EConstr.t = Context.Named.Declaration.get_type h in
    (* let ty, tys = get_atomic_type (Proofview.Goal.sigma gl) h_ty in *)
    (* let v : EConstr.t =  in  *)
    Some (name, h_ty)
;;

let get_all_non_cofix (gl : Proofview.Goal.t) : Names.Id.Set.t =
  Names.Id.Set.filter
    (fun (x : Names.Id.t) -> Bool.not (Mebi_theories.is_cofix x))
    (Context.Named.to_vars (Proofview.Goal.hyps gl))
;;

let clear_old_hyps (gl : Proofview.Goal.t) : unit Proofview.tactic =
  Tactics.clear (Names.Id.Set.to_list (get_all_non_cofix gl))
;;

let do_new_cofix (gl : Proofview.Goal.t) : unit Proofview.tactic =
  Log.notice "clear H; cofix Cofix0; apply In_sim, Pack_sim; intros.";
  Mebi_theories.tactics
    [ Mebi_tactics.cofix gl
    ; Mebi_tactics.apply ~gl (Mebi_theories.c_In_sim ())
    ; Mebi_tactics.apply ~gl (Mebi_theories.c_Pack_sim ())
    ; Mebi_tactics.intros_all ()
    ; clear_old_hyps gl
    ]
;;

exception CannotUnpackTransitionsOfMN of unit

let get_bisim_states_of (m : State.t) (n : State.t option) : States.t =
  Log.debug "mebi_bisim.get_bisim_states_of";
  let bisims =
    Partition.filter (fun p -> States.mem m p) (get_bisim_states ())
  in
  assert (Int.equal 1 (Partition.cardinal bisims));
  let states : States.t = List.hd (Partition.to_list bisims) in
  (match n with None -> () | Some n -> assert (States.mem n states));
  states
;;

let get_n_candidate_actions m2 (n : Fsm.t) n_action n1 dest_states
  : States.t Actions.t
  =
  Log.debug "mebi_bisim.get_n_candidate_actions";
  let n_actions_all = Edges.find n.edges n1 in
  let n_actions = Actions.create 0 in
  Actions.iter
    (fun action dests ->
      let bisim_dests =
        States.inter dest_states (Actions.find n_actions_all action)
      in
      if States.is_empty bisim_dests
      then ()
      else if
        Action.check_equal
          ~annotations:false
          ~constructor_trees:false
          n_action
          action
      then Actions.add n_actions action dests
      else ())
    n_actions_all;
  n_actions
;;

let get_n_candidate_action_list m2 (n : Fsm.t) n_action n1 dest_states
  : (Action.t * State.t) list
  =
  Log.debug "mebi_bisim.get_n_candidate_action_list";
  Actions.fold
    (fun k v acc ->
      Log.debug
        (Printf.sprintf
           "mebi_bisim.get_n_candidate_action_list (%i)\nk:\n%s"
           (List.length acc)
           (Action.to_string k));
      States.fold
        (fun d acc0 ->
          Log.debug
            (Printf.sprintf
               "mebi_bisim.get_n_candidate_action_list (%i) (%i)\n"
               (List.length acc)
               (List.length acc0));
          (k, d) :: acc0)
        v
        acc)
    (get_n_candidate_actions m2 n n_action n1 dest_states)
    []
;;

let warning_multiple_n_candidates candidates =
  if Bool.not (Int.equal 1 (List.length candidates))
  then
    Log.warning
      (Printf.sprintf
         "mebi_bisim.get_n_candidate_action, multiple candidates found \
          (returning hd):\n\
          %s"
         (Utils.Strfy.list
            (Utils.Strfy.tuple Action.to_string State.to_string)
            candidates))
;;

let get_n_candidate_action m2 (n : Fsm.t) n_action n1 dests : Action.t * State.t
  =
  Log.debug "mebi_bisim.get_n_candidate_action";
  let candidates = get_n_candidate_action_list m2 n n_action n1 dests in
  warning_multiple_n_candidates candidates;
  let the_candidate = List.hd candidates in
  the_candidate
;;

let get_n_candidate
      (m2 : State.t)
      (n : Fsm.t)
      (n_action : Action.t)
      (n1 : State.t)
      (dests : States.t)
  : State.t
  =
  Log.debug "mebi_bisim.get_n_candidate";
  snd (get_n_candidate_action m2 n n_action n1 dests)
;;

let handle_eexists
      (gl : Proofview.Goal.t)
      (mt : Transition_opt.t)
      (nt : Transition_opt.t)
      (cm2 : State.t)
  : unit Proofview.tactic
  =
  Log.debug "mebi_bisim.handle_eexists";
  match mt.goto, nt.goto with
  | Some mgoto, None ->
    assert (State.equal mgoto cm2);
    assert (Label.equal mt.label nt.label);
    let dests : States.t = get_bisim_states_of mgoto nt.goto in
    let naction : Action.t = transition_opt_to_action nt in
    let n_candidate : State.t =
      get_n_candidate mgoto (get_n ()) naction nt.from dests
    in
    let n2 : EConstr.t = enc_to_econstr n_candidate.enc in
    Mebi_theories.tactics
      [ Tactics.constructor_tac true None 1 (Tactypes.ImplicitBindings [ n2 ])
      ; Tactics.split Tactypes.NoBindings
      ]
  | _, _ -> raise (CannotUnpackTransitionsOfMN ())
;;

let handle_weak_silent_transition
      (gl : Proofview.Goal.t)
      (from : State.t)
      (goto : State.t)
  : unit Proofview.tactic
  =
  Log.debug "mebi_bisim.handle_weak_silent_transition";
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

let warning_multiple_n_dests n_dests =
  if Bool.not (Int.equal 1 (States.cardinal n_dests))
  then
    Log.warning
      (Printf.sprintf
         "mebi_bisim.warning_multiple_n_dests, multiple candidate destss found \
          (returning hd):\n\
          %s"
         (Model.states_to_string n_dests))
;;

let debug_constrs gl =
  let cl = Tacmach.pf_concl gl in
  let env = Proofview.Goal.env gl in
  let (ind, _), redcl =
    Tacmach.pf_apply Tacred.reduce_to_quantified_ind gl cl
  in
  let consnames = (snd (Inductive.lookup_mind_specif env ind)).mind_consnames in
  let nconstr = Array.length consnames in
  Log.warning
    (Printf.sprintf
       "mebi_bisim.debug_constrs, num constrs: (%i)\n%s"
       nconstr
       (Array.fold_left
          (fun acc s -> Printf.sprintf "%s, %s" acc (Names.Id.to_string s))
          ""
          consnames))
;;

let get_from_state_of_relation gl states (rel : EConstr.t) : State.t =
  let sigma = Proofview.Goal.sigma gl in
  let ty, tys = get_atomic_type sigma rel in
  assert (Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_clos_refl_trans_1n ()));
  Log.debug
    (Printf.sprintf
       "mebi_bisim.get_from_state_of_relation, %i"
       (Array.length tys));
  find_state_of_enc (econstr_to_enc tys.(1)) states
;;

let rec build_tactics_from_constr_tree gl
  : Mebi_constr.Tree.t -> (unit -> unit Proofview.tactic) list
  = function
  | Node ((enc, index), []) ->
    (* NOTE: constructors index from 1, not 0 *)
    let index = index + 1 in
    [ (fun () ->
        Log.notice (Printf.sprintf "constructor %i." index);
        Tactics.one_constructor index Tactypes.NoBindings)
      (* ; Mebi_tactics.simplify_and_subst_all ~gl () *)
    ]
  | Node ((enc, index), tree) ->
    (* NOTE: constructors index from 1, not 0 *)
    let index = index + 1 in
    (fun () ->
      Log.notice (Printf.sprintf "constructor %i." index);
      Tactics.one_constructor index Tactypes.NoBindings)
    :: build_tactics_from_constr_tree gl (List.hd tree)
;;

let build_constructors (gl : Proofview.Goal.t)
  : Note.annotations -> unit Proofview.tactic
  = function
  | [] ->
    Log.debug "mebi_bisim.build_constructors, rt1n_refl";
    Log.notice "apply rt1n_refl.";
    Mebi_tactics.apply ~gl (Mebi_theories.c_rt1n_refl ())
  | h :: tl ->
    (match h with
     | [] -> ()
     | { from; via } :: hl ->
       Log.debug
         (Printf.sprintf
            "mebi_bisim.build_constructors, anno: %s"
            (Note.to_string { from; via }));
       (* TODO: this is the problem, we are just taking the head rather than looking at what one to take *)
       let constrs = build_tactics_from_constr_tree gl (List.hd action.meta) in
       the_proof_state := Constructors (hl :: tl, Some constrs);
       Mebi_tactics.eapply
         ~gl
         (if Label.is_silent via
          then (
            Log.notice "eapply rt1n_trans.";
            Mebi_theories.c_rt1n_trans ())
          else (
            Log.notice "eapply rt1n_refl.";
            Mebi_theories.c_rt1n_refl ())))
;;

let handle_weak_visible_transition
      (gl : Proofview.Goal.t)
      ((m1, mA, m2) : State.t * Action.t * State.t)
      ((n1, nA, n2) : State.t * Action.t * State.t)
  : unit Proofview.tactic
  =
  Log.debug "mebi_bisim.handle_weak_visible_transition";
  let dests : States.t = get_bisim_states_of m2 (Some n2) in
  assert (States.mem n2 dests);
  let n_actions = Edges.find (get_n ()).edges n1 in
  let n_action = Model.get_action_labelled nA.label n_actions in
  let n_dests = Actions.find n_actions n_action in
  (* TODO: revisit this *)
  Log.debug
    (Printf.sprintf
       "mebi_bisim.handle_weak_visible_transition\nn_annos hd:\n%s"
       (* (Annotations.t_to_string (List.hd n_action.annotations_list)) *)
       (Note.annotations_to_string n_action.annotations));
  warning_multiple_n_dests n_dests;
  let the_dest = List.hd (States.to_list n_dests) in
  assert (States.mem the_dest dests);
  (* TODO: need to test here too *)
  the_proof_state := Constructors (List.hd n_action.annotations_list, None);
  Mebi_theories.tactics
    [ Mebi_tactics.eapply ~gl (Mebi_theories.c_wk_some ())
    ; Mebi_tactics.unfold_econstr gl (Mebi_theories.c_silent ())
    ]
;;

let handle_weak_transition
      (gl : Proofview.Goal.t)
      (mt : Transition_opt.t)
      (nt : Transition_opt.t)
  : unit Proofview.tactic
  =
  (* Log.debug
     (Printf.sprintf
     "mebi_bisim.handle_weak_transition\nmt:\n%s\n\nnt:\n%s\n"
     (pstr_transition mt)
     (pstr_transition nt)); *)
  match mt.goto, nt.goto with
  | Some mgoto, Some ngoto ->
    (match mt.label.is_silent with
     | Some true ->
       the_proof_state := NewWeakSim;
       handle_weak_silent_transition gl nt.from ngoto
     | _ -> handle_weak_visible_transition gl (mt.from, mA, m2) (nt.from, nA, n2))
  | _, _ -> raise (CannotUnpackTransitionsOfMN ())
;;

exception CouldNotHandle_NewTransition of unit

let handle_new_transition_exists
      (gl : Proofview.Goal.t)
      (mt : Transition_opt.t)
      (nt : Transition_opt.t)
      m2
  : unit Proofview.tactic
  =
  Log.debug
    (Printf.sprintf
       "mebi_bisim.handle_new_transition, Exists (n2, m2)\n- m2: %s\n\n%s"
       (State.to_string m2)
       (Transition_opt.to_string nt));
  the_proof_state := GoalTransition mt;
  Log.notice "eexists n2";
  handle_eexists gl mt nt m2
;;

let handle_new_transition gl mt : unit Proofview.tactic =
  Log.debug "mebi_bisim.handle_new_transition";
  Log.debug
    (Printf.sprintf
       "mebi_bisim.handle_new_transition, mt:\n%s"
       (Transition_opt.to_string mt));
  match handle_concl gl !(get_the_result ()) with
  | Exists (nt, m2) -> handle_new_transition_exists gl mt nt m2
  | _ -> raise (CouldNotHandle_NewTransition ())
;;

exception CouldNotHandle_GoalTransition of unit

let handle_goal_transition (gl : Proofview.Goal.t) mt : unit Proofview.tactic =
  Log.debug "mebi_bisim.handle_goal_transition";
  Log.debug
    (Printf.sprintf
       "mebi_bisim.handle_goal_transition, mt:\n%s"
       (Transition_opt.to_string mt));
  match handle_concl gl !(get_the_result ()) with
  | Transition (Weak_Transition, nt) ->
    Log.debug "mebi_bisim.handle_goal_transition, Weak_Transition";
    handle_weak_transition gl mt nt
  | Transition (Silent_Transition, nt) ->
    Log.warning
      "mebi_bisim.handle_goal_transition, Silent_Transition (does nothing)";
    Proofview.tclUNIT ()
  | Transition (Silent1_Transition, nt) ->
    Log.warning
      "mebi_bisim.handle_goal_transition, Silent1_Transition (does nothing)";
    Proofview.tclUNIT ()
  | Transition (LTS_Transition, nt) ->
    Log.warning
      "mebi_bisim.handle_goal_transition, LTS_Transition (does nothing)";
    Proofview.tclUNIT ()
  | _ -> raise (CouldNotHandle_GoalTransition ())
;;

exception CouldNotHandle_NewCofix of unit

let handle_new_cofix (gl : Proofview.Goal.t) : unit Proofview.tactic =
  Log.debug "mebi_bisim.handle_new_cofix";
  match handle_hyps gl !(get_the_result ()) with
  | Do_Inversion p ->
    Log.debug "mebi_bisim.handle_new_cofix, do inversion";
    Log.notice "inversion H.";
    p
  | H_Transition mt ->
    Log.debug
      (Printf.sprintf
         "mebi_bisim.handle_new_cofix, H_transition mt\n%s"
         (Transition_opt.to_string mt));
    the_proof_state := NewTransition mt;
    handle_new_transition gl mt
  | x ->
    Log.warning
      (Printf.sprintf
         "mebi_bisim.handle_new_cofix, ERR: %s"
         (hyp_result_string x));
    raise (CouldNotHandle_NewCofix ())
;;

exception CouldNotHandle_NewWeakSim of unit

let handle_new_weak_sim (gl : Proofview.Goal.t) : unit Proofview.tactic =
  match handle_concl gl !(get_the_result ()) with
  | New_Weak_Sim x ->
    Log.debug "mebi_bisim.handle_new_weak_sim, New_Weak_Sim";
    (match find_cofix_opt gl x with
     | Some (n, _h) ->
       Log.notice
         (Printf.sprintf "apply %s. (via trivial.)" (Names.Id.to_string n));
       Log.debug
         (Printf.sprintf
            "mebi_bisim.handle_new_weak_sim, apply:\n%s"
            (Rocq_utils.Strfy.econstr
               (Proofview.Goal.env gl)
               (Proofview.Goal.sigma gl)
               _h));
       (* Tactics.exact_check h *)
       Auto.gen_trivial ~debug:Hints.Info [] None
     | None ->
       the_proof_state := NewCofix;
       do_new_cofix gl)
  | Exists _ ->
    Log.debug "handle_new_weak_sim, got Exists, trying handle_new_cofix";
    handle_new_cofix gl
  | x ->
    Log.warning
      (Printf.sprintf
         "mebi_bisim.handle_new_weak_sim, ERR: %s"
         (concl_result_string x));
    raise (CouldNotHandle_NewWeakSim ())
;;

let do_simplify gl : unit Proofview.tactic =
  Log.notice "simpl in *.";
  Mebi_tactics.simplify_and_subst_all ~gl ()
;;

let do_rt1n_refl gl =
  Log.notice "apply rt1n_refl.";
  Mebi_theories.tactics
    [ Mebi_tactics.apply ~gl (Mebi_theories.c_rt1n_refl ()); do_simplify gl ]
;;

let handle_constuctors (gl : Proofview.Goal.t)
  :  Note.annotations * (unit -> unit Proofview.tactic) list option
  -> unit Proofview.tactic
  = function
  | [], None ->
    Log.debug "mebi_bisim.handle_constuctors, ([], None)";
    the_proof_state := NewWeakSim;
    do_rt1n_refl gl
  | [], Some [] ->
    Log.debug "mebi_bisim.handle_constuctors, ([], Some [])";
    the_proof_state := NewWeakSim;
    do_rt1n_refl gl
  | ls, Some [] ->
    Log.debug "mebi_bisim.handle_constuctors, (ls, Some [])";
    Log.debug
      (Printf.sprintf
         "mebi_bisim.handle_constuctors, ls:\n%s"
         (Note.annotations_to_string ls));
    Mebi_theories.tactics [ do_simplify gl; build_constructors gl ls ]
  | ls, None ->
    Log.debug "mebi_bisim.handle_constuctors, (ls, None)";
    build_constructors gl ls
  | ls, Some (h :: tl) ->
    Log.debug "mebi_bisim.handle_constuctors, (ls, Some h::t)";
    (* build_constructors gl ls *)
    the_proof_state := Constructors (ls, Some tl);
    h ()
;;

exception CouldNotHandle_NewProof of unit

let handle_new_proof gl : unit Proofview.tactic =
  Log.debug "mebi_bisim.handle_new_proof";
  match handle_hyps gl !(get_the_result ()) with
  | Empty ->
    Log.debug "mebi_bisim.handle_new_proof, Empty hyps (start of proof)";
    the_proof_state := NewWeakSim;
    handle_new_weak_sim gl
  | x ->
    Log.warning
      (Printf.sprintf
         "mebi_bisim.handle_new_proof, ERR: %s"
         (hyp_result_string x));
    raise (CouldNotHandle_NewProof ())
;;

let handle_proof_state () : unit Proofview.tactic =
  Log.debug (proof_state_to_string !the_proof_state);
  Proofview.Goal.enter (fun gl ->
    match !the_proof_state with
    | NewProof -> handle_new_proof gl
    | NewWeakSim -> handle_new_weak_sim gl
    | NewCofix -> handle_new_cofix gl
    | NewTransition mt -> handle_new_transition gl mt
    | GoalTransition mt -> handle_goal_transition gl mt
    | Constructors (annos, tacs) -> handle_constuctors gl (annos, tacs))
;;

let loop_iter () : unit Proofview.tactic =
  Log.debug "mebi_bisim.loop_iter";
  Mebi_theories.tactics
    [ handle_proof_state (); Mebi_tactics.simplify_and_subst_all () ]
;;

(** ...
    @param u
      is the upper-bound, i.e., the maximum number of iterations to try solve the proof via [loop_iter].
*)
let solve (u : int) (pstate : Declare.Proof.t) : Declare.Proof.t =
  Log.notice (Printf.sprintf "Try to solve (%i) iterations." u);
  let rec iter_body (n : int) (pstate : Declare.Proof.t) : int * Declare.Proof.t
    =
    if Proof.is_done (Declare.Proof.get pstate)
    then (
      Log.notice (Printf.sprintf "Solved in (%i) iterations." (u - n));
      n, pstate)
    else (
      match Int.compare n 0 with
      | -1 ->
        Log.warning (Printf.sprintf "Could not Solve in (%i) iterations." u);
        0, pstate
      | _ ->
        Log.notice (Printf.sprintf "Solve, iteration: (%i)" (u - n));
        let pstate : Declare.Proof.t =
          Mebi_tactics.update_proof_by_tactic pstate (loop_iter ())
        in
        iter_body (n - 1) pstate)
  in
  let rem, pstate = iter_body u pstate in
  Log.notice (Printf.sprintf "Stopped after (%i) iterations." (u - rem));
  pstate
;;
