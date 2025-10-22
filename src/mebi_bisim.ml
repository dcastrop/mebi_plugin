open Logging
open Mebi_wrapper
open Model

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

type hyp_cofix =
  { m : State.t
  ; n : State.t
  }

type transition =
  { from : State.t
  ; action : Action.t
  ; dest : State.t option
  }

type proof_state =
  | ST_NewProof
  | ST_WeakSim
  | ST_Begin_Inversion
  | ST_Inversion of (Mebi_theories.hyp option * Mebi_theories.hyp)
  | ST_Hmt of transition
  (* | NewCofix *)
  | GoalTransition of transition
  | Constructors of
      (Model.Action.annotation * (unit -> unit Proofview.tactic) list option)

let the_proof_state : proof_state ref = ref ST_NewProof
let reset_the_proof_state () : unit = the_proof_state := ST_NewProof

let proof_state_str () : string =
  match !the_proof_state with
  | ST_NewProof -> "ST_NewProof"
  | ST_WeakSim -> "ST_WeakSim"
  | ST_Begin_Inversion -> "ST_Begin_Inversion"
  | ST_Inversion _ -> "ST_Inversion"
  (* | NewCofix -> "NewCofix" *)
  | ST_Hmt _ -> "ST_Hmt"
  | GoalTransition _ -> "GoalTransition"
  | Constructors _ -> "Constructors"
;;

let is_concl_weak_sim sigma concl_ty : bool =
  Mebi_setup.Eq.econstr sigma concl_ty (Mebi_theories.c_weak_sim ())
;;

let is_concl_weak_transition sigma concl_ty : bool =
  Mebi_setup.Eq.econstr sigma concl_ty (Mebi_theories.c_weak ())
;;

(* exception UnableToDetermineProofState of unit

   let detect_proof_state gl : proof_state =
   (* let env = Proofview.Goal.env gl in *)
   let sigma = Proofview.Goal.sigma gl in
   let concl = Proofview.Goal.concl gl in
   let concl_ty, concl_tys = get_atomic_type sigma concl in
   if is_concl_weak_sim sigma concl_ty
   then ST_WeakSim
   else if is_concl_weak_transition sigma concl_ty
   then NewCofix
   else if is_concl_weak_transition sigma concl_ty
   then NewCofix
   else raise (UnableToDetermineProofState ())
   ;; *)

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

let get_m () : Fsm.t = !(get_the_result ()).the_fsm_1
let get_n () : Fsm.t = !(get_the_result ()).the_fsm_2
let get_bisim_states () : Partition.t = !(get_the_result ()).bisim_states

let get_non_bisim_states () : Partition.t =
  !(get_the_result ()).non_bisim_states
;;

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

let pstr_transition (x : transition) : string =
  Printf.sprintf
    "- from:%s\n- action:%s\n- dest:%s\n"
    (Model.pstr_state x.from)
    (Model.pstr_action x.action)
    (Strfy.option Model.pstr_state x.dest)
;;

exception Enc_Of_EConstr_NotFound of (EConstr.t * States.t)
exception State_Of_Enc_NotFound of (Enc.t * States.t)
exception Error_Multiple_States_Of_Enc_Found of (Enc.t * States.t)

let find_state_of_enc (x : Enc.t) (s : States.t) : State.t =
  match
    List.filter
      (fun ((y, _) : State.t) -> Mebi_setup.Eq.enc x y)
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
      (fun ((y, _) : State.t) -> Mebi_setup.Eq.enc x y)
      (States.to_list s)
  with
  | h :: [] -> Some h
  | [] -> None
  | _ :: _ -> raise (Error_Multiple_States_Of_Enc_Found (x, s))
;;

exception Label_Of_Enc_NotFound of (Enc.t * Alphabet.t)
exception Error_Multiple_Labels_Of_Enc_Found of (Enc.t * Alphabet.t)

let find_label_of_enc (x : Enc.t) (s : Alphabet.t) : Action.Label.t =
  match
    List.filter
      (fun ((y, _) : Action.Label.t) -> Mebi_setup.Eq.enc x y)
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
  : transition
  =
  Log.debug "mebi_bisim.get_concl_transition";
  let from = find_state_of_enc (econstr_to_enc from_term) m.states in
  let action =
    get_action_with_label
      (get_actions_from from m.edges)
      (find_label_of_enc (econstr_to_enc action_term) m.alphabet)
  in
  match econstr_to_enc_opt sigma dest_term with
  | None ->
    if abort_on_failed_dest_enc
    then raise (Enc_Of_EConstr_NotFound (dest_term, m.states))
    else { from; action; dest = None }
  | Some dest_enc ->
    { from; action; dest = find_state_of_enc_opt dest_enc m.states }
;;

let get_weak_transition m sigma tys : transition =
  Log.debug
    (Printf.sprintf "mebi_bisim.get_weak_transition, %i" (Array.length tys));
  get_concl_transition m sigma (tys.(3), tys.(5), tys.(4))
;;

let get_lts_transition m sigma tys : transition =
  Log.debug
    (Printf.sprintf "mebi_bisim.get_lts_transition, %i" (Array.length tys));
  get_concl_transition
    ~abort_on_failed_dest_enc:true
    m
    sigma
    (tys.(0), tys.(1), tys.(2))
;;

let get_hyp_transition (m : Fsm.t) sigma (tys : EConstr.t array)
  : transition option
  =
  Log.debug
    (Printf.sprintf "mebi_bisim.get_hyp_transition, %i" (Array.length tys));
  match econstr_to_enc_opt sigma tys.(0) with
  | None -> None
  | Some from_enc ->
    (match find_state_of_enc_opt from_enc m.states with
     | None -> None
     | Some from ->
       (match econstr_to_enc_opt sigma tys.(1) with
        | None -> raise (Enc_Of_EConstr_NotFound (tys.(2), m.states))
        | Some action_enc ->
          (match econstr_to_enc_opt sigma tys.(2) with
           | None -> None
           | Some dest_enc ->
             Some
               { from
               ; action =
                   get_action_with_label
                     (get_actions_from from m.edges)
                     (find_label_of_enc action_enc m.alphabet)
               ; dest = find_state_of_enc_opt dest_enc m.states
               })))
;;

let get_cofix (m : Fsm.t) (n : Fsm.t) env sigma (tys : EConstr.t array)
  : hyp_cofix
  =
  Log.debug (Printf.sprintf "mebi_bisim.get_cofix, %i" (Array.length tys));
  { m = find_state_of_enc (econstr_to_enc tys.(5)) m.states
  ; n = find_state_of_enc (econstr_to_enc tys.(6)) n.states
  }
;;

exception UnhandledConcl of EConstr.t

type concl_transition =
  | Weak_Transition
  | Silent_Transition
  | Silent1_Transition
  | LTS_Transition

let try_get_weak_transition sigma n (ty, tys)
  : (concl_transition * transition) option
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
  : (concl_transition * transition) option
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
  : (concl_transition * transition) option
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

let try_get_concl_transition gl n x : (concl_transition * transition) option =
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

let try_get_exists gl m n x : (transition * State.t) option =
  Log.debug "mebi_bisim.try_get_exists";
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let _ty, tys = get_atomic_type sigma x in
  Log.debug
    (Printf.sprintf
       "mebi_bisim.try_get_exists, x ty: %s"
       (Strfy.econstr (Proofview.Goal.env gl) sigma _ty));
  let _binder, _tys, constr = get_lambda sigma tys.(1) in
  let _ty, tys = get_app sigma constr in
  Log.debug
    (Printf.sprintf
       "mebi_bisim.try_get_exists, constr ty: %s"
       (Strfy.econstr (Proofview.Goal.env gl) sigma _ty));
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
  | Exists of (transition * State.t)
  | Transition of (concl_transition * transition)
(* | Weak_Transition of transition *)
(* | LTS_Transition of transition *)

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
  | H_Inversion of (hyp_transition * Mebi_theories.hyp)
  (* | H_Transition of (hyp_transition * transition) *)
  | H_Transition of transition
  | Pass

exception ExpectedOnlyOne_H_ToBeInverted of Mebi_theories.hyp list

let handle_hyp
      ({ the_fsm_1 = m; the_fsm_2 = n; _ } : Algorithms.Bisimilar.result)
      env
      sigma
      (h : Mebi_theories.hyp)
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
  then H_Inversion (Full, h)
  else if Mebi_theories.is_var sigma tys.(1)
  then H_Inversion (Layer, h)
  else (
    Log.debug "mebi_bisim.handle_hyp, H_Transition";
    match get_hyp_transition m sigma tys with
    | None -> Pass
    | Some t -> H_Transition t
    (* ((match t.dest with None -> To_Invert | Some _ -> Full), t) *))
;;

type hyp_result =
  | Do_Inversion of Mebi_theories.hyp
  | H_Transition of transition
  | Cofixes of hyp_cofix list
  | Empty

let warning_multiple_h_transitions_to_invert h tl =
  Log.warning
    (Printf.sprintf
       "mebi_bisim.handle_hyps, multiple H transitions returned? (only \
        returning head)\n\
        %s"
       (Strfy.list
          ~force_newline:true
          (* (fun ((t, x) : hyp_transition * transition) -> *)
          (fun (x : transition) ->
            Strfy.list
              ~force_newline:true
              ~indent:1
              Strfy.str
              [ (* Strfy.tuple
                   ~is_keyval:true
                   ~indent:2
                   Strfy.str
                   Strfy.str
                   ("kind", match t with Full -> "Full" | Layer -> "Layer")
                   ; *)
                Strfy.tuple
                  ~is_keyval:true
                  ~indent:2
                  Strfy.str
                  Model.State.pstr
                  ("from", x.from)
              ; Strfy.tuple
                  ~is_keyval:true
                  ~indent:2
                  Strfy.str
                  Model.Action.pstr
                  ("action", x.action)
              ; Strfy.tuple
                  ~is_keyval:true
                  ~indent:2
                  Strfy.str
                  (Strfy.option Model.State.pstr)
                  ("dest", x.dest)
              ])
          (h :: tl)))
;;

let handle_the_hyps r env sigma hyps
  : (Mebi_theories.hyp * bool) option * hyp_cofix list * transition list
  =
  Log.debug "mebi_bisim.handle_the_hyps";
  List.fold_left
    (fun (inversion, cofixes, hs) h ->
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
  let hyps : Mebi_theories.hyp list = Proofview.Goal.hyps gl in
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
      else if Action.eq ~annos:false ~meta:false n_action action
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
           (Model.pstr_action k));
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
         (Strfy.list
            ~force_newline:true
            (Strfy.tuple Model.pstr_action Model.pstr_state)
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

let get_n_candidate m2 (n : Fsm.t) n_action n1 dests : State.t =
  Log.debug "mebi_bisim.get_n_candidate";
  snd (get_n_candidate_action m2 n n_action n1 dests)
;;

let handle_eexists
      (gl : Proofview.Goal.t)
      ({ from = m1; action = mA; dest = m2 } : transition)
      ({ from = n1; action = nA; dest = n2 } : transition)
      (cm2 : State.t)
  : unit Proofview.tactic
  =
  Log.debug "mebi_bisim.handle_eexists";
  match m2, n2 with
  | Some m2, None ->
    assert (State.eq m2 cm2);
    assert (Action.eq ~annos:false ~meta:false mA nA);
    let dests : States.t = get_bisim_states_of m2 n2 in
    let n_candidate : State.t = get_n_candidate m2 (get_n ()) nA n1 dests in
    let n2 : EConstr.t = enc_to_econstr (fst n_candidate) in
    Mebi_theories.tactics
      [ Tactics.constructor_tac true None 1 (Tactypes.ImplicitBindings [ n2 ])
      ; Tactics.split Tactypes.NoBindings
      ]
  | _, _ -> raise (CannotUnpackTransitionsOfMN ())
;;

let handle_weak_silent_transition gl n1 n2 : unit Proofview.tactic =
  Log.debug "mebi_bisim.handle_weak_silent_transition";
  Mebi_theories.tactics
    [ Mebi_tactics.apply ~gl (Mebi_theories.c_wk_none ())
    ; Mebi_tactics.unfold_econstr gl (Mebi_theories.c_silent ())
    ; (if State.eq n1 n2
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
         (Model.pstr_states n_dests))
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
  : Mebi_constr_tree.t -> (unit -> unit Proofview.tactic) list
  = function
  | Node ((enc, index), []) ->
    (* Log.debug
       (Printf.sprintf "mebi_bisim.build_tactics_from_constr_tree, last (%i)" index); *)
    [ (fun () ->
        Log.notice (Printf.sprintf "constructor %i." index);
        Tactics.one_constructor index Tactypes.NoBindings)
      (* ; Mebi_tactics.simplify_and_subst_all ~gl () *)
    ]
  | Node ((enc, index), tree) ->
    (* Log.debug
       (Printf.sprintf "mebi_bisim.build_tactics_from_constr_tree, list (%i)" index); *)
    (fun () ->
      Log.notice (Printf.sprintf "constructor %i." index);
      Tactics.one_constructor index Tactypes.NoBindings)
    :: build_tactics_from_constr_tree gl (List.hd tree)
;;

let build_constructors gl : Model.Action.annotation -> unit Proofview.tactic
  = function
  | [] ->
    Log.debug "mebi_bisim.build_constructors, rt1n_refl";
    Log.notice "apply rt1n_refl.";
    Mebi_tactics.apply ~gl (Mebi_theories.c_rt1n_refl ())
    (* | (_tgt, action) :: ((_tgt', action')::t) ->
       (   match Action.is_silent action, Action.is_silent action with
       | true, true ->

       ;
       let pre_meta, post_meta = (List.hd action.meta) in
       let pre_constrs = build_tactics_from_constr_tree gl (pre_meta) in
       the_proof_state := Constructors (t, Some (pre_constrs, post_constrs));
       Mebi_tactics.eapply ~gl (Mebi_theories.c_rt1n_trans ())) *)
  | (_tgt, action) :: t ->
    (* the_proof_state := BuildConstructors (mt, nt, t); *)
    Log.debug
      (Printf.sprintf
         "mebi_bisim.build_constructors, anno: %s"
         (Model.Action.annotation_pair_to_string (_tgt, action)));
    (* let pre_meta, post_meta = (List.hd action.meta) in *)
    (* let pre_constrs = build_tactics_from_constr_tree gl (pre_meta) in *)
    (* the_proof_state := Constructors (t, Some (pre_constrs, post_constrs)); *)
    let constrs = build_tactics_from_constr_tree gl (List.hd action.meta) in
    the_proof_state := Constructors (t, Some constrs);
    Mebi_tactics.eapply
      ~gl
      (if Action.is_silent action
       then (
         Log.notice "eapply rt1n_trans.";
         Mebi_theories.c_rt1n_trans ())
       else (
         Log.notice "eapply rt1n_refl.";
         Mebi_theories.c_rt1n_refl ()))
;;

(*  :: build_constructors gl (* mt nt *) t *)

(* let do_constr = apply_lts_constructor gl (List.hd action.meta) in *)
(* let next_constrs = handle_weak_constructors gl mt nt t in *)
(* Proofview.tclTHEN do_constr next_constrs *)
(* do_constr *)

(* do_constr *)

let handle_weak_visible_transition
      (gl : Proofview.Goal.t)
      ((m1, mA, m2) : State.t * Action.t * State.t)
      ((n1, nA, n2) : State.t * Action.t * State.t)
  : unit Proofview.tactic
  =
  Log.debug "mebi_bisim.handle_weak_visible_transition";
  (* Log.debug
     (Printf.sprintf
     "mebi_bisim.handle_weak_visible_transition\nm2:\n%s"
     (Model.pstr_state m2)); *)
  let dests : States.t = get_bisim_states_of m2 (Some n2) in
  assert (States.mem n2 dests);
  let n_actions = Edges.find (get_n ()).edges n1 in
  (* Log.debug
     (Printf.sprintf
     "mebi_bisim.handle_weak_visible_transition\nn_actions:\n%s"
     (Strfy.list
     ~force_newline:true
     (Strfy.tuple Model.pstr_action Model.pstr_states)
     (List.of_seq (Actions.to_seq n_actions)))); *)
  let n_action = Model.get_action_with_label n_actions nA.label in
  let n_dests = Actions.find n_actions n_action in
  (* Log.debug
     (Printf.sprintf
     "mebi_bisim.handle_weak_visible_transition\nn_dests:\n%s"
     (Model.pstr_states n_dests)); *)
  Log.debug
    (Printf.sprintf
       "mebi_bisim.handle_weak_visible_transition\nn_annos hd:\n%s"
       (Model.Action.annotation_to_string (List.hd n_action.annos)));
  warning_multiple_n_dests n_dests;
  let the_dest = List.hd (States.to_list n_dests) in
  assert (States.mem the_dest dests);
  (* let _do_constrs =
     handle_weak_constructors gl (get_n ()).states (List.hd n_action.annos)
     in *)
  (* the_proof_state
  := BuildConstructors ((m1, mA, m2), (n1, nA, n2), List.hd n_action.annos); *)
  the_proof_state := Constructors (List.hd n_action.annos, None);
  Mebi_theories.tactics
    [ Mebi_tactics.eapply ~gl (Mebi_theories.c_wk_some ())
    ; Mebi_tactics.unfold_econstr gl (Mebi_theories.c_silent ())
      (* TODO: save for next iteration of loop, seems to be applying to the wrong goal -> MAYBE: active some proof state now, that shortcuts to this? *)
      (* ; Mebi_tactics.eapply ~gl (Mebi_theories.c_rt1n_trans ())  *)
      (* ; do_constrs *)
    ]
;;

let handle_weak_transition
      (gl : Proofview.Goal.t)
      (mt : transition)
      (nt : transition)
  : unit Proofview.tactic
  =
  (* Log.debug
     (Printf.sprintf
     "mebi_bisim.handle_weak_transition\nmt:\n%s\n\nnt:\n%s\n"
     (pstr_transition mt)
     (pstr_transition nt)); *)
  let { from = m1; action = mA; dest = m2 } = mt in
  let { from = n1; action = nA; dest = n2 } = nt in
  match m2, n2 with
  | Some m2, Some n2 ->
    (match Action.Label.is_silent mA.label with
     | Some true ->
       the_proof_state := ST_WeakSim;
       handle_weak_silent_transition gl n1 n2
     | _ -> handle_weak_visible_transition gl (m1, mA, m2) (n1, nA, n2))
  | _, _ -> raise (CannotUnpackTransitionsOfMN ())
;;

exception CouldNotHandle_ST_Hmt of unit

let handle_new_transition gl mt : unit Proofview.tactic =
  match handle_concl gl !(get_the_result ()) with
  | Exists (nt, m2) ->
    Log.debug
      (Printf.sprintf
         "mebi_bisim [%s] handle_new_transition, mt:\n%s\nnt:%s"
         (proof_state_str ())
         (pstr_transition mt)
         (pstr_transition nt));
    the_proof_state := GoalTransition mt;
    Log.notice "eexists n2";
    handle_eexists gl mt nt m2
  | _ ->
    Log.debug
      (Printf.sprintf
         "mebi_bisim [%s] handle_new_transition, mt:\n%s"
         (proof_state_str ())
         (pstr_transition mt));
    raise (CouldNotHandle_ST_Hmt ())
;;

exception CouldNotHandle_GoalTransition of unit

let handle_goal_transition (gl : Proofview.Goal.t) mt : unit Proofview.tactic =
  Log.debug
    (Printf.sprintf
       "mebi_bisim [%s] handle_goal_transition, H_Transition mt:\n%s"
       (proof_state_str ())
       (pstr_transition mt));
  match handle_concl gl !(get_the_result ()) with
  | Transition (Weak_Transition, nt) ->
    Log.debug
      (Printf.sprintf
         "mebi_bisim [%s] handle_goal_transition, Weak_Transitionnt:\n%s"
         (proof_state_str ())
         (pstr_transition nt));
    handle_weak_transition gl mt nt
  | Transition (Silent_Transition, nt) ->
    Log.warning
      (Printf.sprintf
         "mebi_bisim [%s] handle_goal_transition, Silent_Transition (does \
          nothing) nt:\n\
          %s"
         (proof_state_str ())
         (pstr_transition nt));
    Proofview.tclUNIT ()
  | Transition (Silent1_Transition, nt) ->
    Log.warning
      (Printf.sprintf
         "mebi_bisim [%s] handle_goal_transition, Silent1_Transition (does \
          nothing) nt:\n\
          %s"
         (proof_state_str ())
         (pstr_transition nt));
    Proofview.tclUNIT ()
  | Transition (LTS_Transition, nt) ->
    Log.warning
      (Printf.sprintf
         "mebi_bisim [%s] handle_goal_transition, LTS_Transition (does \
          nothing) nt:\n\
          %s"
         (proof_state_str ())
         (pstr_transition nt));
    Proofview.tclUNIT ()
  | _ ->
    Log.debug
      (Printf.sprintf
         "mebi_bisim [%s] handle_goal_transition, mt:\n%s"
         (proof_state_str ())
         (pstr_transition mt));
    raise (CouldNotHandle_GoalTransition ())
;;

exception CouldNotHandle_BeginInversion of unit
exception ErrorUpdatingProofState of proof_state

let handle_begin_inversion (gl : Proofview.Goal.t) : unit Proofview.tactic =
  Log.debug
    (Printf.sprintf
       "mebi_bisim [%s] handle_begin_inversion"
       (proof_state_str ()));
  match handle_hyps gl !(get_the_result ()) with
  | Do_Inversion h ->
    Log.debug
      (Printf.sprintf
         "mebi_bisim [%s] handle_begin_inversion, Do_Inversion"
         (proof_state_str ()));
    Log.notice "inversion H.";
    (the_proof_state
     := match !the_proof_state with
        | ST_Inversion (None, _) -> ST_Inversion (Some h, h)
        | ST_Inversion (Some p, _) -> ST_Inversion (Some p, h)
        | _ -> raise (ErrorUpdatingProofState !the_proof_state));
    Mebi_tactics.do_inversion h
  | H_Transition mt ->
    Log.debug
      (Printf.sprintf
         "mebi_bisim [%s] handle_begin_inversion, H_Transition mt:\n%s"
         (proof_state_str ())
         (pstr_transition mt));
    the_proof_state := ST_Hmt mt;
    (* handle_new_transition gl mt *)
    Proofview.tclUNIT ()
  | _ ->
    Log.debug
      (Printf.sprintf
         "mebi_bisim [%s] handle_begin_inversion"
         (proof_state_str ()));
    raise (CouldNotHandle_BeginInversion ())
;;

let handle_inversion (gl : Proofview.Goal.t) : unit Proofview.tactic =
  Log.debug
    (Printf.sprintf "mebi_bisim [%s] handle_inversion" (proof_state_str ()));
  match handle_hyps gl !(get_the_result ()) with
  | Do_Inversion h ->
    Log.debug
      (Printf.sprintf
         "mebi_bisim [%s] handle_inversion, Do_Inversion"
         (proof_state_str ()));
    Log.notice "inversion H.";
    (the_proof_state
     := match !the_proof_state with
        | ST_Inversion (None, _) -> ST_Inversion (Some h, h)
        | ST_Inversion (Some p, _) -> ST_Inversion (Some p, h)
        | _ -> raise (ErrorUpdatingProofState !the_proof_state));
    Mebi_tactics.do_inversion h
  | H_Transition mt ->
    Log.debug
      (Printf.sprintf
         "mebi_bisim [%s] handle_inversion, H_Transition mt:\n%s"
         (proof_state_str ())
         (pstr_transition mt));
    the_proof_state := ST_Hmt mt;
    (* handle_new_transition gl mt *)
    Proofview.tclUNIT ()
  | _ ->
    Log.debug
      (Printf.sprintf
         "mebi_bisim [%s] handle_begin_inversion"
         (proof_state_str ()));
    raise (CouldNotHandle_BeginInversion ())
;;

(* exception CouldNotHandle_NewCofix of unit

   let handle_new_cofix (gl : Proofview.Goal.t) : unit Proofview.tactic =
   Log.debug
   (Printf.sprintf
   "mebi_bisim [%s] handle_new_cofix"
   (proof_state_str ()));
   match handle_hyps gl !(get_the_result ()) with
   | Do_Inversion h ->
   Log.debug
   (Printf.sprintf
   "mebi_bisim [%s] handle_new_cofix, Do_Inversion"
   (proof_state_str ()));
   Log.notice "inversion H.";
   (the_proof_state
   := match !the_proof_state with
   | ST_Inversion (None, _) -> ST_Inversion (Some h, h)
   | ST_Inversion (Some p, _) -> ST_Inversion (Some p, h)
   | _ -> raise (ErrorUpdatingProofState ()));
   Mebi_tactics.do_inversion h
   | H_Transition mt ->
   Log.debug
   (Printf.sprintf
   "mebi_bisim [%s] handle_new_cofix, H_Transition mt:\n%s"
   (proof_state_str ())
   (pstr_transition mt));
   the_proof_state := ST_Hmt mt;
   handle_new_transition gl mt
   | _ ->
   Log.debug
   (Printf.sprintf "mebi_bisim [%s] handle_new_cofix" (proof_state_str ()));
   raise (CouldNotHandle_NewCofix ())
   ;; *)

exception CouldNotHandle_ST_WeakSim of unit

let handle_new_weak_sim (gl : Proofview.Goal.t) : unit Proofview.tactic =
  match handle_concl gl !(get_the_result ()) with
  | New_Weak_Sim x ->
    Log.debug
      (Printf.sprintf
         "mebi_bisim [%s] handle_new_weak_sim, New_Weak_Sim"
         (proof_state_str ()));
    (match find_cofix_opt gl x with
     | Some (n, _h) ->
       Log.notice (Printf.sprintf "apply %s. (trivial)" (Names.Id.to_string n));
       Log.debug
         (Printf.sprintf
            "mebi_bisim.handle_new_weak_sim, apply:\n%s"
            (Strfy.econstr (Proofview.Goal.env gl) (Proofview.Goal.sigma gl) _h));
       Auto.gen_trivial ~debug:Hints.Info [] None
     | None ->
       the_proof_state := ST_Begin_Inversion;
       do_new_cofix gl)
  | _ ->
    Log.debug
      (Printf.sprintf
         "mebi_bisim [%s] handle_new_weak_sim"
         (proof_state_str ()));
    raise (CouldNotHandle_ST_WeakSim ())
;;

let do_simplify gl : unit Proofview.tactic =
  Log.notice "simpl in *.";
  Mebi_tactics.simplify_and_subst_all ~gl ()
;;

let handle_constuctors gl
  :  Model.Action.annotation * (unit -> unit Proofview.tactic) list option
  -> unit Proofview.tactic
  = function
  | [], None ->
    Log.debug
      (Printf.sprintf
         "mebi_bisim [%s] handle_constuctors ([], None)"
         (proof_state_str ()));
    the_proof_state := ST_WeakSim;
    (* handle_new_weak_sim gl *)
    Log.notice "apply rt1n_refl.";
    Mebi_theories.tactics
      [ Mebi_tactics.apply ~gl (Mebi_theories.c_rt1n_refl ()); do_simplify gl ]
  | [], Some [] ->
    Log.debug
      (Printf.sprintf
         "mebi_bisim [%s] handle_constuctors ([], Some [])"
         (proof_state_str ()));
    the_proof_state := ST_WeakSim;
    (* handle_new_weak_sim gl *)
    Log.notice "apply rt1n_refl.";
    Mebi_theories.tactics
      [ Mebi_tactics.apply ~gl (Mebi_theories.c_rt1n_refl ()); do_simplify gl ]
  | ls, Some [] ->
    Log.debug
      (Printf.sprintf
         "mebi_bisim [%s] handle_constuctors (ls, Some [])"
         (proof_state_str ()));
    Log.debug
      (Printf.sprintf
         "mebi_bisim.handle_constuctors, ls:\n%s"
         (Model.Action.annotation_to_string ls));
    Mebi_theories.tactics [ do_simplify gl; build_constructors gl ls ]
  | ls, None ->
    Log.debug
      (Printf.sprintf
         "mebi_bisim [%s] handle_constuctors (ls, None)"
         (proof_state_str ()));
    build_constructors gl ls
  | ls, Some (h :: t) ->
    Log.debug
      (Printf.sprintf
         "mebi_bisim [%s] handle_constuctors (ls, Some h::t)"
         (proof_state_str ()));
    (* build_constructors gl ls *)
    the_proof_state := Constructors (ls, Some t);
    h ()
;;

exception CouldNotHandle_ST_NewProof of unit

let handle_new_proof gl : unit Proofview.tactic =
  Log.debug
    (Printf.sprintf "mebi_bisim [%s] handle_new_proof" (proof_state_str ()));
  match handle_hyps gl !(get_the_result ()) with
  | Empty ->
    Log.debug "mebi_bisim.handle_new_proof, Empty hyps (start of proof)";
    the_proof_state := ST_WeakSim;
    handle_new_weak_sim gl
  | _ -> raise (CouldNotHandle_ST_NewProof ())
;;

let handle_proof_state () : unit Proofview.tactic =
  Proofview.Goal.enter (fun gl ->
    match !the_proof_state with
    | ST_NewProof -> handle_new_proof gl
    | ST_WeakSim -> handle_new_weak_sim gl
    | ST_Begin_Inversion -> handle_begin_inversion gl
    | ST_Inversion _h -> handle_begin_inversion gl
    | ST_Hmt mt -> handle_new_transition gl mt
    | GoalTransition mt -> handle_goal_transition gl mt
    (* | BuildConstructors (mt, nt, constrs) ->
      handle_build_constuctors gl mt nt constrs *)
    (* | ApplyConstructors tactics -> handle_apply_constuctors gl tactics *)
    | Constructors (anno, tacs) -> handle_constuctors gl (anno, tacs))
;;

let loop_test () : unit Proofview.tactic =
  Log.debug (Printf.sprintf "mebi_bisim [%s] loop_test" (proof_state_str ()));
  Mebi_theories.tactics
    [ handle_proof_state (); Mebi_tactics.simplify_and_subst_all () ]
;;
