open Logging
open Mebi_wrapper
open Model

let the_cached_result : Algorithms.Bisimilar.result ref option ref = ref None
let reset_the_cached_result () : unit = the_cached_result := None

exception MissingBisimResult of unit

let get_the_result () : Algorithms.Bisimilar.result ref =
  Log.trace "mebi_bisim.get_the_result";
  match !the_cached_result with
  | None -> raise (MissingBisimResult ()) (* missing_bisim_result () *)
  | Some r -> r
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

type hyp_cofix =
  { m : State.t
  ; n : State.t
  }

type transition =
  { from : State.t
  ; action : Action.t
  ; dest : State.t option
  }

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

let find_state_of_enc_opt (x : Enc.t option) (s : States.t) : State.t option =
  match x with None -> None | Some x -> Some (find_state_of_enc x s)
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

let enc_to_econstr : Enc.t -> EConstr.t =
  fun (x : Enc.t) -> run ~keep_encoding:true ~fresh:false (get_decoding x)
;;

let econstr_to_enc_opt (sigma : Evd.evar_map) : EConstr.t -> Enc.t option =
  fun (x : EConstr.t) ->
  (* if EConstr.isEvar sigma x *)
  if EConstr.isRel sigma x then None else Some (econstr_to_enc x)
;;

let get_weak_transition (m : Fsm.t) sigma (tys : EConstr.t array) : transition =
  Log.trace "mebi_bisim.get_weak_transition";
  let from = find_state_of_enc (econstr_to_enc tys.(3)) m.states in
  let action =
    get_action_with_label
      (get_actions_from from m.edges)
      (find_label_of_enc (econstr_to_enc tys.(5)) m.alphabet)
  in
  let dest =
    find_state_of_enc_opt (econstr_to_enc_opt sigma tys.(4)) m.states
  in
  { from; action; dest }
;;

let get_lts_transition (m : Fsm.t) sigma (tys : EConstr.t array) : transition =
  Log.trace "mebi_bisim.get_lts_transition";
  (* Log.debug
     (Printf.sprintf "mebi_bisim.get_lts_transition, %i" (Array.length tys)); *)
  let from = find_state_of_enc (econstr_to_enc tys.(0)) m.states in
  let action =
    get_action_with_label
      (get_actions_from from m.edges)
      (find_label_of_enc (econstr_to_enc tys.(1)) m.alphabet)
  in
  let dest =
    find_state_of_enc_opt (econstr_to_enc_opt sigma tys.(2)) m.states
  in
  { from; action; dest }
;;

let get_cofix (m : Fsm.t) (n : Fsm.t) env sigma (tys : EConstr.t array)
  : hyp_cofix
  =
  Log.trace "mebi_bisim.get_cofix";
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

let try_get_weak_transition gl n x : transition option =
  Log.debug "mebi_bisim.try_get_weak_transition";
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let ty, tys = get_atomic_type sigma x in
  if Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_weak ())
  then Some (get_weak_transition n sigma tys)
  else None
;;

let try_get_weak_sim gl x : unit option =
  Log.debug "mebi_bisim.try_get_weak_sim";
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let ty, tys = get_atomic_type sigma x in
  if Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_weak_sim ())
  then Some ()
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
    let nt = try_get_weak_transition gl n wk_trans in
    let ms = try_get_m_state_weak_sim gl m wk_sim in
    (match nt, ms with Some nt, Some ms -> Some (nt, ms) | _, _ -> None)
  | _ -> None
;;

let try_get_lts_transition gl n x : transition option =
  Log.debug "mebi_bisim.try_get_lts_transition";
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let ty, tys = get_atomic_type sigma x in
  if Mebi_setup.Eq.econstr sigma ty (Mebi_theories.c_LTS ())
  then Some (get_lts_transition n sigma tys)
  else None
;;

type concl_result =
  | New_Weak_Sim
  | Exists of (transition * State.t)
  | Weak_Transition of transition
  | LTS_Transition of transition

let handle_concl
      (gl : Proofview.Goal.t)
      ({ the_fsm_1 = m; the_fsm_2 = n; _ } : Algorithms.Bisimilar.result)
  : concl_result
  =
  Log.debug "mebi_bisim.handle_concl";
  let the_concl : EConstr.t = Proofview.Goal.concl gl in
  match try_get_weak_transition gl n the_concl with
  | Some t -> Weak_Transition t
  | None ->
    (match try_get_weak_sim gl the_concl with
     | Some () -> New_Weak_Sim
     | None ->
       (match try_get_exists gl m n the_concl with
        | Some (n_transition, m_state) -> Exists (n_transition, m_state)
        | None ->
          (match try_get_lts_transition gl n the_concl with
           | Some t -> LTS_Transition t
           | None -> raise (UnhandledConcl the_concl))))
;;

type hyp_kind =
  | Cofix of hyp_cofix
  | H_Inversion_an2 of unit Proofview.tactic
  | H_Inversion_a of unit Proofview.tactic
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
  else if Mebi_theories.is_var sigma tys.(1)
  then
    if Mebi_theories.is_var sigma tys.(2)
    then H_Inversion_an2 (Mebi_tactics.do_inversion h)
    else
      H_Inversion_a (Mebi_tactics.do_inversion h)
      (* Log.debug
         (Printf.sprintf
         "mebi_bisim.handle_hyp, H_Inversion: %s\n%s"
         (Strfy.econstr env sigma h_ty)
         (Strfy.list
         ~force_newline:true
         (Strfy.tuple
         (Strfy.tuple (Strfy.tuple Strfy.bool Strfy.bool) Strfy.bool)
         (Strfy.econstr env sigma))
         (Array.fold_left
         (fun acc e ->
         ( ( (EConstr.isRef sigma e, EConstr.isVar sigma e)
         , EConstr.isEvar sigma e )
         , e )
         :: acc)
         []
         tys))); *)
  else H_Transition (get_lts_transition m sigma tys)
;;

type hyp_result =
  | Do_Inversion of unit Proofview.tactic
  | H_Transition of transition
  | Cofixes of hyp_cofix list
  | Empty

let handle_hyps (gl : Proofview.Goal.t) (r : Algorithms.Bisimilar.result)
  : hyp_result
  =
  Log.debug "mebi_bisim.handle_hyps";
  let env : Environ.env = Proofview.Goal.env gl in
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let hyps : Mebi_theories.hyp list = Proofview.Goal.hyps gl in
  let the_hyps
    : (unit Proofview.tactic * bool) option * hyp_cofix list * transition list
    =
    List.fold_left
      (fun (inversion, cofixes, hs) h ->
        match handle_hyp r env sigma h with
        | Cofix x -> inversion, x :: cofixes, hs
        | H_Inversion_a p ->
          (match inversion with
           | None -> Some (p, false), cofixes, hs
           | Some (p', false) -> Some (p, false), cofixes, hs
           | Some (p', true) -> Some (p', true), cofixes, hs)
        | H_Inversion_an2 p ->
          (match inversion with
           | None -> Some (p, true), cofixes, hs
           | Some (p', false) -> Some (p, true), cofixes, hs
           | Some (p', true) ->
             Log.warning
               "mebi_bisim.handle_hyps, handle_hyp returned H_Inversion when \
                one already found. (replacing)";
             Some (p, true), cofixes, hs)
        | H_Transition t -> inversion, cofixes, t :: hs
        | Pass -> inversion, cofixes, hs)
      (None, [], [])
      hyps
  in
  match the_hyps with
  | Some (inv, _), _, _ -> Do_Inversion inv
  | None, _, h :: [] -> H_Transition h
  | None, _, h :: tl ->
    Log.warning
      (Printf.sprintf
         "mebi_bisim.handle_hyps, multiple H transitions returned? (only \
          returning head)\n\
          %s"
         (Strfy.list
            ~force_newline:true
            (fun (x : transition) ->
              Strfy.list
                ~force_newline:true
                ~indent:1
                Strfy.str
                [ Strfy.tuple
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
            (h :: tl)));
    H_Transition h
  | None, [], [] -> Empty
  | None, cofixes, [] -> Cofixes cofixes
;;

(* let temp_evar_econstr () : EConstr.t = *)

let get_all_cofix (gl : Proofview.Goal.t) : Names.Id.Set.t =
  Names.Id.Set.filter
    (fun (x : Names.Id.t) -> Mebi_theories.is_cofix x)
    (Context.Named.to_vars (Proofview.Goal.hyps gl))
;;

let get_all_non_cofix (gl : Proofview.Goal.t) : Names.Id.Set.t =
  Names.Id.Set.filter
    (fun (x : Names.Id.t) -> Bool.not (Mebi_theories.is_cofix x))
    (Context.Named.to_vars (Proofview.Goal.hyps gl))
;;

let clear_old_hyps (gl : Proofview.Goal.t) : unit Proofview.tactic =
  Tactics.clear (Names.Id.Set.to_list (get_all_non_cofix gl))
;;

let handle_new_cofix (gl : Proofview.Goal.t) : unit Proofview.tactic =
  Mebi_theories.tactics
    [ Mebi_tactics.cofix gl
    ; Mebi_tactics.apply (Mebi_theories.c_In_sim ())
    ; Mebi_tactics.apply (Mebi_theories.c_Pack_sim ())
    ; Mebi_tactics.intros_all ()
    ; clear_old_hyps gl
    ]
;;

type proof_state =
  | NewProof
  | NewCofix
  | NewTransition

let determine_proof_state (gl : Proofview.Goal.t) : proof_state option = None

exception CannotUnpackTransitionsOfMN of unit

let get_bisim_states (m : State.t) (n : State.t option) (pi : Partition.t)
  : States.t
  =
  let bisims = Partition.filter (fun p -> States.mem m p) pi in
  assert (Int.equal 1 (Partition.cardinal bisims));
  let states : States.t = List.hd (Partition.to_list bisims) in
  (match n with None -> () | Some n -> assert (States.mem n states));
  states
;;

let handle_eexists
      (gl : Proofview.Goal.t)
      ({ the_fsm_1 = m; the_fsm_2 = n; bisim_states; _ } :
        Algorithms.Bisimilar.result)
      ({ from = m1; action = mA; dest = m2 } : transition)
      ({ from = n1; action = nA; dest = n2 } : transition)
      (cm2 : State.t)
  : unit Proofview.tactic
  =
  match m2, n2 with
  | Some m2, None ->
    assert (State.eq m2 cm2);
    assert (Action.eq ~annos:false ~meta:false mA nA);
    let _from_states = get_bisim_states m1 (Some n1) bisim_states in
    let dest_states = get_bisim_states m2 n2 bisim_states in
    let n_actions_all = Edges.find n.edges n1 in
    let n_actions = Actions.copy n_actions_all in
    Actions.filter_map_inplace
      (fun action dests ->
        match Action.eq ~annos:false ~meta:false nA action with
        | false -> None
        | true -> Some dests)
      n_actions;
    let n_dests = Actions.find n_actions nA in
    let n_candidates = States.inter n_dests dest_states in
    if Int.equal 1 (States.cardinal n_candidates)
    then (
      let n2 : EConstr.t =
        enc_to_econstr (fst (List.hd (States.to_list n_candidates)))
      in
      Mebi_theories.tactics
        [ Tactics.constructor_tac true None 1 (Tactypes.ImplicitBindings [ n2 ])
        ; Tactics.split Tactypes.NoBindings
        ])
    else (
      Log.warning
        (Printf.sprintf
           "mebi_bisim.handle_eexists, more than one candidate for n2: \
            (skipping)\n\
            %s"
           (Strfy.states n_candidates));
      Proofview.tclUNIT ())
  | _, _ -> raise (CannotUnpackTransitionsOfMN ())
;;

let handle_weak_transition
      (gl : Proofview.Goal.t)
      ({ the_fsm_1 = m; the_fsm_2 = n; _ } : Algorithms.Bisimilar.result)
      ({ from = m1; action = mA; dest = m2 } : transition)
      ({ from = n1; action = nA; dest = n2 } : transition)
  : unit Proofview.tactic
  =
  match m2, n2 with
  | Some m2, Some n2 ->
    Mebi_theories.tactics
      [ Mebi_tactics.apply (Mebi_theories.c_wk_none ())
      ; Mebi_tactics.unfold_econstr
          gl
          (match Action.Label.is_silent mA.label with
           | Some true -> Mebi_theories.c_silent ()
           | _ -> Mebi_theories.c_silent1 ())
      ; (if State.eq n1 n2
         then Mebi_tactics.apply (Mebi_theories.c_rt1n_refl ())
         else Mebi_tactics.apply (Mebi_theories.c_rt1n_trans ()))
      ]
  | _, _ -> raise (CannotUnpackTransitionsOfMN ())
;;

let iter_loop (r : Algorithms.Bisimilar.result) : unit Proofview.tactic =
  Proofview.Goal.enter (fun (x : Proofview.Goal.t) ->
    match handle_hyps x r with
    | Empty ->
      Log.notice "mebi_bisim.iter_loop, Empty hyps (start of proof?)";
      handle_new_cofix x
    | Do_Inversion p ->
      Log.notice "mebi_bisim.iter_loop, do inversion";
      p
    | Cofixes cs ->
      Log.notice "mebi_bisim.iter_loop, hyp cofixes (does nothing)";
      Proofview.tclUNIT ()
    | H_Transition mt ->
      Log.notice "mebi_bisim.iter_loop, H_transition mt";
      (match handle_concl x r with
       | New_Weak_Sim ->
         Log.notice "mebi_bisim.iter_loop, New_Weak_Sim";
         handle_new_cofix x
       | Weak_Transition nt ->
         Log.notice "mebi_bisim.iter_loop, Weak_Transition nt";
         handle_weak_transition x r mt nt
       | LTS_Transition _t ->
         Log.notice "mebi_bisim.iter_loop, LTS_Transition _t (does nothing)";
         Proofview.tclUNIT ()
       | Exists (nt, m2) ->
         Log.notice "mebi_bisim.iter_loop, Exists n2";
         handle_eexists x r mt nt m2))
;;

let loop_test () : unit Proofview.tactic =
  Log.debug "mebi_bisim.get_test";
  let the_result = get_the_result () in
  Mebi_theories.tactics
    [ iter_loop !the_result; Mebi_tactics.simplify_and_subst_all () ]
;;
