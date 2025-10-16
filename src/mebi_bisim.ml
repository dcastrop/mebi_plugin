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
exception Invalid_KindOf_EConstr_Expected_Lambda of EConstr.t
exception Invalid_KindOf_EConstr_Expected_App of EConstr.t
exception ExpectedAppToContainWkTransAndSim of EConstr.t array
exception UnhandledMebiTheoryKind of Mebi_theories.theory_kind
exception UnhandledConcl of EConstr.t

let try_get_weak_transition gl n x : transition option =
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  match EConstr.kind_of_type sigma x with
  | AtomicType (ty, tys) ->
    (match Mebi_theories.match_theory_kind sigma ty with
     | Some k ->
       (match k with
        | Theories_weak -> Some (get_weak_transition n sigma tys)
        | _ -> None)
     | _ -> None)
  | _ -> raise (Invalid_KindOf_EConstr_Expected_Atomic x)
;;

let try_get_weak_sim gl x : unit option =
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  match EConstr.kind_of_type sigma x with
  | AtomicType (ty, tys) ->
    (match Mebi_theories.match_theory_kind sigma ty with
     | Some k -> (match k with Theories_weak_sim -> Some () | _ -> None)
     | _ -> None)
  | _ -> raise (Invalid_KindOf_EConstr_Expected_Atomic x)
;;

let try_get_m_state_weak_sim gl (m : Fsm.t) x : State.t option =
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  match EConstr.kind_of_type sigma x with
  | AtomicType (ty, tys) ->
    (match Mebi_theories.match_theory_kind sigma ty with
     | Some k ->
       (match k with
        | Theories_weak_sim ->
          Some (find_state_of_enc (econstr_to_enc tys.(5)) m.states)
        | k -> raise (UnhandledMebiTheoryKind k))
     | None -> None)
  | _ -> raise (Invalid_KindOf_EConstr_Expected_Atomic x)
;;

let try_get_exists gl m n x : (transition * State.t) option =
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  match EConstr.kind_of_type sigma x with
  | AtomicType (ty, tys) ->
    (match EConstr.kind sigma tys.(1) with
     | Lambda (_binder, _types, constr) ->
       (match EConstr.kind sigma constr with
        | App (_ty, tys) ->
          (match Array.to_list tys with
           | [ wk_trans; wk_sim ] ->
             (match try_get_weak_transition gl n wk_trans with
              | None -> None
              | Some t ->
                (match try_get_m_state_weak_sim gl m wk_sim with
                 | None -> None
                 | Some s -> Some (t, s)))
           | _ -> None)
        | _ -> None)
     | _ -> None)
  | _ -> raise (Invalid_KindOf_EConstr_Expected_Atomic x)
;;

let try_get_lts_transition gl n x : transition option =
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  match EConstr.kind_of_type sigma x with
  | AtomicType (ty, tys) ->
    (match Mebi_theories.match_theory_kind sigma ty with
     | Some k ->
       (match k with
        | Theories_LTS -> Some (get_lts_transition n sigma tys)
        | _ -> None)
     | _ -> None)
  | _ -> raise (Invalid_KindOf_EConstr_Expected_Atomic x)
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
        | Some (n_transition, m_state) ->
          (* let n_states : States.t = get_n_states gl m n bisim_states m_state n_transition
             in
             Exists (n_transition, m_state, n_states) *)
          Exists (n_transition, m_state)
        | None ->
          (match try_get_lts_transition gl n the_concl with
           | Some t -> LTS_Transition t
           | None -> raise (UnhandledConcl the_concl))))
;;

(*
   match EConstr.kind_of_type sigma the_concl with
   | AtomicType (ty, tys) ->
   let open Mebi_theories in
   (match match_theory_kind sigma ty with
   | Some k ->
   (match k with
   | Theories_weak -> Weak_Transition (get_weak_transition n sigma tys)
   | Theories_weak_sim -> New_Weak_Sim
   | k -> raise (UnhandledMebiTheoryKind k))
   | None ->
   (match Array.length tys with
   | 2 ->
   (match EConstr.kind sigma tys.(1) with
   | Lambda (_binder, _types, constr) ->
   (match EConstr.kind sigma constr with
   | App (_ty, tys) ->
   (match Array.to_list tys with
   | [ wk_trans; wk_sim ] ->
   let n_transition : transition =
   match EConstr.kind_of_type sigma tys.(0) with
   | AtomicType (ty, tys) ->
   (match match_theory_kind sigma ty with
   | Some k ->
   (match k with
   | Theories_weak -> get_weak_transition n sigma tys
   | k -> raise (UnhandledMebiTheoryKind k))
   | None -> raise (UnhandledConcl (ty, tys)))
   | _ -> raise (Invalid_KindOf_EConstr_Expected_Atomic tys.(0))
   in
   let m_dest_state : State.t =
   match EConstr.kind_of_type sigma tys.(1) with
   | AtomicType (ty, tys) ->
   (match Mebi_theories.match_theory_kind sigma ty with
   | Some k ->
   (match k with
   | Mebi_theories.Theories_weak_sim ->
   find_state_of_enc (econstr_to_enc tys.(5)) m.states
   | k -> raise (UnhandledMebiTheoryKind k))
   | None -> raise (UnhandledConcl (ty, tys)))
   | _ -> raise (Invalid_KindOf_EConstr_Expected_Atomic tys.(1))
   in
   Exists (n_transition, m_dest_state)
   | _ -> raise (ExpectedAppToContainWkTransAndSim tys))
   | _ -> raise (Invalid_KindOf_EConstr_Expected_App constr))
   | _ -> raise (Invalid_KindOf_EConstr_Expected_Lambda tys.(1)))
   | 3 -> LTS_Transition (get_lts_transition n sigma tys)
   | _ -> raise (UnhandledConcl (ty, tys))))
   | _ -> raise (Invalid_KindOf_EConstr_Expected_Atomic the_concl) *)

type hyp_kind =
  | Cofix of hyp_cofix
  | H_Inversion of unit Proofview.tactic
  | H_Transition of transition
  | Pass

exception ExpectedOnlyOne_H_ToBeInverted of Mebi_setup.hyp list

let handle_hyp
      ({ the_fsm_1 = m; the_fsm_2 = n; _ } : Algorithms.Bisimilar.result)
      env
      sigma
      (h : Mebi_setup.hyp)
  : hyp_kind
  =
  Log.debug "mebi_bisim.handle_hyp";
  let h_ty : EConstr.t = Context.Named.Declaration.get_type h in
  match EConstr.kind_of_type sigma h_ty with
  | AtomicType (ty, tys) ->
    (match Mebi_theories.match_theory_kind sigma ty with
     | Some k ->
       (match k with
        | Mebi_theories.Theories_weak_sim -> Cofix (get_cofix m n env sigma tys)
        | k -> raise (UnhandledMebiTheoryKind k))
     | None ->
       if Array.length tys < 3
       then Pass
       else if Mebi_theories.need_to_invert sigma tys
       then H_Inversion (Mebi_tactics.do_inversion h)
       else H_Transition (get_lts_transition m sigma tys))
  | _ -> raise (Invalid_KindOf_EConstr_Expected_Atomic h_ty)
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
  let hyps : Mebi_setup.hyp list = Proofview.Goal.hyps gl in
  let the_hyps : unit Proofview.tactic option * hyp_cofix list * transition list
    =
    List.fold_left
      (fun (inversion, cofixes, hs) h ->
        match handle_hyp r env sigma h with
        | Cofix x -> inversion, x :: cofixes, hs
        | H_Inversion p ->
          (match inversion with
           | None -> Some p, cofixes, hs
           | Some p' ->
             Log.warning
               "mebi_bisim.handle_hyps, handle_hyp returned H_Inversion when \
                one already found. (replacing)";
             Some p, cofixes, hs)
        | H_Transition t -> inversion, cofixes, t :: hs
        | Pass -> inversion, cofixes, hs)
      (None, [], [])
      hyps
  in
  match the_hyps with
  | Some inv, _, _ -> Do_Inversion inv
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

let handle_new_cofix (gl : Proofview.Goal.t) : unit Proofview.tactic =
  Proofview.tclTHEN
    (Mebi_tactics.cofix gl)
    (Proofview.tclTHEN
       (Mebi_tactics.apply (Mebi_theories.c_In_sim ()))
       (Proofview.tclTHEN
          (Mebi_tactics.apply (Mebi_theories.c_Pack_sim ()))
          (Mebi_tactics.intros_all ())
          (* (Proofview.tclTHEN
             (Mebi_tactics.intros_all ())
             (Refine.refine ~typecheck:true (fun sigma -> _ ))) *)))
;;

let get_all_cofix (gl : Proofview.Goal.t) : Names.Id.Set.t =
  Names.Id.Set.filter
    (fun (x : Names.Id.t) -> Mebi_theories.is_cofix x)
    (Context.Named.to_vars (Proofview.Goal.hyps gl))
;;

(* type iter_result =
   | Do_Tactic of unit Proofview.tactic
   | Try_Cofixes of hyp_cofix list
   | H_Transition of transition *)

(* let iter_hyps r x : hyp_result =
   match handle_hyps x r with
   | Do_Inversion p -> Do_Inversion p
   | H_Transition t -> H_Transition t
   | Cofixes cs -> Cofixes cs
   | Empty -> Empty
   ;;

   let iter_concl r x : unit Proofview.tactic option =
   match handle_concl x r with
   | New_Weak_Sim -> Some (handle_new_cofix x)
   | Weak_Transition _t ->
   Log.debug "mebi_bisim.iter_concl, Weak_Transition _t (does nothing)";
   Some (Proofview.tclUNIT ())
   | LTS_Transition _t ->
   Log.debug "mebi_bisim.iter_concl, LTS_Transition _t (does nothing)";
   Some (Proofview.tclUNIT ())
   | Exists (_nt, _m2) ->
   Log.debug
   (Printf.sprintf
   "mebi_bisim.iter_concl, Exists (_nt, _m) (does nothing)\n\
   m2:\n\
   %s\n\n\
   nt:\n\
   - from: %s\n\
   - action: %s\n\
   - dest: %s"
     (Strfy.state _m2)
     (Strfy.state _nt.from)
     (Strfy.action _nt.action)
     (Strfy.option Strfy.state _nt.dest));
     None
     ;; *)

type proof_state =
  | NewProof
  | NewCofix
  | NewTransition

let determine_proof_state (gl : Proofview.Goal.t) : proof_state option = None

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
      Log.notice "mebi_bisim.iter_loop, hyp cofixes";
      Proofview.tclUNIT ()
    | H_Transition t ->
      (match handle_concl x r with
       | New_Weak_Sim -> handle_new_cofix x
       | Weak_Transition _t ->
         Log.notice "mebi_bisim.iter_loop, Weak_Transition _t (does nothing)";
         Proofview.tclUNIT ()
       | LTS_Transition _t ->
         Log.notice "mebi_bisim.iter_loop, LTS_Transition _t (does nothing)";
         Proofview.tclUNIT ()
       | Exists (_nt, _m2) ->
         Log.notice
           (Printf.sprintf
              "mebi_bisim.iter_loop, Exists (_nt, _m) (does nothing)\n\
               TODO: use the destination of transition H to find bisimilar \
               states, which are reachable from the beginning of transition Nt.\n\
               where H:\n\
               - from: %s\n\
               - action: %s\n\
               - dest: %s\n\
               and Nt begins with:\n\
               %s"
              (Strfy.state t.from)
              (Strfy.action t.action)
              (Strfy.option Strfy.state t.dest)
              (Strfy.state _nt.from));
         Log.debug
           (Printf.sprintf
              "m2:\n%s\n\nnt:\n- from: %s\n- action: %s\n- dest: %s"
              (Strfy.state _m2)
              (Strfy.state _nt.from)
              (Strfy.action _nt.action)
              (Strfy.option Strfy.state _nt.dest));
         Proofview.tclUNIT ()
         (* match iter_concl r x with
            | None ->
            Log.warning "mebi_bisim.iter_loop, Nothing to do";
            Proofview.tclUNIT ()
            | Some p ->
            Log.notice "mebi_bisim.iter_loop, Some concl to do";
            Proofview.tclUNIT () *)))
;;

let loop_test () : unit Proofview.tactic =
  Log.debug "mebi_bisim.get_test";
  let the_result = get_the_result () in
  (* let iter = iter_loop !the_result in
     return iter *)
  Proofview.tclTHEN
    (iter_loop !the_result)
    (Mebi_tactics.simplify_and_subst_all ())
;;
(* let iter_body (i:int) (acc:unit Proofview.tactic) =

   return (Proofview.tactic acc) in
   iterate 0 1 (unit Proofview.tactic) iter_body *)

(*return
  (

  Proofview.Goal.enter (fun (x : Proofview.Goal.t) ->
  proof_loop !the_result x
  (* Log.debug "mebi_bisim.get_test A";
  match determine_proof_state x with
  | Some p ->
  (* NOTE: temporary *)
  let _ = handle_concl x !the_result in
  let _ = handle_hyps x !the_result in
  (match p with
  | CheckCofix -> Proofview.tclUNIT ()
  | NewIter -> Proofview.tclUNIT ())
  (* NOTE: [determine_proof_state] will not be an option *)
  | None -> Proofview.tclUNIT () *)
  (* let _ = handle_concl x !the_result in
  let _ = handle_hyps x !the_result in
  Proofview.tclUNIT () *)
  )) *)
