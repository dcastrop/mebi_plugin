open Logging
open Mebi_wrapper
open Model

let the_cached_result : Algorithms.Bisimilar.result ref option ref = ref None
let reset_the_cached_result () : unit = the_cached_result := None

let get_the_result () : Algorithms.Bisimilar.result ref mm =
  Log.trace "mebi_bisim.get_the_result";
  match !the_cached_result with
  | None -> missing_bisim_result ()
  | Some r -> return r
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

type proof_state =
  | CheckCofix
  | NewIter

(* type hyp_cofix =
  { m : State.t
  ; n : State.t
  } *)

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
  if EConstr.isEvar sigma x then None else Some (econstr_to_enc x)
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

exception Invalid_KindOf_EConstr_Expected_Atomic of EConstr.t
exception UnhandledMebiTheoryKind of Mebi_theories.theory_kind

type concl_kind =
  | Weak_Sim of EConstr.t array
  | Weak_Transition of transition
  | LTS_Transition of transition
  | Pass

let handle_concl
      (gl : Proofview.Goal.t)
      ({ the_fsm_1 = m; the_fsm_2 = n; _ } : Algorithms.Bisimilar.result)
  : concl_kind
  =
  Log.trace "mebi_bisim.handle_concl";
  let env : Environ.env = Proofview.Goal.env gl in
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let the_concl : EConstr.t = Proofview.Goal.concl gl in
  match EConstr.kind_of_type sigma the_concl with
  | AtomicType (ty, tys) ->
    let open Mebi_theories in
    (match match_theory_kind sigma ty with
     | Some k ->
       (match k with
        | Theories_weak ->
          Log.debug "mebi_bisim.handle_concl, Theories_weak";
          Weak_Transition (get_weak_transition n sigma tys)
        | Theories_weak_sim ->
          Log.debug "mebi_bisim.handle_concl, Theories_weak_sim";
          Weak_Sim tys
        | k ->
          Log.warning
            (Printf.sprintf
               "mebi_bisim.handle_concl, unhandledMebiTheoryKind %s"
               (Strfy.econstr env sigma ty));
          raise (UnhandledMebiTheoryKind k))
     | None ->
       if Array.length tys > 2
       then (
         Log.debug
           (Printf.sprintf
              "mebi_bisim.handle_concl, is LTS transition:\n%s"
              (Strfy.list
                 ~force_newline:true
                 (Strfy.econstr env sigma)
                 (Array.to_list tys)));
         LTS_Transition (get_lts_transition n sigma tys))
       else (
         Log.debug "mebi_bisim.handle_concl, pass";
         Pass))
  | _ -> raise (Invalid_KindOf_EConstr_Expected_Atomic the_concl)
;;

let get_cofix (gl : Proofview.Goal.t) : Names.Id.Set.t =
  Names.Id.Set.filter
    (fun (x : Names.Id.t) -> Mebi_theories.is_cofix x)
    (Context.Named.to_vars (Proofview.Goal.hyps gl))
;;

type hyp_kind =
  | Cofix of Mebi_setup.hyp
  | H_To_Invert of Mebi_setup.hyp
  | H_Transition of transition
  | Pass

let handle_hyp
      ({ the_fsm_1 = m; the_fsm_2 = n; _ } : Algorithms.Bisimilar.result)
      env
      sigma
      (h : Mebi_setup.hyp)
  : hyp_kind
  =
  Log.trace "mebi_bisim.handle_hyp";
  let h_ty : EConstr.t = Context.Named.Declaration.get_type h in
  match EConstr.kind_of_type sigma h_ty with
  | AtomicType (ty, tys) ->
    let open Mebi_theories in
    (match match_theory_kind sigma ty with
     | Some k ->
       (match k with
        | Theories_weak_sim ->
          Log.debug
            (Printf.sprintf
               "mebi_bisim.handle_hyp, is Cofix (Theories_weak_sim: %b)"
               (Mebi_theories.is_cofix (Context.Named.Declaration.get_id h)));
          Cofix h
        | k ->
          Log.warning
            (Printf.sprintf
               "mebi_bisim.handle_hyp, unhandledMebiTheoryKind %s"
               (Strfy.econstr env sigma ty));
          raise (UnhandledMebiTheoryKind k))
     | None ->
       if Array.length tys > 2
       then
         if to_invert sigma tys
         then (
           Log.debug "mebi_bisim.handle_hyp, is H (LTS transition, to invert)";
           H_To_Invert h)
         else (
           Log.debug "mebi_bisim.handle_hyp, is H (LTS transition, full)";
           Log.debug
             (Printf.sprintf
                "mebi_bisim.handle_hyp, is LTS transition:\n%s"
                (Strfy.list
                   ~force_newline:true
                   (Strfy.econstr env sigma)
                   (Array.to_list tys)));
           H_Transition (get_lts_transition m sigma tys))
       else (
         Log.debug "mebi_bisim.handle_hyp, Pass";
         Pass))
  | _ -> raise (Invalid_KindOf_EConstr_Expected_Atomic h_ty)
;;

exception ExpectedOnlyOne_H_ToBeInverted of Mebi_setup.hyp list

let do_invert (hyps : hyp_kind list) : unit Proofview.tactic =
  let hyps : Mebi_setup.hyp list =
    List.fold_left
      (fun acc h -> match h with H_To_Invert h' -> h' :: acc | _ -> acc)
      []
      hyps
  in
  match hyps with
  | h :: [] -> Inv.inv_clear_tac (Context.Named.Declaration.get_id h)
  | _ -> raise (ExpectedOnlyOne_H_ToBeInverted hyps)
;;

let handle_hyps (gl : Proofview.Goal.t) (r : Algorithms.Bisimilar.result) : unit
  =
  Log.trace "mebi_bisim.handle_hyps";
  let env : Environ.env = Proofview.Goal.env gl in
  let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
  let the_hyps : EConstr.named_context = Proofview.Goal.hyps gl in
  let _hyps =
    List.fold_left (fun acc h -> handle_hyp r env sigma h :: acc) [] the_hyps
  in
  (* TODO: determine  *)
  (* TODO: using [proof_state] automatically apply these in a loop *)
  ()
;;

let determine_proof_state (gl : Proofview.Goal.t) : proof_state option = None

let get_test () : unit Proofview.tactic mm =
  Log.trace "mebi_bisim.get_test";
  let open Syntax in
  let* the_result = get_the_result () in
  return
    (Proofview.Goal.enter (fun (x : Proofview.Goal.t) ->
       match determine_proof_state x with
       | Some p ->
         (* NOTE: temporary *)
         let _ = handle_concl x !the_result in
         let _ = handle_hyps x !the_result in
         (match p with
          | CheckCofix -> Proofview.tclUNIT ()
          | NewIter -> Proofview.tclUNIT ())
       (* NOTE: [determine_proof_state] will not be an option *)
       | None -> Proofview.tclUNIT ()))
;;
