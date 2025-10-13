open Logging
open Mebi_wrapper

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

type hyp_cofix =
  { m : Model.State.t
  ; n : Model.State.t
  }

type transition =
  { from : Model.State.t
  ; action : Model.Action.t
  ; dest : Model.State.t option
  }

exception State_Of_Enc_NotFound of (Mebi_wrapper.Enc.t * Model.States.t)

exception
  Error_Multiple_States_Of_Enc_Found of (Mebi_wrapper.Enc.t * Model.States.t)

let find_state_of_enc (x : Mebi_wrapper.Enc.t) (s : Model.States.t)
  : Model.State.t
  =
  match
    List.filter
      (fun ((y, _) : Model.State.t) -> Mebi_setup.Eq.enc x y)
      (Model.States.to_list s)
  with
  | h :: [] -> h
  | [] -> raise (State_Of_Enc_NotFound (x, s))
  | _ :: _ -> raise (Error_Multiple_States_Of_Enc_Found (x, s))
;;

exception Label_Of_Enc_NotFound of (Mebi_wrapper.Enc.t * Model.Alphabet.t)

exception
  Error_Multiple_Labels_Of_Enc_Found of (Mebi_wrapper.Enc.t * Model.Alphabet.t)

let find_label_of_enc (x : Mebi_wrapper.Enc.t) (s : Model.Alphabet.t)
  : Model.Action.Label.t
  =
  match
    List.filter
      (fun ((y, _) : Model.Action.Label.t) -> Mebi_setup.Eq.enc x y)
      (Model.Alphabet.to_list s)
  with
  | h :: [] -> h
  | [] -> raise (Label_Of_Enc_NotFound (x, s))
  | _ :: _ -> raise (Error_Multiple_Labels_Of_Enc_Found (x, s))
;;

let get_concl_transition (m : Fsm.t) env sigma : EConstr.t -> transition mm =
  (* let get_transition (m : Fsm.t) env sigma : EConstr.t -> transition mm = *)
  fun (x : EConstr.t) ->
  Log.trace "mebi_bisim.get_concl_transition";
  match EConstr.kind_of_type sigma x with
  | AtomicType (_ty, tys) ->
    assert (Array.length tys = 6);
    (* assert (EConstr.isInd sigma tys.(0) && EConstr.isRef sigma tys.(0));
    assert (EConstr.isInd sigma tys.(1) && EConstr.isRef sigma tys.(1));
    assert (EConstr.isInd sigma tys.(2) && EConstr.isRef sigma tys.(2)); *)
    (* assert (EConstr.isApp sigma tys.(3)); *)
    (* assert (EConstr.isApp sigma tys.(4)); *)
    (* assert (EConstr.isApp sigma tys.(5)); *)
    let open Syntax in
    let* _ = Mebi_wrapper.debug_econstr_kind tys.(3) in
    Log.debug "mebi_bisim.get_concl_transition, from_enc";
    let* from_enc = Mebi_wrapper.get_encoding tys.(3) in
    let* _ = Mebi_wrapper.debug_econstr_kind tys.(4) in
    Log.debug "mebi_bisim.get_concl_transition, label_enc";
    let* label_enc = Mebi_wrapper.get_encoding tys.(4) in
    let* _ = Mebi_wrapper.debug_econstr_kind tys.(5) in
    Log.debug "mebi_bisim.get_concl_transition, dest_enc";
    let* dest_enc = Mebi_wrapper.get_encoding tys.(5) in
    let from = find_state_of_enc from_enc m.states in
    let label = find_label_of_enc from_enc m.alphabet in
    let actions = Model.get_actions_from from m.edges in
    let action = Model.get_action_with_label actions label in
    return { from; action; dest = None }
  | _ -> invalid_kind_of_econstr_expected_atomic x
;;

(* let get_hyp_transition (m : Fsm.t) env sigma : EConstr.t -> transition mm =
  fun (x : EConstr.t) ->
  Log.trace "mebi_bisim.get_hyp_transition";
  match EConstr.kind_of_type sigma x with
  | AtomicType (_ty, tys) ->
    assert (Array.length tys = 3);
    let open Syntax in
    let* from_enc = Mebi_wrapper.get_encoding tys.(3) in
    let from = find_state_of_enc from_enc m.states in
    let* dest_enc = Mebi_wrapper.get_encoding tys.(4) in
    let dest = Some (find_state_of_enc from_enc m.states) in
    let* label_enc = Mebi_wrapper.get_encoding tys.(5) in
    let label = find_label_of_enc from_enc m.alphabet in
    let actions = Model.get_actions_from from m.edges in
    let action = Model.get_action_with_label actions label in
    return { from; action; dest }
  | _ -> invalid_kind_of_econstr_expected_atomic x
;; *)

exception Invalid_KindOf_EConstr_Expected_Atomic of EConstr.t

let get_econstr_transition (m : Fsm.t) env sigma
  : EConstr.t -> EConstr.t * EConstr.t * EConstr.t
  =
  (* let get_transition (m : Fsm.t) env sigma : EConstr.t -> transition mm = *)
  fun (x : EConstr.t) ->
  Log.trace "mebi_bisim.get_econstr_transition";
  match EConstr.kind_of_type sigma x with
  | AtomicType (_ty, tys) ->
    assert (Array.length tys = 6);
    assert (EConstr.isInd sigma tys.(0) && EConstr.isRef sigma tys.(0));
    assert (EConstr.isInd sigma tys.(1) && EConstr.isRef sigma tys.(1));
    assert (EConstr.isInd sigma tys.(2) && EConstr.isRef sigma tys.(2));
    assert (EConstr.isApp sigma tys.(3));
    (* Log.debug
       (Printf.sprintf
       "mebi_bisim.get_econstr_transition, 0: %s"
       (Strfy.econstr_kind env sigma tys.(0)));
       Log.debug
       (Printf.sprintf
       "mebi_bisim.get_econstr_transition, 1: %s"
       (Strfy.econstr_kind env sigma tys.(1)));
       Log.debug
       (Printf.sprintf
       "mebi_bisim.get_econstr_transition, 2: %s"
       (Strfy.econstr_kind env sigma tys.(2)));
       Log.debug
       (Printf.sprintf
       "mebi_bisim.get_econstr_transition, 3: %s"
       (Strfy.econstr_kind env sigma tys.(3)));
       Log.debug
       (Printf.sprintf
       "mebi_bisim.get_econstr_transition, 4: %s"
       (Strfy.econstr_kind env sigma tys.(4)));
       Log.debug
       (Printf.sprintf
       "mebi_bisim.get_econstr_transition, 5: %s"
       (Strfy.econstr_kind env sigma tys.(5))); *)
    assert (EConstr.isApp sigma tys.(4) || EConstr.isEvar sigma tys.(4));
    assert (EConstr.isApp sigma tys.(5));
    tys.(3), tys.(4), tys.(5)
  | _ -> raise (Invalid_KindOf_EConstr_Expected_Atomic x)
;;

(* let get_concl_transition (n:Fsm.t) env sigma : EConstr.t -> concl_transition mm =
  fun (x:EConstr.t) ->
  match EConstr.kind_of_type sigma x with
    | AtomicType (_ty, tys) ->
      let open Syntax in
      assert (Array.length tys = 3);
      let* from_enc = Mebi_wrapper.get_encoding (tys.(0)) in 
      let* action_enc = Mebi_wrapper.get_encoding (tys.(1)) in 
      let* dest_enc = Mebi_wrapper.get_encoding (tys.(2)) in 
      let from = find_state_of_enc from_enc n.states in
      let action = find_action_of_enc from_enc n.states in
      let dest = find_state_of_enc from_enc n.states in
return

      {from  
      ; action; dest }

    | _ -> raise (Invalid_KindOfTypeEConstr_Expected_Atomic  x) *)
exception Invalid_KindOfTypeEConstr_Expected_Atomic of EConstr.t

let get_test () : unit Proofview.tactic mm =
  Log.trace "mebi_bisim.get_test";
  let open Syntax in
  let* _ = Mebi_wrapper.show_fwd_map () in
  let* _ = Mebi_wrapper.show_bck_map () in
  let* the_result = get_the_result () in
  return
    (Proofview.Goal.enter (fun (x : Proofview.Goal.t) ->
       let env : Environ.env = Proofview.Goal.env x in
       let sigma : Evd.evar_map = Proofview.Goal.sigma x in
       let the_concl : EConstr.t = Proofview.Goal.concl x in
       let n1, n2, n_act =
         get_econstr_transition !the_result.the_fsm_2 env sigma the_concl
       in
       Log.debug
         (Printf.sprintf
            "mebi_bisim.get_test, the concl:\n- n1: %s\n- n2: %s\n- n_act: %s"
            (Strfy.econstr env sigma n1)
            (Strfy.econstr env sigma n2)
            (Strfy.econstr env sigma n_act));
       (* let the_hyps : EConstr.named_context = Proofview.Goal.hyps x in *)
       let { from = _m1; action = _mAct; dest = _m2 } =
         Mebi_wrapper.run
           ~keep_encoding:true
           ~fresh:false
           (get_concl_transition !the_result.the_fsm_1 env sigma the_concl)
       in
       (* let {n1=from; nAct=action;n2=dest} = get_concl_transition the_result.n env sigma in  *)
       Proofview.tclUNIT ()
       (* return () *)))
;;

(* type isolated_terms = {m1:Model.State.t;m2:Model.State.t;n1:Model.State.t;n2:Model.State.t option}



let get_isolated_terms () : Proofview.Goal.t -> isolated_terms = 
    fun (x : Proofview.Goal.t) ->
    let env : Environ.env = Proofview.Goal.env x in
    let sigma : Evd.evar_map = Proofview.Goal.sigma x in
*)
