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

let find_state_of_enc_opt (x : Mebi_wrapper.Enc.t option) (s : Model.States.t)
  : Model.State.t option
  =
  match x with None -> None | Some x -> Some (find_state_of_enc x s)
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

let econstr_to_enc : EConstr.t -> Enc.t =
  fun (x : EConstr.t) ->
  Mebi_wrapper.run
    ~keep_encoding:true
    ~fresh:false
    (Mebi_wrapper.get_encoding x)
;;

let econstr_to_enc_opt (sigma : Evd.evar_map) : EConstr.t -> Enc.t option =
  fun (x : EConstr.t) ->
  if EConstr.isEvar sigma x then None else Some (econstr_to_enc x)
;;

exception Invalid_KindOf_EConstr_Expected_Atomic of EConstr.t

type econstr_enc = Enc.t * EConstr.t
type econstr_evar = Enc.t option * EConstr.t

let get_transition (m : Fsm.t) env sigma : EConstr.t -> transition =
  fun (x : EConstr.t) ->
  Log.trace "mebi_bisim.get_transition";
  Log.debug (Printf.sprintf "mebi_bisim.get_transition, fsm: %s" (Fsm.pstr m));
  match EConstr.kind_of_type sigma x with
  | AtomicType (_ty, tys) ->
    Log.debug "mebi_bisim.get_transition A A";
    assert (Array.length tys = 6);
    assert (EConstr.isInd sigma tys.(0) && EConstr.isRef sigma tys.(0));
    assert (EConstr.isInd sigma tys.(1) && EConstr.isRef sigma tys.(1));
    assert (EConstr.isInd sigma tys.(2) && EConstr.isRef sigma tys.(2));
    assert (EConstr.isApp sigma tys.(3));
    assert (EConstr.isApp sigma tys.(4) || EConstr.isEvar sigma tys.(4));
    assert (EConstr.isApp sigma tys.(5));
    (* *)
    let from_enc = econstr_to_enc tys.(3) in
    let from = find_state_of_enc from_enc m.states in
    Log.debug
      (Printf.sprintf "mebi_bisim.get_transition, from:\n%s" (Strfy.state from));
    (* *)
    let label_enc = econstr_to_enc tys.(5) in
    let label = find_label_of_enc label_enc m.alphabet in
    Log.debug
      (Printf.sprintf
         "mebi_bisim.get_transition, label:\n%s"
         (Strfy.action_label label));
    let actions = Model.get_actions_from from m.edges in
    Log.debug
      (Printf.sprintf
         "mebi_bisim.get_transition, actions:\n[%s]"
         (Model.Actions.fold
            (fun a d acc ->
              Printf.sprintf
                "%s { action: \n  %s\n  ; states: %s\n  }\n "
                acc
                (Strfy.action ~indent:2 a)
                (Strfy.states d))
            actions
            ""));
    let action = Model.get_action_with_label actions label in
    Log.debug
      (Printf.sprintf
         "mebi_bisim.get_transition, action:\n%s"
         (Strfy.action action));
    (* *)
    let dest_enc = econstr_to_enc_opt sigma tys.(4) in
    let dest = find_state_of_enc_opt dest_enc m.states in
    Log.debug
      (Printf.sprintf
         "mebi_bisim.get_transition, dest:\n%s"
         (Strfy.option Strfy.state dest));
    { from; action; dest }
  | _ -> raise (Invalid_KindOf_EConstr_Expected_Atomic x)
;;

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
       let { from = n1; action = n_act; dest = n2 } =
         get_transition !the_result.the_fsm_2 env sigma the_concl
       in
       Log.notice
         (Printf.sprintf
            "mebi_bisim.get_test, the concl:\n\
             - n1:\n\
             %s\n\
             - n_act:\n\
             %s\n\
             - n2:\n\
             %s"
            (Strfy.state ~indent:1 n1)
            (Strfy.action ~indent:1 n_act)
            (Strfy.option Strfy.state n2));
       Proofview.tclUNIT ()))
;;
