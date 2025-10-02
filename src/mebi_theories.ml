open Mebi_wrapper
open Logging

(* source: https://github.com/rocq-prover/rocq/blob/master/doc/plugin_tutorial/tuto3/src/tuto_tactic.ml *)

let constants : EConstr.t list ref = ref ([] : EConstr.t list)

(* This is a pattern to collect terms from the Coq memory of valid terms
   and proofs.  This pattern extends all the way to the definition of function
   c_U *)
let collect_bisimilarity_theories () : EConstr.t list mm =
  Log.trace "mebi_theories.collect_bisimilarity_theories";
  match !constants with
  | [] ->
    let open Mebi_wrapper.Syntax in
    let* env = get_env in
    let find_reference (path : string list) (id : string) : Names.GlobRef.t =
      let path = Names.DirPath.make (List.rev_map Names.Id.of_string path) in
      let fp = Libnames.make_path path (Names.Id.of_string id) in
      Nametab.global_of_path fp
    in
    Log.debug "mebi_theories.collect_bisimilarity_theories, mapping constants";
    constants
    := List.map
         (fun (x : Names.GlobRef.t) ->
           EConstr.of_constr (UnivGen.constr_of_monomorphic_global env x))
         [ find_reference [ "MEBI"; "Bisimilarity" ] "LTS"
         ; find_reference [ "MEBI"; "Bisimilarity" ] "tau"
         ; find_reference [ "MEBI"; "Bisimilarity" ] "silent"
         ; find_reference [ "MEBI"; "Bisimilarity" ] "silent1"
         ; find_reference [ "MEBI"; "Bisimilarity" ] "weak"
         ; find_reference [ "MEBI"; "Bisimilarity" ] "wk_some"
         ; find_reference [ "MEBI"; "Bisimilarity" ] "wk_none"
         ; find_reference [ "MEBI"; "Bisimilarity" ] "simF"
         ; find_reference [ "MEBI"; "Bisimilarity" ] "Pack_sim"
         ; find_reference [ "MEBI"; "Bisimilarity" ] "sim_weak"
         ; find_reference [ "MEBI"; "Bisimilarity" ] "weak_sim"
         ; find_reference [ "MEBI"; "Bisimilarity" ] "In_sim"
         ; find_reference [ "MEBI"; "Bisimilarity" ] "out_sim"
         ; find_reference [ "MEBI"; "Bisimilarity" ] "weak_bisim"
         ];
    Mebi_wrapper.return !constants
  | _ -> Mebi_wrapper.return !constants
;;

let c_LTS () : EConstr.t mm =
  Log.trace "mebi_theories.c_LTS";
  let open Mebi_wrapper.Syntax in
  let* cs = collect_bisimilarity_theories () in
  match cs with
  | it :: _ -> return it
  | _ ->
    failwith
      "could not obtain an internal representation of Theories.Bisimilarity.LTS"
;;

let c_tau () : EConstr.t mm =
  Log.trace "mebi_theories.c_tau";
  let open Mebi_wrapper.Syntax in
  let* cs = collect_bisimilarity_theories () in
  match cs with
  | _ :: it :: _ -> return it
  | _ ->
    failwith
      "could not obtain an internal representation of Theories.Bisimilarity.tau"
;;

let c_silent () : EConstr.t mm =
  Log.trace "mebi_theories.c_silent";
  let open Mebi_wrapper.Syntax in
  let* cs = collect_bisimilarity_theories () in
  match cs with
  | _ :: _ :: it :: _ -> return it
  | _ ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.silent"
;;

let c_silent1 () : EConstr.t mm =
  Log.trace "mebi_theories.c_silent1";
  let open Mebi_wrapper.Syntax in
  let* cs = collect_bisimilarity_theories () in
  match cs with
  | _ :: _ :: _ :: it :: _ -> return it
  | _ ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.silent1"
;;

let c_weak () : EConstr.t mm =
  Log.trace "mebi_theories.c_weak";
  let open Mebi_wrapper.Syntax in
  let* cs = collect_bisimilarity_theories () in
  match cs with
  | _ :: _ :: _ :: _ :: it :: _ -> return it
  | _ ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.weak"
;;

let c_wk_some () : EConstr.t mm =
  Log.trace "mebi_theories.c_wk_some";
  let open Mebi_wrapper.Syntax in
  let* cs = collect_bisimilarity_theories () in
  match cs with
  | _ :: _ :: _ :: _ :: _ :: it :: _ -> return it
  | _ ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.wk_some"
;;

let c_wk_none () : EConstr.t mm =
  Log.trace "mebi_theories.c_wk_none";
  let open Mebi_wrapper.Syntax in
  let* cs = collect_bisimilarity_theories () in
  match cs with
  | _ :: _ :: _ :: _ :: _ :: _ :: it :: _ -> return it
  | _ ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.wk_none"
;;

let c_simF () : EConstr.t mm =
  Log.trace "mebi_theories.c_simF";
  let open Mebi_wrapper.Syntax in
  let* cs = collect_bisimilarity_theories () in
  match cs with
  | _ :: _ :: _ :: _ :: _ :: _ :: _ :: it :: _ -> return it
  | _ ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.simF"
;;

let c_Pack_sim () : EConstr.t mm =
  Log.trace "mebi_theories.c_Pack_sim";
  let open Mebi_wrapper.Syntax in
  let* cs = collect_bisimilarity_theories () in
  match cs with
  | _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: it :: _ -> return it
  | _ ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.Pack_sim"
;;

let c_sim_weak () : EConstr.t mm =
  Log.trace "mebi_theories.c_sim_weak";
  let open Mebi_wrapper.Syntax in
  let* cs = collect_bisimilarity_theories () in
  match cs with
  | _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: it :: _ -> return it
  | _ ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.sim_weak"
;;

let c_weak_sim () : EConstr.t mm =
  Log.trace "mebi_theories.c_weak_sim";
  let open Mebi_wrapper.Syntax in
  let* cs = collect_bisimilarity_theories () in
  match cs with
  | _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: it :: _ -> return it
  | _ ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.weak_sim"
;;

let c_In_sim () : EConstr.t mm =
  Log.trace "mebi_theories.c_In_sim";
  let open Mebi_wrapper.Syntax in
  let* cs = collect_bisimilarity_theories () in
  match cs with
  | _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: it :: _ -> return it
  | _ ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.In_sim"
;;

let c_out_sim () : EConstr.t mm =
  Log.trace "mebi_theories.c_out_sim";
  let open Mebi_wrapper.Syntax in
  let* cs = collect_bisimilarity_theories () in
  match cs with
  | _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: it :: _ ->
    return it
  | _ ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.out_sim"
;;

let c_weak_bisim () : EConstr.t mm =
  Log.trace "mebi_theories.c_weak_bisim";
  let open Mebi_wrapper.Syntax in
  let* cs = collect_bisimilarity_theories () in
  match cs with
  | _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: it :: _ ->
    return it
  | _ ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.weak_bisim"
;;

(* The following tactic is meant to pack an hypothesis when no other
   data is already packed.

   The main difficulty in defining this tactic is to understand how to
   construct the input expected by apply_in. *)
let _package (i : Names.Id.t) : unit Proofview.tactic =
  Proofview.Goal.enter (fun gl ->
    Tactics.apply_in
      true
      false
      i
      [ (* this means that the applied theorem is not to be cleared. *)
        (* None, (CAst.make (c_M (), *)
        (* we don't specialize the theorem with extra values. *)
        (* Tactypes.NoBindings)) *) ]
      (* we don't destruct the result according to any intro_pattern *)
      None)
;;

(* 
let proof_test () : unit Proofview.tactic mm =
  fun (st : wrapper ref) ->
  Log.debug "mebi_wrapper.proof_test";
  let _h_hyps_id = Names.Id.of_string "TestPacked" in
  (* *)
  { state = st
  ; value =
      Proofview.Goal.enter (fun gl ->
        let _hyps = Environ.named_context_val (Proofview.Goal.env gl) in
        Proofview.tclUNIT ())
      (* let x = Proofview.Goal.goal gl in

         if Termops.mem_named_context_val h_hyps_id hyps then
         Proofview.tclTHEN (repackage i h_hyps_id)
         (Proofview.tclTHEN (Tactics.clear [h_hyps_id; i])
         (Tactics.introduction h_hyps_id))
         else
         Proofview.tclTHEN (package i)
         (Proofview.tclTHEN (Tactics.rename_hyp [i, h_hyps_id])
         (Tactics.move_hyp h_hyps_id Logic.MoveLast)) *)
  }
;; *)
(* let coq_st = !st.coq_ref in
  (* *)
  let ((_en, pv) : Proofview.entry * Proofview.proofview) =
    Proofview.init !coq_st.coq_ctx []
    (* [!coq_st.coq_env, _] *)
  in
  Log.debug
    (Printf.sprintf
       "mebi_wrapper.proof_test: is finished => %b"
       (Proofview.finished pv));
  (* *)
  let rel_ctx : EConstr.rel_context = EConstr.rel_context !coq_st.coq_env in
  Log.debug
    (Printf.sprintf
       "mebi_wrapper.proof_test: rel_ctx => \"%s\""
       (Pp.string_of_ppcmds
          (Printer.pr_rel_context
             !coq_st.coq_env
             !coq_st.coq_ctx
             (EConstr.to_rel_context !coq_st.coq_ctx rel_ctx))));
  (* *)
  let named_ctx : EConstr.named_context =
    EConstr.named_context !coq_st.coq_env
  in
  Log.debug
    (Printf.sprintf
       "mebi_wrapper.proof_test: named_ctx => \"%s\""
       (Pp.string_of_ppcmds
          (Printer.pr_named_context
             !coq_st.coq_env
             !coq_st.coq_ctx
             (EConstr.to_named_context !coq_st.coq_ctx named_ctx))));
  (* *)
  { state = st; value = (Proofview.Goal.enter begin fun gl ->
    let hyps = Environ.named_context_val (Proofview.Goal.env gl) in
    
  end
    ) } *)
(*  *)
(* Log.debug
    (Printf.sprintf
       "mebi_wrapper.proof_test: default_goal => %s"
       (Pp.string_of_ppcmds
          (Goal_select.pr_goal_selector
             (Goal_select.get_default_goal_selector ())))); *)
(*  *)
(* let p : Proof.t =
     Proof.start
     ~name:(Names.Id.of_string "test_proof")
     ~poly:false
     !coq_st.coq_ctx
     []
     in
     Log.debug
     (Printf.sprintf "mebi_wrapper.proof_test: is done => %b" (Proof.is_done p)); *)
