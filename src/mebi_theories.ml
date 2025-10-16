open Logging

(* source: https://github.com/rocq-prover/rocq/blob/master/doc/plugin_tutorial/tuto3/src/tuto_tactic.ml *)

let constants : EConstr.t list ref = ref ([] : EConstr.t list)

(* let find_reference (path : string list) (id : string) : Names.GlobRef.t =
   let path = Names.DirPath.make (List.rev_map Names.Id.of_string path) in
   let fp = Libnames.make_path path (Names.Id.of_string id) in
   Nametab.global_of_path fp*)
let find_reference = Coqlib.find_reference [@ocaml.warning "-3"]

(****************************************************************************)

(* NOTE: for handling *)

(****************************************************************************)

(* This is a pattern to collect terms from the Coq memory of valid terms
   and proofs.  This pattern extends all the way to the definition of function
   c_U *)
let collect_bisimilarity_theories () : EConstr.t list =
  Log.trace "mebi_theories.collect_bisimilarity_theories";
  match !constants with
  | [] ->
    Log.debug "mebi_theories.collect_bisimilarity_theories, mapping constants";
    (* TODO: why does error "Not_found" occur if line below is removed? *)
    let new_constants : EConstr.t list =
      List.map
        (fun (x : Names.GlobRef.t) ->
          (* Mebi_wrapper.globref_to_econstr x *)
          EConstr.of_constr
            (UnivGen.constr_of_monomorphic_global (Global.env ()) x))
        [ find_reference "MEBI" [ "MEBI"; "Bisimilarity" ] "LTS"
        ; find_reference "MEBI" [ "MEBI"; "Bisimilarity" ] "tau"
        ; find_reference "MEBI" [ "MEBI"; "Bisimilarity" ] "silent"
        ; find_reference "MEBI" [ "MEBI"; "Bisimilarity" ] "silent1"
        ; find_reference "MEBI" [ "MEBI"; "Bisimilarity" ] "weak"
        ; find_reference "MEBI" [ "MEBI"; "Bisimilarity" ] "wk_some"
        ; find_reference "MEBI" [ "MEBI"; "Bisimilarity" ] "wk_none"
        ; find_reference "MEBI" [ "MEBI"; "Bisimilarity" ] "simF"
        ; find_reference "MEBI" [ "MEBI"; "Bisimilarity" ] "Pack_sim"
        ; find_reference "MEBI" [ "MEBI"; "Bisimilarity" ] "sim_weak"
        ; find_reference "MEBI" [ "MEBI"; "Bisimilarity" ] "weak_sim"
        ; find_reference "MEBI" [ "MEBI"; "Bisimilarity" ] "In_sim"
        ; find_reference "MEBI" [ "MEBI"; "Bisimilarity" ] "out_sim"
        ; find_reference "MEBI" [ "MEBI"; "Bisimilarity" ] "weak_bisim"
          (* NOTE: if updating to rocq change "Coq" to "Corelib" *)
          (* NOTE: docs say "Coq" should be "Stdlib" for version prior to rocq, but this doesn't work for me *)
        ; find_reference
            "MEBI"
            [ "Coq"; "Relations"; "Relation_Definitions" ]
            "relation"
        ; find_reference
            "MEBI"
            [ "Coq"; "Relations"; "Relation_Operators" ]
            "clos_refl_trans_1n"
        ; find_reference
            "MEBI"
            [ "Coq"; "Relations"; "Relation_Operators" ]
            "rt1n_refl"
        ; find_reference
            "MEBI"
            [ "Coq"; "Relations"; "Relation_Operators" ]
            "rt1n_trans"
        ; find_reference
            "MEBI"
            [ "Coq"; "Relations"; "Relation_Operators" ]
            "clos_trans_1n"
        ]
    in
    constants := new_constants;
    !constants
  | _ -> !constants
;;

let rec indexed_c : int * EConstr.t list -> EConstr.t option = function
  | i, [] -> None
  | 0, h :: _ -> Some h
  | i, _ :: t -> indexed_c (i - 1, t)
;;

let c_LTS () : EConstr.t =
  Log.trace "mebi_theories.c_LTS";
  let cs = collect_bisimilarity_theories () in
  match indexed_c (0, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of Theories.Bisimilarity.LTS"
  | Some c -> c
;;

let c_tau () : EConstr.t =
  Log.trace "mebi_theories.c_tau";
  let cs = collect_bisimilarity_theories () in
  match indexed_c (1, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of Theories.Bisimilarity.tau"
  | Some c -> c
;;

let c_silent () : EConstr.t =
  Log.trace "mebi_theories.c_silent";
  let cs = collect_bisimilarity_theories () in
  match indexed_c (2, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.silent"
  | Some c -> c
;;

let c_silent1 () : EConstr.t =
  Log.trace "mebi_theories.c_silent1";
  let cs = collect_bisimilarity_theories () in
  match indexed_c (3, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.silent1"
  | Some c -> c
;;

let c_weak () : EConstr.t =
  Log.trace "mebi_theories.c_weak";
  let cs = collect_bisimilarity_theories () in
  match indexed_c (4, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.weak"
  | Some c -> c
;;

let c_wk_some () : EConstr.t =
  Log.trace "mebi_theories.c_wk_some";
  let cs = collect_bisimilarity_theories () in
  match indexed_c (5, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.wk_some"
  | Some c -> c
;;

let c_wk_none () : EConstr.t =
  Log.trace "mebi_theories.c_wk_none";
  let cs = collect_bisimilarity_theories () in
  match indexed_c (6, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.wk_none"
  | Some c -> c
;;

let c_simF () : EConstr.t =
  Log.trace "mebi_theories.c_simF";
  let cs = collect_bisimilarity_theories () in
  match indexed_c (7, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.simF"
  | Some c -> c
;;

let c_Pack_sim () : EConstr.t =
  Log.trace "mebi_theories.c_Pack_sim";
  let cs = collect_bisimilarity_theories () in
  match indexed_c (8, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.Pack_sim"
  | Some c -> c
;;

let c_sim_weak () : EConstr.t =
  Log.trace "mebi_theories.c_sim_weak";
  let cs = collect_bisimilarity_theories () in
  match indexed_c (9, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.sim_weak"
  | Some c -> c
;;

let c_weak_sim () : EConstr.t =
  Log.trace "mebi_theories.c_weak_sim";
  let cs = collect_bisimilarity_theories () in
  match indexed_c (10, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.weak_sim"
  | Some c -> c
;;

let c_In_sim () : EConstr.t =
  Log.trace "mebi_theories.c_In_sim";
  let cs = collect_bisimilarity_theories () in
  match indexed_c (11, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.In_sim"
  | Some c -> c
;;

let c_out_sim () : EConstr.t =
  Log.trace "mebi_theories.c_out_sim";
  let cs = collect_bisimilarity_theories () in
  match indexed_c (12, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.out_sim"
  | Some c -> c
;;

let c_weak_bisim () : EConstr.t =
  Log.trace "mebi_theories.c_weak_bisim";
  let cs = collect_bisimilarity_theories () in
  match indexed_c (13, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.weak_bisim"
  | Some c -> c
;;

let c_relations () : EConstr.t =
  Log.trace "mebi_theories.c_relations";
  let cs = collect_bisimilarity_theories () in
  match indexed_c (14, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Relations.Relation_Definitions.relations"
  | Some c -> c
;;

let c_clos_refl_trans_1n () : EConstr.t =
  Log.trace "mebi_theories.c_clos_refl_trans_1n";
  let cs = collect_bisimilarity_theories () in
  match indexed_c (15, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Relations.Relation_Definitions.clos_refl_trans_1n"
  | Some c -> c
;;

let c_rt1n_refl () : EConstr.t =
  Log.trace "mebi_theories.c_rt1n_refl";
  let cs = collect_bisimilarity_theories () in
  match indexed_c (16, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Relations.Relation_Definitions.rt1n_refl"
  | Some c -> c
;;

let c_rt1n_trans () : EConstr.t =
  Log.trace "mebi_theories.c_rt1n_trans";
  let cs = collect_bisimilarity_theories () in
  match indexed_c (17, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Relations.Relation_Definitions.rt1n_trans"
  | Some c -> c
;;

let c_clos_trans_1n () : EConstr.t =
  Log.trace "mebi_theories.c_clos_trans_1n";
  let cs = collect_bisimilarity_theories () in
  match indexed_c (18, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Relations.Relation_Definitions.clos_trans_1n"
  | Some c -> c
;;

(******)

type theory_kind =
  | Theories_LTS
  | Theories_tau
  | Theories_silent
  | Theories_silent1
  | Theories_weak
  | Theories_wk_some
  | Theories_wk_none
  | Theories_simF
  | Theories_Pack_sim
  | Theories_sim_weak
  | Theories_weak_sim
  | Theories_In_sim

let match_theory_kind sigma : EConstr.t -> theory_kind option =
  fun (x : EConstr.t) ->
  let eq : EConstr.t -> bool = Mebi_setup.Eq.econstr sigma x in
  if eq (c_LTS ())
  then Some Theories_LTS
  else if eq (c_tau ())
  then Some Theories_tau
  else if eq (c_silent ())
  then Some Theories_silent
  else if eq (c_silent1 ())
  then Some Theories_silent1
  else if eq (c_weak ())
  then Some Theories_weak
  else if eq (c_wk_some ())
  then Some Theories_wk_some
  else if eq (c_wk_none ())
  then Some Theories_wk_none
  else if eq (c_simF ())
  then Some Theories_simF
  else if eq (c_Pack_sim ())
  then Some Theories_Pack_sim
  else if eq (c_sim_weak ())
  then Some Theories_sim_weak
  else if eq (c_weak_sim ())
  then Some Theories_weak_sim
  else if eq (c_In_sim ())
  then Some Theories_In_sim
  else None
;;

let is_var sigma (x : EConstr.t) : bool =
  EConstr.isRef sigma x && EConstr.isVar sigma x
;;

let need_to_invert sigma (tys : EConstr.t array) : bool =
  Bool.not
    (List.is_empty
       (List.filter (fun ty -> is_var sigma ty) (Array.to_list tys)))
;;

(*****************************************************************************)

let get_hyp_names (gl : Proofview.Goal.t) : Names.Id.Set.t =
  Log.trace "mebi_tactics.get_hyp_names";
  Context.Named.to_vars (Proofview.Goal.hyps gl)
;;

let next_name_of (names : Names.Id.Set.t) : Names.Id.t -> Names.Id.t =
  fun x ->
  Log.trace (Printf.sprintf "mebi_tactics.next_name_of (%s)" (Strfy.name_id x));
  Namegen.next_ident_away x names
;;

let new_name_of_string (gl : Proofview.Goal.t) : string -> Names.Id.t =
  fun x ->
  Log.trace (Printf.sprintf "mebi_tactics.new_name_of_string (%s)" x);
  next_name_of (get_hyp_names gl) (Names.Id.of_string x)
;;

let is_cofix (x : Names.Id.t) : bool =
  Names.Id.equal (Nameops.root_of_id x) (Names.Id.of_string "Cofix")
;;

let new_cofix_name (gl : Proofview.Goal.t) : Names.Id.t =
  new_name_of_string gl "Cofix0"
;;

let new_H_name (gl : Proofview.Goal.t) : Names.Id.t = new_name_of_string gl "H0"

(*****************************************************************************)

let get_proof_from_pstate : Declare.Proof.t -> Proof.t = Declare.Proof.get
let get_partial_proof : Proof.t -> EConstr.t list = Proof.partial_proof

(*****************************************************************************)

(* The following tactic is meant to pack an hypothesis when no other
   data is already packed.

   The main difficulty in defining this tactic is to understand how to
   construct the input expected by apply_in. *)
(* let _package (i : Names.Id.t) : unit Proofview.tactic =
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
;; *)

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
       (Strfy.pp
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
       (Strfy.pp
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
       (Strfy.pp
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
