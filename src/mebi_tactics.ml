open Logging
(* open Mebi_wrapper *)

(* the prefix *)
let trace_enabled : bool = true
let log_trace (x : string) : unit = if trace_enabled then Log.trace x else ()

let log_tracex (xs : string list) : unit =
  log_trace (Utils.Strfy.list Utils.Strfy.string xs)
;;

(**********************************)
(****** COQ PROOF TACTICS *********)
(**********************************)

let update_proof_by_tactic (pstate : Declare.Proof.t)
  : unit Proofview.tactic -> Declare.Proof.t
  =
  fun x ->
  let new_pstate, is_safe_tactic = Declare.Proof.by x pstate in
  if Bool.not is_safe_tactic
  then Log.warning "mebi_tactics.update_proof_by_tactic, unsafe tactic used";
  new_pstate
;;

let rec update_proof_by_tactics (pstate : Declare.Proof.t)
  : unit Proofview.tactic list -> Declare.Proof.t
  = function
  | [] -> pstate
  | h :: t -> update_proof_by_tactics (update_proof_by_tactic pstate h) t
;;

(*************************)

let do_inversion (h : Rocq_utils.hyp) : unit Proofview.tactic =
  Inv.inv_tac (Context.Named.Declaration.get_id h)
;;

let subst_all () : unit Proofview.tactic = Equality.subst_all ()

let simplify_all ?(gl : Proofview.Goal.t option) () : unit Proofview.tactic =
  let the_fun =
    fun gl ->
    List.fold_left
      (fun acc (h : Rocq_utils.hyp) ->
        Mebi_theories.tactics
          [ acc
          ; Tactics.simpl_in_hyp
              (Context.Named.Declaration.get_id h, Locus.InHyp)
          ])
      Tactics.simpl_in_concl
      (Proofview.Goal.hyps gl)
  in
  match gl with None -> Proofview.Goal.enter the_fun | Some gl -> the_fun gl
;;

let simplify_and_subst_all ?(gl : Proofview.Goal.t option) ()
  : unit Proofview.tactic
  =
  match gl with
  | None -> Mebi_theories.tactics [ simplify_all (); subst_all () ]
  | Some gl -> Mebi_theories.tactics [ simplify_all ~gl (); subst_all () ]
;;

(*************************)

(*************************)

let the_goals : (int, Proofview.Goal.t) Hashtbl.t ref = ref (Hashtbl.create 0)

let reset_the_goals () : unit =
  log_tracex
    [ __FUNCTION__; Printf.sprintf "(was %i)" (Hashtbl.length !the_goals) ];
  Hashtbl.clear !the_goals
;;

let add_goal (g : Proofview.Goal.t) : unit =
  Hashtbl.add !the_goals (Evar.hash (Proofview.Goal.goal g)) g
;;

(* let update_goals () : unit Proofview.tactic mm =
   Log.trace "mebi_tactics.update_goals";
   reset_the_goals ();
   return
   (Proofview.tclBIND
   Proofview.Goal.goals
   (fun (gs : Proofview.Goal.t Proofview.tactic list) ->
   Log.debug
   (Printf.sprintf "mebi_tactics.update_goals: %i goals" (List.length gs));
   Proofview.tclIGNORE
   (List.fold_left
   (fun (acc : unit Proofview.tactic) g ->
   (* Log.debug "mebi_tactics.update_goals list iter"; *)
   Proofview.tclIGNORE
   (Proofview.tclBIND g (fun g ->
   (* Log.debug
   (Printf.sprintf
   "mebi_tactics.update_goals, adding: %s"
   (Rocq_utils.Strfy.goal g)); *)
   add_goal g;
   acc)))
   (Proofview.tclUNIT ())
   gs)))
   ;; *)

(* let get_the_goals () : (int, Proofview.Goal.t) Hashtbl.t Proofview.tactic mm =
   reset_the_goals ();
   let open Mebi_wrapper.Syntax in
   let* upd_g : unit Proofview.tactic = update_goals () in
   return (Proofview.tclTHEN upd_g (Proofview.tclUNIT !the_goals))
   ;; *)

(* let get_hyp (name:string) : (( EConstr.constr
   , EConstr.types
   , EConstr.ERelevance.t )
   Context.Named.Declaration.pt) Proofview.tactic mm =
   return (Proofview.Goal.enter (fun gl ->
   let hyps : EConstr.named_context = Proofview.Goal.hyps gl in
   let hyp = List.find (fun (h) -> Names.Id.equal (Names.Id.of_string name) (Context.Named.Declaration.get_id h)) hyps in

   ))
   ;; *)

(* let pstr_the_goals () : string =
   Log.trace
   (Printf.sprintf
   "mebi_tactics.pstr_the_goals (%i)"
   (Hashtbl.length !the_goals));
   Utils.Strfy.list
   ~force_newline:true
   ~label:"Goals"
   (Utils.Strfy.tuple ~force_newline:true ~indent:1 Utils.Strfy.int Rocq_utils.Strfy.goal)
   (List.of_seq (Hashtbl.to_seq !the_goals))
   ;; *)

let goal_test () : unit Proofview.tactic =
  (* let* env = get_env in *)
  (* let* sigma = get_sigma in *)
  (* return *)
  Proofview.Goal.enter (fun (gl : Proofview.Goal.t) ->
    let env1 : Environ.env = Proofview.Goal.env gl in
    let sigma1 : Evd.evar_map = Proofview.Goal.sigma gl in
    let goal : Evar.t = Proofview.Goal.goal gl in
    let goalstr : string = Rocq_utils.Strfy.evar' env1 sigma1 goal in
    let concl : EConstr.constr = Proofview.Goal.concl gl in
    let concl1str : string = Rocq_utils.Strfy.econstr env1 sigma1 concl in
    Log.debug
      (Printf.sprintf
         "mebi_tactics.goal_test (%s) concl is App\n- %s\n- %s"
         goalstr
         concl1str
         (match EConstr.kind sigma1 concl with
          | App (econstr, econstr_arr) ->
            Printf.sprintf
              "%s => %s"
              (Rocq_utils.Strfy.econstr env1 sigma1 econstr)
              (Utils.Strfy.list
                 (Rocq_utils.Strfy.econstr env1 sigma1)
                 (Array.to_list econstr_arr))
          | _ -> "(Err: Not kind App)"));
    let hyps : EConstr.named_context = Proofview.Goal.hyps gl in
    Log.debug
      (Printf.sprintf
         "mebi_tactics.goal_test (%s) hyps, evars of named context: %s"
         goalstr
         (Utils.Strfy.list
            Rocq_utils.Strfy.evar
            (Evar.Set.to_list (Evd.evars_of_named_context sigma1 hyps))));
    List.iter
      (fun (hyp :
             ( EConstr.constr
               , EConstr.types
               , EConstr.ERelevance.t )
               Context.Named.Declaration.pt) ->
        let name : Names.Id.t = Context.Named.Declaration.get_id hyp in
        let name_str : string = Names.Id.to_string name in
        let rel : EConstr.ERelevance.t =
          Context.Named.Declaration.get_relevance hyp
        in
        let rel_bool : bool = EConstr.ERelevance.is_irrelevant sigma1 rel in
        let tys : EConstr.types = Context.Named.Declaration.get_type hyp in
        let tys_str : string =
          match EConstr.kind_of_type sigma1 tys with
          | AtomicType (ty, ty_arr) ->
            Printf.sprintf
              "%s => %s"
              (Rocq_utils.Strfy.econstr env1 sigma1 ty)
              (Utils.Strfy.list
                 (Rocq_utils.Strfy.econstr env1 sigma1)
                 (Array.to_list ty_arr))
          | _ -> "(Err: Not kind AtomicType)"
        in
        (* Utils.Strfy.list (Strfy.evar) (Evar.Set.to_list (Evd.evars_of_named_context sigma1 hyps)) *)
        Log.debug
          (Printf.sprintf
             "mebi_tactics.goal_test (%s) hyp is %s"
             goalstr
             (match hyp with
              | Context.Named.Declaration.LocalAssum (_, _) ->
                Printf.sprintf
                  "LocalAssum:\n- name: %s\n- is irrelevant: %b\n- %s"
                  name_str
                  rel_bool
                  tys_str
              | Context.Named.Declaration.LocalDef (_, econstr, _) ->
                Printf.sprintf
                  "LocalDef:\n- name: %s\n- is irrelevant: %b\n- %s\n- (%s)"
                  name_str
                  rel_bool
                  tys_str
                  (Rocq_utils.Strfy.econstr env1 sigma1 econstr))))
      hyps;
    Proofview.tclUNIT ())
;;

(****************************************************************************)

let apply ?(gl : Proofview.Goal.t option) (c : EConstr.t)
  : unit Proofview.tactic
  =
  let the_fun = fun gl -> Tactics.apply c in
  match gl with None -> Proofview.Goal.enter the_fun | Some gl -> the_fun gl
;;

let eapply ?(gl : Proofview.Goal.t option) (c : EConstr.t)
  : unit Proofview.tactic
  =
  let the_fun = fun gl -> Tactics.eapply c in
  match gl with None -> Proofview.Goal.enter the_fun | Some gl -> the_fun gl
;;

(****************************************************************************)

let unfold_econstr (gl : Proofview.Goal.t) : EConstr.t -> unit Proofview.tactic
  = function
  | x ->
    let sigma = Proofview.Goal.sigma gl in
    let y = Rocq_convert.econstr_to_constr sigma x in
    (match Constr.kind y with
     | Const (c, _) ->
       Log.debug "mebi_tactics.unfold_econstr, const";
       Tactics.unfold_constr (Names.GlobRef.ConstRef c)
     | _ ->
       Log.warning "mebi_tactics.unfold_econstr -- not Constr!";
       Proofview.tclUNIT ())
;;

let unfold_constrexpr (gl : Proofview.Goal.t)
  : Constrexpr.constr_expr -> unit Proofview.tactic
  = function
  | x ->
    let env = Proofview.Goal.env gl in
    let sigma = Proofview.Goal.sigma gl in
    let sigma, y = Rocq_convert.constrexpr_to_econstr env sigma x in
    let z = Rocq_convert.econstr_to_constr sigma y in
    (match Constr.kind z with
     | Const (c, _) ->
       Log.debug "mebi_tactics.unfold_constrexpr, const";
       Tactics.unfold_constr (Names.GlobRef.ConstRef c)
     | _ ->
       Log.warning "mebi_tactics.unfold_constrexpr -- not Constr!";
       Proofview.tclUNIT ())
;;

let rec unfold_constrexpr_list (gl : Proofview.Goal.t)
  : Constrexpr.constr_expr list -> unit Proofview.tactic
  = function
  | [] -> Proofview.tclUNIT ()
  | h :: [] -> unfold_constrexpr gl h
  | h :: t ->
    Mebi_theories.tactics
      [ unfold_constrexpr gl h; unfold_constrexpr_list gl t ]
;;

let cofix (gl : Proofview.Goal.t) : unit Proofview.tactic =
  Tactics.cofix (Mebi_theories.new_cofix_name gl)
;;

let intros_all () : unit Proofview.tactic = Tactics.intros

let intro_of_string (gl : Proofview.Goal.t) (s : string) : unit Proofview.tactic
  =
  Tactics.introduction (Mebi_theories.new_name_of_string gl s)
;;

(*
   type unfold_kind =
   | Gr of Names.GlobRef.t
   | Id of Names.Id.t
   (* | Ln of Libnames.qualid *)
   | Ex of EConstr.t
   | Ce of Constrexpr.constr_expr

   let unfold ?(pre : 'a Proofview.tactic option = None) ()
   : unfold_kind -> unit Proofview.tactic mm
   = function
   | Gr x ->
   Log.debug "mebi_tactics.unfold Gr _";
   return (seq pre (Tactics.unfold_constr x))
   | Id x ->
   Log.debug "mebi_tactics.unfold Id _";
   return (seq pre (Tactics.unfold_body x))
   | Ex x ->
   Log.debug "mebi_tactics.unfold Ex _";
   let open Mebi_wrapper.Syntax in
   let* _ = Mebi_wrapper.debug_term_kind x in
   let* _ = Mebi_wrapper.debug_term_constr_kind x in
   (* return (Proofview.tclUNIT ()) *)
   let* y = econstr_to_constr x in
   (match Constr.kind y with
   | App (z, _) ->
   Log.debug "mebi_tactics.unfold Ex _ App";
   return (seq pre (Proofview.tclUNIT ()))
   (* return (Tactics.unfold_body y) *)
   | _ ->
   Log.debug "mebi_tactics.unfold Ex _ Other";
   return (seq pre (Proofview.tclUNIT ())))
   | Ce x ->
   Log.debug "mebi_tactics.unfold Ce _";
   let open Mebi_wrapper.Syntax in
   let* y = constrexpr_to_econstr x in
   let* _ = Mebi_wrapper.debug_term_kind y in
   let* _ = Mebi_wrapper.debug_term_constr_kind y in
   let* z = econstr_to_constr y in
   (match Constr.kind z with
   | Const (z, _) ->
   Log.debug "mebi_tactics.unfold Ce _ Const";
   return (Tactics.unfold_constr (Names.GlobRef.ConstRef z))
   | _ ->
   Log.debug "mebi_tactics.unfold Ce _ Other";
   return (seq pre (Proofview.tclUNIT ())))
   ;; *)

(* let* z = econstr_to_constr y in
   (match Constr.kind z with
   | App (z, _) ->
   Log.debug "mebi_tactics.unfold Ce _ App";
   return (Proofview.tclUNIT ())
   (* return (Tactics.unfold_body x) *)
   | _ ->
   Log.debug "mebi_tactics.unfold Ce _ Other";
   return (Proofview.tclUNIT ())) *)

(* | Ln l ->
   Log.debug "mebi_tactics.unfold Ln _";
   Tactics.unfold_constr (Mebi_utils.ref_to_glob l) *)

(* | Ce e -> Mebi_wrapper.constr *)

(*************************************************)

(* let unknown_counter = ref 0

   let new_auto_name ?(prefix : string = "H") () : string =
   let s = Printf.sprintf "%s%i" prefix !unknown_counter in
   unknown_counter := !unknown_counter + 1;
   s
   ;;

   let handle_name : string option -> string = function
   | None -> new_auto_name ()
   | Some n -> new_auto_name ~prefix:n ()
   ;;

   type id_kind =
   | Id of Names.Id.t
   | Str of string
   | Unk of unit

   let intro ?(pre : 'a Proofview.tactic option = None) ()
   : id_kind -> unit Proofview.tactic
   = function
   | Id i ->
   Log.debug "mebi_tactics.intro Id _";
   seq pre (Tactics.introduction i)
   | Str s ->
   Log.debug "mebi_tactics.intro Str _";
   let i = Names.Id.of_string (handle_name (Some s)) in
   seq pre (Tactics.introduction i)
   | Unk _ ->
   Log.debug "mebi_tactics.intro Unk _";
   seq pre Tactics.intro
   ;;

   let rec intros
   ?(all : bool = false)
   ?(pre : 'a Proofview.tactic option = None)
   ()
   : id_kind list -> unit Proofview.tactic
   = function
   | [] -> seq pre (if all then Tactics.intros else Proofview.tclUNIT ())
   | h :: t -> seq pre (Proofview.tclTHEN (intro () h) (intros ~all () t))
   ;; *)
