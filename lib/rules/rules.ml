(* See the [.mli]. S2, doc/s2-linearisation.md. *)

type premise =
  | Step of
      { lts : Names.inductive
      ; src : Constr.t
      ; lbl : Constr.t
      ; tgt : Constr.t
      }
  | Decide of
      { prop : Constr.t
      ; witness : Constr.t
      }

type rule =
  { name : Names.Id.t
  ; ctx : Constr.rel_context
  ; vars : (Names.Name.t * Constr.types) list
  ; premises : premise list
  ; src : Constr.t
  ; lbl : Constr.t
  ; tgt : Constr.t
  }

type lts =
  { ind : Names.inductive
  ; rules : rule list
  }

exception Not_an_lts of Names.inductive
exception Not_decidable of Names.Id.t * Constr.rel_context * Constr.t
exception Decidable_not_loaded

(** [global name] is the constant [name] of stdlib's DecidableClass.

    @raise Decidable_not_loaded if it is not loaded (raised here). *)
let global (name : string) : Names.GlobRef.t =
  try
    Nametab.locate
      (Libnames.qualid_of_string ("Stdlib.Classes.DecidableClass." ^ name))
  with
  | Not_found -> raise Decidable_not_loaded
;;

(** [witness env sigma p] is [Decidable_witness p i], for the instance [i]
    of [Decidable p] that typeclass resolution finds in [env], or [None] if
    it finds none.

    @raise Decidable_not_loaded as {!global} (propagated). *)
let witness (env : Environ.env) (sigma : Evd.evar_map) (p : Constr.t)
  : Constr.t option
  =
  let cls = UnivGen.constr_of_monomorphic_global env (global "Decidable") in
  let wit =
    UnivGen.constr_of_monomorphic_global env (global "Decidable_witness")
  in
  let goal = EConstr.of_constr (Constr.mkApp (cls, [| p |])) in
  match Class_tactics.resolve_one_typeclass env sigma goal with
  | sigma', inst ->
    let inst = EConstr.to_constr sigma' (Evarutil.nf_evar sigma' inst) in
    Some (Constr.mkApp (wit, [| p; inst |]))
  | exception e when CErrors.noncritical e -> None
;;

(** [step_args env t] is [Some (lts, src, lbl, tgt)] if [t] is a step
    [lts src lbl tgt] of an LTS ({!Rocq_utils.lts_shaped}), else [None].
    Raises nothing. *)
let step_args (env : Environ.env) (t : Constr.t)
  : (Names.inductive * Constr.t * Constr.t * Constr.t) option
  =
  match Constr.decompose_app_list t with
  | h, [ src; lbl; tgt ] ->
    (match Constr.kind h with
     | Constr.Ind (i, _) when Rocq_utils.lts_shaped env i ->
       Some (i, src, lbl, tgt)
     | _ -> None)
  | _ -> None
;;

(** [rule env sigma ind spec j name] is constructor [j] (from 0) of [ind],
    named [name], as a rule. Each binder is read in the context of the
    binders before it; a premise is then lifted over the rest, so that
    every term of the rule is over the whole context.

    @raise Not_an_lts if its conclusion is not a step of [ind]
    @raise Not_decidable if a premise is neither a step nor decidable
    @raise Decidable_not_loaded (all raised here, or by {!witness}) *)
let rule
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (ind : Names.inductive)
      (spec :
        Declarations.mutual_inductive_body * Declarations.one_inductive_body)
      (j : int)
      (name : Names.Id.t)
  : rule
  =
  let ty =
    Inductive.type_of_constructor ((ind, j + 1), UVars.Instance.empty) spec
  in
  let ctx, concl = Term.decompose_prod_decls ty in
  let n = List.length ctx in
  let vars, premises, _, _ =
    List.fold_left
      (fun (vars, premises, i, env_i) decl ->
        let t = Context.Rel.Declaration.get_type decl in
        (* [t] is under the [i] binders before it; the rule's terms are
           under all [n] *)
        let lift = Vars.lift (n - i) in
        let vars, premises =
          if Bool.not (Premise_search.is_prop env_i sigma (EConstr.of_constr t))
          then (Context.Rel.Declaration.get_name decl, t) :: vars, premises
          else (
            match step_args env_i t with
            | Some (lts, src, lbl, tgt) ->
              ( vars
              , Step { lts; src = lift src; lbl = lift lbl; tgt = lift tgt }
                :: premises )
            | None ->
              (match witness env_i sigma t with
               | Some w ->
                 vars, Decide { prop = lift t; witness = lift w } :: premises
               | None -> raise (Not_decidable (name, ctx, lift t))))
        in
        vars, premises, i + 1, Environ.push_rel decl env_i)
      ([], [], 0, env)
      (List.rev ctx)
  in
  match step_args (Environ.push_rel_context ctx env) concl with
  | Some (i, src, lbl, tgt) when Names.Ind.CanOrd.equal i ind ->
    { name
    ; ctx
    ; vars = List.rev vars
    ; premises = List.rev premises
    ; src
    ; lbl
    ; tgt
    }
  | _ -> raise (Not_an_lts ind)
;;

(* See the [.mli]. *)
let of_lts (env : Environ.env) (sigma : Evd.evar_map) (root : Names.inductive)
  : lts list
  =
  let seen : lts list ref = ref [] in
  let rec visit (ind : Names.inductive) : unit =
    if Bool.not (List.exists (fun l -> Names.Ind.CanOrd.equal l.ind ind) !seen)
    then (
      if Bool.not (Rocq_utils.lts_shaped env ind) then raise (Not_an_lts ind);
      let ((_, mip) as spec) = Inductive.lookup_mind_specif env ind in
      let rules =
        Array.to_list
          (Array.mapi (rule env sigma ind spec) mip.Declarations.mind_consnames)
      in
      seen := !seen @ [ { ind; rules } ];
      List.iter
        (fun (r : rule) ->
          List.iter
            (function Step { lts; _ } -> visit lts | Decide _ -> ())
            r.premises)
        rules)
  in
  visit root;
  !seen
;;

(* See the [.mli]. *)
let pr (env : Environ.env) (sigma : Evd.evar_map) (ltss : lts list) : Pp.t =
  let open Pp in
  let pr_rule (r : rule) : Pp.t =
    let env_r = Environ.push_rel_context r.ctx env in
    let pr_t (t : Constr.t) = Printer.pr_constr_env env_r sigma t in
    let pr_premise = function
      | Step { lts; src; lbl; tgt } ->
        pr_t
          (Constr.mkApp
             (Constr.mkIndU (lts, UVars.Instance.empty), [| src; lbl; tgt |]))
      | Decide { prop; _ } -> pr_t prop ++ str "   [Decidable]"
    in
    let vars = prlist_with_sep spc (fun (x, _) -> Names.Name.print x) r.vars in
    v
      2
      (h
         (Names.Id.print r.name
          ++ (if List.is_empty r.vars then mt () else str " " ++ vars)
          ++ str " :")
       ++ fnl ()
       ++ prlist (fun p -> str "  " ++ pr_premise p ++ fnl ()) r.premises
       ++ str "⊢ "
       ++ pr_t r.src
       ++ str "  --"
       ++ pr_t r.lbl
       ++ str "-->  "
       ++ pr_t r.tgt)
  in
  let pr_lts (l : lts) : Pp.t =
    let ty, _ =
      Typeops.type_of_global_in_context env (Names.GlobRef.IndRef l.ind)
    in
    Printer.pr_global (Names.GlobRef.IndRef l.ind)
    ++ str " : "
    ++ Printer.pr_constr_env env sigma ty
    ++ fnl ()
    ++ prlist_with_sep fnl (fun r -> str "  " ++ pr_rule r) l.rules
  in
  prlist_with_sep (fun () -> fnl () ++ fnl ()) pr_lts ltss
;;
