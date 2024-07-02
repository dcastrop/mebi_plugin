open Pp

let rec split_at i l acc =
  if i <= 0
  then l, acc
  else (
    match l with
    | [] -> acc, []
    | h :: t -> split_at (i - 1) t (h :: acc))
;;

(* let test_constr_match t = *)
(*   let open Constr in *)
(*   match kind t with *)
(*   | Var v -> Feedback.msg_notice (strbrk "Is a var") *)
(*   | Ind (_, _) -> Feedback.msg_notice (strbrk "An inductive type") *)
(*   | Sort _ -> Feedback.msg_notice (strbrk "A sort") *)
(*   | _ -> Feedback.msg_notice (strbrk "Not a var") *)

let arity_is_Prop env sigma mip =
  let open Declarations in
  let a = mip.mind_arity in
  match a with
  | RegularArity ar ->
    if Sorts.is_prop ar.mind_sort
    then ()
    else
      CErrors.user_err
        (str "Invalid sort ("
         ++ Printer.pr_sort sigma ar.mind_sort
         ++ str "). Expecting Prop.")
  | TemplateArity _ -> CErrors.user_err (str "Expecting LTS in Prop.")
;;

let get_labels_and_terms env sigma = function
  | [ t1; a; t2 ] ->
    let open Context.Rel in
    if Declaration.equal Constr.equal t1 t2
    then a, t1
    else
      CErrors.user_err
        (str "Expecting arity \"term -> label -> term -> Prop\". Type mismatch: "
         ++ Printer.pr_rel_decl env sigma t1
         ++ str " <=> "
         ++ Printer.pr_rel_decl env sigma t2
         ++ strbrk ".")
  | _ -> CErrors.user_err (str "Expecting arity \"term -> label -> term -> Prop\".")
;;

let _fresh_typed_name avoid env sigma =
  let nn = Tactics.fresh_id_in_env avoid Namegen.default_type_ident env in
  let nn = Context.nameR nn in
  let sigma, s = Evd.new_sort_variable Evd.univ_rigid sigma in
  let new_type = EConstr.mkSort s in
  let sigma, arg_type = Evarutil.new_evar env sigma new_type in
  sigma, (nn, arg_type)
;;

let get_lts_labels_and_terms env sigma mip =
  let open Declarations in
  let i_ctx = mip.mind_arity_ctxt in
  let i_parms, i_idx = split_at mip.mind_nrealdecls i_ctx [] in
  get_labels_and_terms env sigma i_idx
;;

let check_ref_lts env sigma gref =
  let open Names.GlobRef in
  match gref with
  | IndRef i ->
    let mib, mip = Inductive.lookup_mind_specif env i in
    arity_is_Prop env sigma mip;
    let lbl, term = get_lts_labels_and_terms env sigma mip in
    Context.Rel.Declaration.get_type lbl, Context.Rel.Declaration.get_type term
  | _ ->
    CErrors.user_err
      (str "Reference '" ++ Printer.pr_global gref ++ str "' does not define a LTS.")
;;

let get_constructors env sigma gref =
  let open Names.GlobRef in
  match gref with
  | IndRef i ->
    let _, mip = Inductive.lookup_mind_specif env i in
    mip.mind_consnames, mip.mind_nf_lc
  | _ -> assert false
;;

(** Checks if two terms unify
    TODO: lots of doubts
    - Conversion.CUMUL?
    - Is [w_unify] the best way?
    - ... *)
let m_unify env sigma t0 t1 =
  try
    let sigma = Unification.w_unify env sigma Conversion.CUMUL t0 t1 in
    Some sigma
  with
  | Pretype_errors.PretypeError (_, _, Pretype_errors.CannotUnify _) -> None
;;

(** Generates [LTS term ?act ?term2] for unification *)
let mk_template env sigma lts termL lbl_ty term_ty =
  let sigma, act = Evarutil.new_evar env sigma lbl_ty in
  let sigma, termR = Evarutil.new_evar env sigma term_ty in
  let template = EConstr.mkApp (lts, [| termL; act; termR |]) in
  sigma, template
;;

(** Checks possible transitions for this term: *)
let check_valid_constructor env sigma gref t term_ty lbl_ty transitions =
  let open Names.GlobRef in
  match gref with
  | IndRef i ->
    let mib, _ = Inductive.lookup_mind_specif env i in
    let univ = mib.mind_univ_hyps in
    let lts = EConstr.mkIndU (i, EConstr.EInstance.make univ) in
    Array.fold_left
      (fun (sigma, acc) tm ->
        let sigma, to_unif = mk_template env sigma lts t lbl_ty term_ty in
        match m_unify env sigma to_unif (EConstr.of_constr (snd tm)) with
        | Some sigma -> sigma, acc + 1
        | None -> sigma, acc)
      (sigma, 0)
      transitions
  (* END FIXME *)
  | _ -> assert false
;;

(* TODO: check which are all possible next transitions *)
(* TODO: check following functions/modules: *)
(* [ ] Unification *)
(* [ ] Reductionops.infer_conv *)
(*  *)

(** Builds an LTS from a Term [t : T] and an LTS [P : forall Ts, T -> A -> T -> Prop]

    Constraints:
    - [ T \& A \not\in Ts ]

    Notes:
    - Constructors of [P] are the transitions
    - States are the sets of possible transitions
    - A term [t] is represented by the state of the transitions that can be taken *)
let lts (iref : Names.GlobRef.t) (tref : Constrexpr.constr_expr_r CAst.t) : unit =
  let env = Global.env () in
  let sigma = Evd.from_env env in
  let lbls, terms = check_ref_lts env sigma iref in
  let lbls = EConstr.of_constr lbls in
  let terms = EConstr.of_constr terms in
  let sigma, t = Constrintern.interp_constr_evars env sigma tref in
  let sigma = Typing.check env sigma t terms in
  let c_names, transitions = get_constructors env sigma iref in
  let sigma, constrs = check_valid_constructor env sigma iref t terms lbls transitions in
  Feedback.msg_notice
    (str "Types of terms: " ++ Printer.pr_econstr_env env sigma terms ++ strbrk "");
  Feedback.msg_notice
    (str "Types of labels: " ++ Printer.pr_econstr_env env sigma lbls ++ strbrk "");
  Feedback.msg_notice
    (str "Constructors: " ++ Pp.prvect_with_sep (fun _ -> str ", ") Names.Id.print c_names);
  Feedback.msg_notice
    (str "Transitions: "
     ++ Pp.prvect_with_sep
          (fun _ -> strbrk "\n")
          (fun t -> Printer.pr_constr_env env sigma (snd t))
          transitions);
  Feedback.msg_notice (str "Target matches: " ++ Pp.int constrs)
;;
