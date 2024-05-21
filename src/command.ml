open Pp

let rec split_at i l acc =
  if i <= 0 then
    (l, acc)
    else
      match l with
      | [] -> (acc, [])
      | h :: t -> split_at (i-1) t (h :: acc);;

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
     if Sorts.is_prop ar.mind_sort then
       ()
     else
       CErrors.user_err (str "Invalid sort (" ++ Printer.pr_sort sigma ar.mind_sort ++
                         str "). Expecting Prop.")
   | TemplateArity _ ->
       CErrors.user_err (str "Expecting LTS in Prop.")

let get_labels_and_terms env sigma = function
  | [t1; a; t2] ->
    if Context.Rel.Declaration.equal Constr.equal t1 t2 then (a, t1)
    else
      CErrors.user_err (str "Expecting LTS of the form '?Term -> ?Action -> ?Term \
    -> Prop (FIXME: format error)")
  | typs ->
    CErrors.user_err (str "Expecting LTS of the form '?Term -> ?Action -> ?Term \
    -> Prop (FIXME: format error)")

let get_lts_labels_and_terms env sigma mip =
    let open Declarations in
    let i_ctx = mip.mind_arity_ctxt in
    let i_parms, i_idx = split_at mip.mind_nrealdecls i_ctx [] in
    get_labels_and_terms env sigma i_idx

let check_ref_lts env sigma gref =
  let open Names.GlobRef in
  match gref with
  | IndRef i ->
    let (mib, mip) = Inductive.lookup_mind_specif env i in
    arity_is_Prop env sigma mip;
    let (lbl, term) = get_lts_labels_and_terms env sigma mip in
    (Context.Rel.Declaration.get_type lbl, Context.Rel.Declaration.get_type term)
  | _ -> CErrors.user_err (str "Reference '" ++ Printer.pr_global gref ++
                           str "' does not define a LTS.")

(** Builds a LTS from a Term [t : T] and an LTS [P : forall Ts, T -> A -> T -> Prop]
 *  Constraints:
 *  - [T \& A \not\in Ts]
 *)
let lts (iref : Names.GlobRef.t) (tref : Constrexpr.constr_expr_r CAst.t) : unit =
  let env = Global.env () in
  let sigma = Evd.from_env env in
  let (lbls, terms) = check_ref_lts env sigma iref in

  let (sigma, t) = Constrintern.interp_constr_evars env sigma tref in
  let sigma = Typing.check env sigma t (EConstr.of_constr terms) in

  Feedback.msg_notice
    (str "Types of terms: " ++ Printer.pr_constr_env env sigma terms ++ strbrk "");
  Feedback.msg_notice
    (str "Types of labels: " ++ Printer.pr_constr_env env sigma lbls ++ strbrk "");
