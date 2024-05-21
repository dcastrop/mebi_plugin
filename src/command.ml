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


let arity_is_Prop env sigma a =
  let open Declarations in
  match a with
  | RegularArity ar ->
    if Sorts.is_prop ar.mind_sort then
      ()
    else
      CErrors.user_err (str "Invalid sort (" ++ Printer.pr_sort sigma ar.mind_sort ++
                        str "). Expecting Prop.")
  | TemplateArity _ ->
      CErrors.user_err (str "Expecting LTS in Prop.")

let check_lts_labels_and_terms env sigma = function
  | [t1; a; t2] ->
    if Context.Rel.Declaration.equal Constr.equal t1 t2 then (a, t1)
    else
      CErrors.user_err (str "Expecting LTS of the form '?Term -> ?Action -> ?Term \
    -> Prop (FIXME: format error)")
  | typs ->
    CErrors.user_err (str "Expecting LTS of the form '?Term -> ?Action -> ?Term \
    -> Prop (FIXME: format error)")


(** Builds a LTS from a Term [t : T] and an LTS [P : forall Ts, T -> A -> T -> Prop]
 *  Constraints:
 *  - [T \& A \not\in Ts]
 *)
let lts (iref : Names.GlobRef.t) : unit =
  let env = Global.env () in
  let sigma = Evd.from_env env in
  match iref with
  | IndRef i ->
    let (mib, mip) = Inductive.lookup_mind_specif env i in
    let i_ctx = mip.mind_arity_ctxt in
    let i_ar = mip.mind_arity in
    arity_is_Prop env sigma i_ar;
    let i_parms, i_idx = split_at mip.mind_nrealdecls i_ctx [] in
    let (lbls, terms) = check_lts_labels_and_terms env sigma i_idx in

    (* let i_types = List.map Context.Rel.Declaration.get_type i_idx in *)
    (* let _ = List.map test_constr_match i_types in *)

    Feedback.msg_notice
      (str "Types of terms: " ++
       Printer.pr_constr_env env sigma (Context.Rel.Declaration.get_type terms)++ strbrk "");

    Feedback.msg_notice
      (str "Types of labels: " ++  Printer.pr_rel_decl env sigma lbls ++ strbrk "");

    Feedback.msg_notice (strbrk "Type " ++ Names.Id.print mip.mind_typename ++
                         strbrk " has " ++ int (Array.length mip.mind_consnames) ++
                         strbrk " constructors and arity context size of " ++
                         int (List.length i_ctx) ++ str "<->" ++
                         int (List.length i_parms) ++ str ", "  ++
                         int (List.length i_idx) ++
                         str " and nrealdecls = " ++
                         int (mip.mind_nrealdecls))
  | _ -> CErrors.user_err (str "Reference '" ++ Printer.pr_global iref ++
                           str "' does not define a LTS.")
