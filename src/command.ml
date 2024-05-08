open Pp

let rec split_at i l acc =
  if i <= 0 then
    (l, acc)
    else
      match l with
      | [] -> (acc, [])
      | h :: t -> split_at (i-1) (h :: acc) t;;

let test_constr_match t =
  let open Constr in
  match kind t with
  | Var v -> Feedback.msg_notice (strbrk "Is a var")
  | Ind (_, _) -> Feedback.msg_notice (strbrk "An inductive type")
  | Sort _ -> Feedback.msg_notice (strbrk "A sort")
  | _ -> Feedback.msg_notice (strbrk "Not a var")

let check_lts_idx = function
  | [t1; a; t2; prop] ->
    Feedback.msg_notice (strbrk "Correct shape")
  | _ ->
    CErrors.user_err (str "Expecting LTS of the form '?Term -> ?Action -> ?Term \
    -> Prop (FIXME: format error)")


(** Builds a LTS from a Term [t : T] and an LTS [P : forall Ts, T -> A -> T -> Prop]
 *  Constraints:
 *  - [T \& A \not\in Ts]
 *)
let lts (gref : Names.GlobRef.t) : unit =
  let env = Global.env () in
  let sigma = Evd.from_env env in
  match gref with
  | IndRef i ->
    let (mib, mip) = Inductive.lookup_mind_specif env i in
    let i_ctx = mip.mind_arity_ctxt in
    let i_parms, i_idx = split_at mip.mind_nrealdecls i_ctx [] in
    check_lts_idx i_idx;
    let i_types = List.map Context.Rel.Declaration.get_type i_idx in
    let _ = List.map test_constr_match i_types in
    Feedback.msg_notice
      (strbrk "Types of idxs: " ++
       seq (List.map (Printer.pr_constr_env env sigma) i_types));


    Feedback.msg_notice (strbrk "Type " ++ Names.Id.print mip.mind_typename ++
                         strbrk " has " ++ int (Array.length mip.mind_consnames) ++
                         strbrk " constructors and arity context size of " ++
                         int (List.length i_ctx) ++ str "<->" ++
                         int (List.length i_parms) ++ str ", "  ++
                         int (List.length i_idx) ++
                         str " and nrealdecls = " ++
                         int (mip.mind_nrealdecls))
  | _ -> CErrors.user_err (str "Reference '" ++ Printer.pr_global gref ++
                           str "' does not define a LTS.")
