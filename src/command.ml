open Pp

let lts (gref : Names.GlobRef.t) : unit =
  let env = Global.env () in
  (* let sigma = Evd.from_env env in *)
  match gref with
  | IndRef i ->
    let (mib, mip) = Inductive.lookup_mind_specif env i in
    let i_ctx = mip.mind_arity_ctxt in
    Feedback.msg_notice (strbrk "Type " ++ Names.Id.print mip.mind_typename ++
                         strbrk " has " ++ int (Array.length mip.mind_consnames) ++
                         strbrk " constructors and arity context size of " ++
                         int (List.length i_ctx) ++ str " and nrealdecls = " ++
                         int (mip.mind_nrealdecls))
  | _ -> CErrors.user_err (str "Reference '" ++ Printer.pr_global gref ++
                           str "' does not define a LTS.")
