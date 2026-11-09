(* See the [.mli]. *)
let lts_shaped (env : Environ.env) (ind : Names.inductive) : bool =
  let mib, mip = Inductive.lookup_mind_specif env ind in
  let option_ref = Rocqlib.lib_ref "core.option.type" in
  mib.mind_nparams = 0
  &&
  match Term.decompose_prod mip.mind_user_arity with
  | [ _; (_, l); _ ], s ->
    Constr.isSort s
    && Sorts.is_prop (Constr.destSort s)
    &&
      (match Constr.kind (fst (Constr.decompose_app l)) with
      | Constr.Ind (i, _) ->
        Names.GlobRef.CanOrd.equal (Names.GlobRef.IndRef i) option_ref
      | _ -> false)
  | _ -> false
;;

(* See the [.mli]. *)
let is_prop (env : Environ.env) (sigma : Evd.evar_map) (t : EConstr.t) : bool =
  try
    match
#if ROCQ_VERSION >= (9, 3, 0)
      Retyping.get_sort_quality_or_set_of env sigma t
#else
      Retyping.get_sort_quality_of env sigma t
#endif
    with
    | UnivGen.QualityOrSet.Qual q -> Sorts.Quality.is_qprop q
    | UnivGen.QualityOrSet.Set -> false
  with
  | _ -> false
;;
