let constrexpr_to_econstr env sigma
  : Constrexpr.constr_expr -> Evd.evar_map * EConstr.t
  =
  Constrintern.interp_constr_evars env sigma
;;

let econstr_to_constr ?(abort_on_undefined_evars : bool = false) sigma
  : EConstr.t -> Constr.t
  =
  EConstr.to_constr ~abort_on_undefined_evars sigma
;;

let econstr_to_constr_opt sigma : EConstr.t -> Constr.t option =
  EConstr.to_constr_opt sigma
;;

let globref_to_econstr env : Names.GlobRef.t -> EConstr.t =
  fun x -> EConstr.of_constr (UnivGen.constr_of_monomorphic_global env x)
;;
