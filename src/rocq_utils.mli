type hyp = (EConstr.t, EConstr.t, Evd.erelevance) Context.Named.Declaration.pt

type constr_kind =
  ( EConstr.t
    , EConstr.t
    , Evd.esorts
    , EConstr.EInstance.t
    , Evd.erelevance )
    Constr.kind_of_term

type ind_constr = Constr.rel_context * Constr.t
type ind_constrs = ind_constr array
type econstr_decl = EConstr.rel_declaration
type econstr_decls = econstr_decl list

val type_of_econstr_rel
  :  ?substl:EConstr.t list
  -> EConstr.rel_declaration
  -> EConstr.t
