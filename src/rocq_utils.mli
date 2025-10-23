
type hyp = (EConstr.t, EConstr.t, Evd.erelevance) Context.Named.Declaration.pt


type constr_kind =
  ( EConstr.t
    , EConstr.t
    , Evd.esorts
    , EConstr.EInstance.t
    , Evd.erelevance ) 
    Constr.kind_of_term
    
val type_of_econstr_rel : ?substl:EConstr.t list -> EConstr.rel_declaration -> EConstr.t
