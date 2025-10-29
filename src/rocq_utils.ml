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

(*****************************************************************************)

let type_of_econstr_rel
      ?(substl : EConstr.t list option)
      (t : EConstr.rel_declaration)
  : EConstr.t
  =
  let ty : EConstr.t = Context.Rel.Declaration.get_type t in
  match substl with None -> ty | Some substl -> EConstr.Vars.substl substl ty
;;
