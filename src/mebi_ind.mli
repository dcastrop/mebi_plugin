type lts = {
  trm_type : EConstr.t;
  lbl_type : EConstr.t;
  constr_transitions : (Constr.rel_context * Constr.t) array;
}

type kind = Type of EConstr.t option | LTS of lts

type info = {
  name : EConstr.t;
  constr_names : Names.Id.t array;
}

type t = { index : int; info : info; kind : kind }

val get_lts_trm_type : t -> EConstr.t Mebi_wrapper.mm

val get_constr_transitions :
  t -> (Constr.rel_context * Constr.t) array Mebi_wrapper.mm
