type lts = {
  trm_type : Evd.econstr;
  lbl_type : Evd.econstr;
  constr_transitions : (Constr.rel_context * Constr.t) array;
}

type kind = Type of Evd.econstr option | LTS of lts

type info = {
  name : Evd.econstr;
  constr_names : Names.variable array;
}

type t = { index : int; info : info; kind : kind }

val get_lts_trm_type : t -> Evd.econstr Mebi_wrapper.mm

val get_constr_transitions :
  t -> (Constr.rel_context * Constr.t) array Mebi_wrapper.mm
