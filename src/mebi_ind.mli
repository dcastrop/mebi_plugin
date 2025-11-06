type lts = {
  trm_type : Evd.econstr;
  lbl_type : Evd.econstr;
  constr_transitions : Rocq_utils.ind_constrs;
}

type kind = Type of Evd.econstr option | LTS of lts

type info = {
  name : Evd.econstr;
  constr_names : Names.variable array;
}

type t = { index : Mebi_setup.Enc.t; info : info; kind : kind }

val get_lts_trm_type : t -> Evd.econstr Mebi_wrapper.mm

val get_constr_transitions :
  t -> Rocq_utils.ind_constrs Mebi_wrapper.mm
