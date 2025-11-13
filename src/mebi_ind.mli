type lts = {
  trm_type : EConstr.t;
  lbl_type : EConstr.t;
  constr_transitions : Rocq_utils.ind_constrs;
}

type kind = Type of EConstr.t option | LTS of lts

type info = {
  name : EConstr.t;
  constr_names : Names.variable array;
}

type t = { enc : Mebi_setup.Enc.t; info : info; kind : kind }

val get_lts_trm_type : t -> EConstr.t Mebi_wrapper.mm
val get_lts_label_type : t -> EConstr.t Mebi_wrapper.mm

val get_constr_transitions :
  t -> Rocq_utils.ind_constrs Mebi_wrapper.mm
