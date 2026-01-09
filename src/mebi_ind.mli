type t =
  { enc : Mebi_setup.Enc.t
  ; ind : EConstr.t
  ; kind : kind
  }

and kind =
  | Type of EConstr.t option
  | LTS of lts

and lts =
  { term_type : EConstr.t
  ; label_type : EConstr.t
  ; constructor_types : lts_constructor array
  }

and lts_constructor =
  { name : Names.variable
  ; constructor : Rocq_utils.ind_constr
  }

exception Mebi_ind_ExpectedLTSNotType of t

val get_lts_term_type : t -> EConstr.t
val get_lts_label_type : t -> EConstr.t
val get_lts_constructor_types : t -> lts_constructor array
val get_lts_constructor_names : t -> Names.variable array
val get_lts_constructors : t -> Rocq_utils.ind_constr array

exception
  Mebi_Utils_mip_InconsistentNumConstructors of Declarations.one_inductive_body

val mip_to_lts_constructors
  :  Declarations.one_inductive_body
  -> lts_constructor array
