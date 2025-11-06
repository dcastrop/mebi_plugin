val ref_to_glob : Libnames.qualid -> Names.GlobRef.t

val ref_list_to_glob_list :
  Libnames.qualid list -> Names.GlobRef.t list

val assert_mip_arity_is_type :
  Declarations.one_inductive_body -> unit Mebi_wrapper.mm

val assert_mip_arity_is_prop :
  Declarations.one_inductive_body -> unit Mebi_wrapper.mm

val get_lts_labels_and_terms :
  Declarations.mutual_inductive_body ->
  Declarations.one_inductive_body ->
  (Constr.rel_declaration * Constr.rel_declaration)
  Mebi_wrapper.mm

val get_ind_info :
  Names.GlobRef.t -> Mebi_ind.info Mebi_wrapper.mm

val get_name_of_lts :
  Names.GlobRef.t -> Evd.econstr Mebi_wrapper.mm

val get_ind_lts :
  Mebi_setup.Enc.t ->
  Names.GlobRef.t ->
  Mebi_ind.t Mebi_wrapper.mm

val econstr_eq :
  Evd.econstr -> Evd.econstr -> bool Mebi_wrapper.mm

val econstr_normalize :
  Evd.econstr -> Evd.econstr Mebi_wrapper.mm

val econstr_kind :
  Evd.econstr -> Rocq_utils.constr_kind Mebi_wrapper.mm

val econstr_is_evar : Evd.econstr -> bool Mebi_wrapper.mm

val econstr_to_constr :
  ?abort_on_undefined_evars:bool ->
  Evd.econstr ->
  Constr.t Mebi_wrapper.mm

val econstr_to_constr_opt :
  Evd.econstr -> Constr.t option Mebi_wrapper.mm

val constrexpr_to_econstr :
  Constrexpr.constr_expr -> Evd.econstr Mebi_wrapper.mm

val globref_to_econstr :
  Names.GlobRef.t -> Evd.econstr Mebi_wrapper.mm

val type_of_econstr :
  Evd.econstr -> Evd.econstr Mebi_wrapper.mm

val type_of_constrexpr :
  Constrexpr.constr_expr -> Evd.econstr Mebi_wrapper.mm

val new_evar_of : Evd.econstr -> Evd.econstr Mebi_wrapper.mm
val is_none_term : Evd.econstr -> bool Mebi_wrapper.mm

val encode_econstr :
  Evd.econstr -> Mebi_setup.Enc.t Mebi_wrapper.mm

val encode_constrexpr :
  Constrexpr.constr_expr -> Mebi_setup.Enc.t Mebi_wrapper.mm

val encode_ref :
  Libnames.qualid -> Mebi_setup.Enc.t Mebi_wrapper.mm
