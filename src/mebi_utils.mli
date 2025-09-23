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

val get_ind_lts :
  int -> Names.GlobRef.t -> Mebi_ind.t Mebi_wrapper.mm

val encode_econstr :
  EConstr.t -> Mebi_wrapper.E.t Mebi_wrapper.mm

val encode_tref :
  Constrexpr.constr_expr -> Mebi_wrapper.E.t Mebi_wrapper.mm

val encode_ref :
  Libnames.qualid -> Mebi_wrapper.E.t Mebi_wrapper.mm
