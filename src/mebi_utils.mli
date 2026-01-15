val ref_to_glob : Libnames.qualid -> Names.GlobRef.t

val ref_list_to_glob_list :
  Libnames.qualid list -> Names.GlobRef.t list

val get_lts_ind_mind :
  Names.GlobRef.t ->
  (Names.inductive * Declarations.mind_specif) Mebi_wrapper.mm

val assert_mip_arity_is_type :
  Declarations.one_inductive_body -> unit Mebi_wrapper.mm

val get_lts_ind_type_mind :
  Names.GlobRef.t ->
  (Names.inductive * Declarations.mind_specif) Mebi_wrapper.mm

val assert_mip_arity_is_prop :
  Declarations.one_inductive_body -> unit Mebi_wrapper.mm

val get_lts_ind_prop_mind :
  Names.GlobRef.t ->
  (Names.inductive * Declarations.mind_specif) Mebi_wrapper.mm

val get_lts_labels_and_terms :
  Declarations.mutual_inductive_body ->
  Declarations.one_inductive_body ->
  (Constr.rel_declaration * Constr.rel_declaration)
  Mebi_wrapper.mm

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
  Evd.econstr -> Rocq_utils.econstr_kind Mebi_wrapper.mm

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

val is_theory_term :
  (unit -> Evd.econstr) -> Evd.econstr -> bool Mebi_wrapper.mm

val is_none_term : Evd.econstr -> bool Mebi_wrapper.mm
val is_some_term : Evd.econstr -> bool Mebi_wrapper.mm

val get_theory_term_enc_opt :
  (Evd.econstr -> bool Mebi_wrapper.mm) ->
  Mebi_setup.Enc.t option Mebi_wrapper.mm

val get_none_enc_opt :
  unit -> Mebi_setup.Enc.t option Mebi_wrapper.mm

val get_some_enc_opt :
  unit -> Mebi_setup.Enc.t option Mebi_wrapper.mm

val try_get_theory_term_enc :
  (Evd.econstr -> bool Mebi_wrapper.mm) ->
  Evd.econstr ->
  Mebi_setup.Enc.t option

val try_get_none_enc_opt :
  Evd.econstr -> Mebi_setup.Enc.t option

val try_get_some_enc_opt :
  Evd.econstr -> Mebi_setup.Enc.t option

val encode_econstr :
  Evd.econstr -> Mebi_setup.Enc.t Mebi_wrapper.mm

val encode_constrexpr :
  Constrexpr.constr_expr -> Mebi_setup.Enc.t Mebi_wrapper.mm

val encode_ref :
  Libnames.qualid -> Mebi_setup.Enc.t Mebi_wrapper.mm

val subst_of_decl :
  EConstr.Vars.substl ->
  ('a, Evd.econstr, 'b) Context.Rel.Declaration.pt ->
  Evd.econstr Mebi_wrapper.mm

val mk_ctx_subst :
  EConstr.Vars.substl ->
  ('a, Evd.econstr, 'b) Context.Rel.Declaration.pt ->
  Evd.econstr Mebi_wrapper.mm

val mk_ctx_substl :
  EConstr.Vars.substl ->
  ('a, Evd.econstr, 'b) Context.Rel.Declaration.pt list ->
  EConstr.Vars.substl Mebi_wrapper.mm

exception Model_info_CouldNotExtractBinding of unit
exception Model_info_CouldNotExtractBindings of unit

val get_constr_app : Constr.t -> Constr.t Rocq_utils.kind_pair

val unpack_constr_args :
  Constr.t Rocq_utils.kind_pair ->
  Constr.t * Constr.t * Constr.t

val resolve_bindings :
  Evd.econstr Tactypes.explicit_bindings option list ->
  Evd.econstr Tactypes.explicit_bindings

val extract_bindings :
  Rocq_utils.ind_constr -> Model_info.rocq_constructor_bindings
