type econstr_decl = EConstr.rel_declaration

val ref_to_glob : Libnames.qualid -> Names.GlobRef.t

val ref_list_to_glob_list :
  Libnames.qualid list -> Names.GlobRef.t list

module Strfy : sig
  val fname : Names.variable Utils.Strfy.to_string
  val fenc : Mebi_setup.Enc.t Utils.Strfy.to_string
  val constr : Constr.t -> string
  val fconstr : Constr.t Utils.Strfy.to_string
  val econstr : Evd.econstr -> string
  val feconstr : Evd.econstr Utils.Strfy.to_string
  val ind_constr : Rocq_utils.ind_constr -> string

  val f_ind_constr :
    Rocq_utils.ind_constr Utils.Strfy.to_string

  val ind_constrs : Rocq_utils.ind_constr array -> string

  val f_ind_constrs :
    Rocq_utils.ind_constr array Utils.Strfy.to_string

  val constrkind :
    ?args:Utils.Strfy.style_args -> Constr.t -> string

  val fconstrkind : Constr.t Utils.Strfy.to_string
  val encode : Evd.econstr -> string
  val fencode : Evd.econstr Utils.Strfy.to_string
  val decode : Mebi_setup.Enc.t -> string
  val fdecode : Mebi_setup.Enc.t Utils.Strfy.to_string
  val econstr_rel_decl : econstr_decl -> string
  val feconstr_rel_decl : econstr_decl Utils.Strfy.to_string

  val rocq_constructor_to_string :
    ?args:Utils.Strfy.style_args ->
    Rocq_bindings.constructor ->
    string
end

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
  Mebi_setup.Enc.t Rocq_ind.t Mebi_wrapper.mm

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

val get_fresh_evar :
  Rocq_utils.evar_source -> Evd.econstr Mebi_wrapper.mm

val mk_ctx_substl :
  EConstr.Vars.substl ->
  ('a, Evd.econstr, 'b) Context.Rel.Declaration.pt list ->
  EConstr.Vars.substl Mebi_wrapper.mm

val extract_args :
  ?substl:EConstr.Vars.substl ->
  Constr.t ->
  Rocq_utils.constructor_args Mebi_wrapper.mm
