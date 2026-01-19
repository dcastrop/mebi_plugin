
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

val get_fresh_evar :
  Rocq_utils.evar_source -> Evd.econstr Mebi_wrapper.mm

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

exception ConstructorArgsExpectsArraySize3 of unit

type constructor_args = {
  lhs : Evd.econstr;
  act : Evd.econstr;
  rhs : Evd.econstr;
}

val constructor_args : Evd.econstr array -> constructor_args

val extract_args :
  ?substl:EConstr.Vars.substl ->
  Constr.t ->
  constructor_args Mebi_wrapper.mm

exception Model_info_CouldNotExtractBinding of unit
exception Model_info_CouldNotExtractBindings of unit

val get_constr_app : Constr.t -> Constr.t Rocq_utils.kind_pair

val unpack_constr_args :
  Constr.t Rocq_utils.kind_pair ->
  Constr.t * Constr.t * Constr.t

val _pair_map_to_string : Evd.econstr -> Names.Name.t -> unit

val map_decl_evar_pairs :
  econstr_decl list ->
  EConstr.Vars.substl ->
  (Evd.econstr * Names.Name.t) list

module C : sig
  type key = Constr.t
  type !'a t

  val create : int -> 'a t
  val clear : 'a t -> unit
  val reset : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val remove : 'a t -> key -> unit
  val find : 'a t -> key -> 'a
  val find_opt : 'a t -> key -> 'a option
  val find_all : 'a t -> key -> 'a list
  val replace : 'a t -> key -> 'a -> unit
  val mem : 'a t -> key -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val filter_map_inplace :
    (key -> 'a -> 'a option) -> 'a t -> unit

  val fold :
    (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

  val length : 'a t -> int
  val stats : 'a t -> Hashtbl.statistics
  val to_seq : 'a t -> (key * 'a) Seq.t
  val to_seq_keys : 'a t -> key Seq.t
  val to_seq_values : 'a t -> 'a Seq.t
  val add_seq : 'a t -> (key * 'a) Seq.t -> unit
  val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
  val of_seq : (key * 'a) Seq.t -> 'a t
end

type binding_extractor = Names.Name.t * binding_instructions

and binding_instructions =
  | Undefined
  | Done
  | Arg of {
      root : Constr.t;
      index : int;
      cont : binding_instructions;
    }

val binding_instructions_to_string :
  binding_instructions -> string

exception Mebi_utils_CannotAddAfterDone of unit

val add_instruction :
  binding_instructions ->
  binding_instructions ->
  binding_instructions

val instruction_length : binding_instructions -> int

exception Mebi_utils_CannotFindBindingName of Evd.econstr

val find_name :
  (Evd.econstr * Names.Name.t) list ->
  Evd.econstr ->
  Names.Name.t

val replace_if_shorter :
  binding_extractor C.t ->
  Constr.t ->
  binding_extractor ->
  unit

val extract_constructor_binding :
  Evd.econstr ->
  Constr.t ->
  (Evd.econstr * Names.Name.t) list ->
  binding_extractor C.t

exception
  Mebi_utils_NoConstructorInstructionsExtracted of
    (Evd.econstr
    * Constr.t
    * (Evd.econstr * Names.Name.t) list)

type rocq_one_constructor_bindings =
  | One_No_Bindings
  | One_Use_Bindings of binding_extractor C.t

val extract_binding :
  Evd.econstr ->
  Constr.t ->
  (Evd.econstr * Names.Name.t) list ->
  rocq_one_constructor_bindings

exception Mebi_utils_BindingInstruction_NotApp of Evd.econstr

exception
  Mebi_utils_BindingInstruction_Undefined of
    Evd.econstr * Evd.econstr

exception
  Mebi_utils_BindingInstruction_IndexOutOfBounds of
    Evd.econstr * int

exception
  Mebi_utils_BindingInstruction_NEQ of Evd.econstr * Constr.t

type binding_cmaps = {
  from : binding_extractor C.t option;
  label : binding_extractor C.t option;
  goto : binding_extractor C.t option;
}

val use_no_bindings : binding_cmaps -> bool

val make_cmaps :
  (Evd.econstr * Names.Name.t) list ->
  Evd.econstr * Constr.t ->
  Evd.econstr * Constr.t ->
  Evd.econstr * Constr.t ->
  binding_cmaps

val make_binding_fun :
  binding_cmaps ->
  Model_info.binding_args ->
  Evd.econstr Tactypes.explicit_bindings

val bs_to_string :
  string -> Constr.t * binding_extractor -> unit

val extract_bindings :
  Rocq_utils.ind_constr -> Model_info.rocq_constructor_bindings
