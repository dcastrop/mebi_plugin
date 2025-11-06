type data = {
  ind_map : Mebi_ind.t Mebi_wrapper.F.t;
  lts_enc : Mebi_setup.Enc.t;
}

val debug_collect_valid_constructors :
  data ->
  Evd.econstr ->
  Rocq_utils.ind_constrs ->
  unit Mebi_wrapper.mm

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

val extract_args :
  ?substl:EConstr.Vars.substl ->
  Constr.t ->
  (Evd.econstr * Evd.econstr * Evd.econstr) Mebi_wrapper.mm

exception
  ConstructorNameNotRecognized of (Evd.econstr * Evd.econstr)

exception InductiveKindNotLTS of Mebi_ind.t

val get_ind_constrs_opt :
  Evd.econstr ->
  Mebi_ind.t Mebi_wrapper.F.t ->
  (Mebi_setup.Enc.t
  * Rocq_utils.ind_constrs
  * Evd.econstr array)
  option
  Mebi_wrapper.mm

type constructor_args = {
  constructor : Rocq_utils.ind_constr;
  decls : Rocq_utils.econstr_decls;
  substl : EConstr.Vars.substl;
  tree : Mebi_constr.Tree.t;
  lhs : Evd.econstr;
  act : Evd.econstr;
  rhs : Evd.econstr;
}

val constructor_args_to_string :
  Environ.env -> Evd.evar_map -> constructor_args -> string

val constructor_args_list_to_string :
  Environ.env ->
  Evd.evar_map ->
  constructor_args list ->
  string

val is_evar : Evd.evar_map -> Evd.econstr -> bool
val lhs_is_evar : Evd.evar_map -> constructor_args -> bool
val act_is_evar : Evd.evar_map -> constructor_args -> bool
val rhs_is_evar : Evd.evar_map -> constructor_args -> bool

val mk_constructor_args :
  Mebi_setup.Enc.t ->
  int ->
  Rocq_utils.ind_constr ->
  constructor_args Mebi_wrapper.mm

type split_evar = { old : Evd.econstr; fresh : Evd.econstr }

val expand_constructor_args_list :
  data ->
  constructor_args list ->
  constructor_args list ->
  constructor_args list Mebi_wrapper.mm

val update_constructor_args :
  data ->
  constructor_args ->
  constructor_args list Mebi_wrapper.mm

val split_constructor_args :
  data ->
  Rocq_utils.ind_constrs ->
  Evd.econstr array ->
  constructor_args ->
  (split_evar list * constructor_args list) Mebi_wrapper.mm

val mk_init_constructor_args_list :
  Mebi_setup.Enc.t ->
  Rocq_utils.ind_constrs ->
  constructor_args list Mebi_wrapper.mm

val unify : Evd.econstr -> Evd.econstr -> bool Mebi_wrapper.mm

val unify_opt :
  Evd.econstr option -> Evd.econstr -> bool Mebi_wrapper.mm

val does_constructor_apply :
  Evd.econstr ->
  Evd.econstr option ->
  constructor_args ->
  bool Mebi_wrapper.mm

val filter_valid_constructors :
  Evd.econstr ->
  Evd.econstr option ->
  constructor_args list ->
  constructor_args list ->
  constructor_args list Mebi_wrapper.mm

val explore_valid_constructors :
  data ->
  Evd.econstr ->
  Evd.econstr option ->
  constructor_args list ->
  Mebi_constr.t list Mebi_wrapper.mm

val collect_valid_constructors :
  data ->
  Evd.econstr ->
  Rocq_utils.ind_constrs ->
  Mebi_constr.t list Mebi_wrapper.mm
