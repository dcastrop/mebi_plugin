val show_unification_debug : bool
val show_extractargs_debug : bool

exception ConstructorArgsExpectsArraySize3 of unit

val constructor_args :
  Evd.econstr array -> Mebi_unification.constructor_args

val map_constr_to_pair :
  Evd.econstr ->
  Evd.econstr ->
  Mebi_unification.Pair.t Mebi_wrapper.mm

val map_constr_to_problem :
  Mebi_unification.constructor_args ->
  Mebi_constr.t ->
  Mebi_unification.Problem.t Mebi_wrapper.mm

val map_problems :
  Mebi_unification.constructor_args ->
  Mebi_unification.Constructors.t ->
  Mebi_unification.Problems.t Mebi_wrapper.mm

val cross_product :
  Mebi_unification.Problems.t list ->
  Mebi_unification.Problem.t list ->
  Mebi_unification.Problems.t list

val try_unify_constructor_args :
  ?debug:bool ->
  Evd.econstr ->
  Evd.econstr option ->
  Mebi_unification.constructor_args ->
  bool Mebi_wrapper.mm

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

val debug_extract_args :
  Constr.t ->
  Mebi_unification.constructor_args ->
  unit Mebi_wrapper.mm

val extract_args :
  ?substl:EConstr.Vars.substl ->
  Constr.t ->
  Mebi_unification.constructor_args Mebi_wrapper.mm

exception
  ConstructorNameNotRecognized of (Evd.econstr * Evd.econstr)

val check_valid_constructors :
  Rocq_utils.ind_constrs ->
  Mebi_ind.t Mebi_wrapper.F.t ->
  Evd.econstr ->
  Evd.econstr option ->
  Mebi_setup.Enc.t ->
  Mebi_unification.Constructors.t Mebi_wrapper.mm

val explore_valid_constructor :
  Mebi_ind.t Mebi_wrapper.F.t ->
  Evd.econstr ->
  Evd.econstr option ->
  Mebi_setup.Enc.t ->
  Mebi_unification.constructor_args ->
  int * Mebi_unification.Constructors.t ->
  EConstr.Vars.substl * Rocq_utils.econstr_decls ->
  Mebi_unification.Constructors.t Mebi_wrapper.mm

val check_updated_ctx :
  Mebi_setup.Enc.t ->
  Mebi_unification.Problems.t list ->
  Mebi_ind.t Mebi_wrapper.F.t ->
  EConstr.Vars.substl * EConstr.rel_declaration list ->
  (Mebi_setup.Enc.t * Mebi_unification.Problems.t list) option
  Mebi_wrapper.mm

val check_for_next_constructors :
  int ->
  Mebi_ind.t Mebi_wrapper.F.t ->
  Evd.econstr ->
  Evd.econstr ->
  Mebi_unification.Constructors.t ->
  (Mebi_setup.Enc.t * Mebi_unification.Problems.t list) option ->
  (Mebi_unification.Constructor_arg.fresh list
  * Mebi_unification.Constructors.t)
  Mebi_wrapper.mm
