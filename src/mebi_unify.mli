type constructor_args = {
  lhs : EConstr.t;
  act : EConstr.t;
  rhs : EConstr.t;
}

exception ConstructorArgsExpectsArraySize3 of unit

val constructor_args : EConstr.t array -> constructor_args

val map_problems :
  constructor_args ->
  Mebi_unification.Constructors.t ->
  Mebi_unification.Problems.t

val cross_product :
  Mebi_unification.Problems.t list ->
  Mebi_unification.Problem.t list ->
  Mebi_unification.Problem.t list list

val try_unify_constructor_args :
  ?debug:bool ->
  EConstr.t ->
  EConstr.t option ->
  constructor_args ->
  bool Mebi_wrapper.mm

val subst_of_decl :
  EConstr.Vars.substl ->
  ('a, EConstr.t, 'b) Context.Rel.Declaration.pt ->
  EConstr.t Mebi_wrapper.mm

val mk_ctx_subst :
  EConstr.Vars.substl ->
  ('a, EConstr.t, 'b) Context.Rel.Declaration.pt ->
  EConstr.t Mebi_wrapper.mm

val mk_ctx_substl :
  EConstr.Vars.substl ->
  ('a, EConstr.t, 'b) Context.Rel.Declaration.pt list ->
  EConstr.Vars.substl Mebi_wrapper.mm

val extract_args :
  ?substl:EConstr.Vars.substl ->
  Constr.t ->
  constructor_args Mebi_wrapper.mm

exception
  ConstructorNameNotRecognized of (EConstr.t * EConstr.t)

val check_valid_constructors :
  Rocq_utils.ind_constrs ->
  Mebi_ind.t Mebi_wrapper.F.t ->
  EConstr.t ->
  EConstr.t option ->
  Mebi_wrapper.Enc.t ->
  Mebi_unification.Constructors.t Mebi_wrapper.mm

val check_for_next_constructors :
  int ->
  Mebi_ind.t Mebi_wrapper.F.t ->
  EConstr.t ->
  EConstr.t ->
  Mebi_unification.Constructors.t ->
  (Mebi_wrapper.Enc.t * Mebi_unification.Problems.t list)
  option ->
  Mebi_unification.Constructors.t Mebi_wrapper.mm

val check_updated_ctx :
  Mebi_wrapper.Enc.t ->
  Mebi_unification.Problems.t list ->
  Mebi_ind.t Mebi_wrapper.F.t ->
  EConstr.Vars.substl * Rocq_utils.econstr_decls ->
  (Mebi_wrapper.Enc.t * Mebi_unification.Problems.t list)
  option
  Mebi_wrapper.mm
