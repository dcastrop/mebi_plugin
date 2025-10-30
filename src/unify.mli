type unification_pair = Evd.econstr * Evd.econstr

val debugstr_unification_pair :
  unification_pair -> string Mebi_wrapper.mm

val debug_unification_pair :
  ?prefix:string -> unification_pair -> unit Mebi_wrapper.mm

type unification_problem =
  unification_pair * Mebi_constr.Tree.t

val debugstr_unification_problem :
  unification_problem -> string Mebi_wrapper.mm

val debug_unification_problem :
  ?prefix:string -> unification_problem -> unit Mebi_wrapper.mm

val debug_unification_problem_list :
  ?prefix:string ->
  unification_problem list ->
  unit Mebi_wrapper.mm

val unify : unification_pair -> bool Mebi_wrapper.mm
val debug_unify : unification_pair -> bool Mebi_wrapper.mm

val do_terms_unify :
  ?f:(unification_pair -> bool Mebi_wrapper.mm) ->
  Evd.econstr ->
  Evd.econstr ->
  bool Mebi_wrapper.mm

val do_terms_unify_opt :
  ?f:(unification_pair -> bool Mebi_wrapper.mm) ->
  Evd.econstr option ->
  Evd.econstr option ->
  bool Mebi_wrapper.mm

val try_unify_constructor_args :
  ?f:(unification_pair -> bool Mebi_wrapper.mm) ->
  Evd.econstr ->
  Evd.econstr option ->
  Evd.econstr * Evd.econstr * Evd.econstr ->
  bool Mebi_wrapper.mm

val unify_all_opt :
  ?f:(unification_pair -> bool Mebi_wrapper.mm) ->
  unification_problem list ->
  Mebi_constr.Tree.t list option Mebi_wrapper.mm

val sandbox_unify_all_opt :
  Evd.econstr ->
  unification_problem list ->
  (Evd.econstr * Mebi_constr.Tree.t list) option
  Mebi_wrapper.mm

val build_constrs :
  Mebi_constr.t list ->
  int ->
  Evd.econstr ->
  Evd.econstr ->
  int * unification_problem list list ->
  Mebi_constr.t list Mebi_wrapper.mm

val mk_ctx_subst :
  ?substl:EConstr.Vars.substl ->
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

type data = {
  ind_map : Mebi_ind.t Mebi_setup.FwdMap.t;
  lts_index : int;
  mutable acc : unification_problem list list;
}

exception
  ConstructorNameNotRecognized of (Evd.econstr * Evd.econstr)

val fail_if_unrecognized_constructor : bool

exception InductiveKindNotLTS of Mebi_ind.t

val get_ind_constrs_opt :
  Evd.econstr ->
  data ->
  Rocq_utils.ind_constrs option Mebi_wrapper.mm

val debug_ind_constr :
  string -> Rocq_utils.ind_constr -> unit Mebi_wrapper.mm

val debug_ind_constrs :
  string -> Rocq_utils.ind_constrs -> unit Mebi_wrapper.mm

val debug_ind_constrs_opt :
  string ->
  Rocq_utils.ind_constrs option ->
  unit Mebi_wrapper.mm

val debug_args :
  string ->
  Evd.econstr * Evd.econstr * Evd.econstr ->
  unit Mebi_wrapper.mm

val collect_valid_constructors :
  ?action:Evd.econstr option ->
  Evd.econstr ->
  Rocq_utils.ind_constrs ->
  data ->
  Mebi_constr.t list Mebi_wrapper.mm

val collect_next_constructors :
  Mebi_constr.t list ->
  Evd.econstr * Evd.econstr * Evd.econstr ->
  data ->
  EConstr.Vars.substl * Rocq_utils.econstr_decls ->
  Mebi_constr.t list Mebi_wrapper.mm

val update_sigma :
  data ->
  EConstr.Vars.substl * EConstr.rel_declaration list ->
  data option Mebi_wrapper.mm
