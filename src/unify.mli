val debug_term : string -> Evd.econstr -> unit Mebi_wrapper.mm

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

type unification_problems = unification_problem list

val debugstr_unification_problems :
  unification_problems -> string Mebi_wrapper.mm

val debug_unification_problems :
  ?prefix:string ->
  unification_problems ->
  unit Mebi_wrapper.mm

val debugstr_unification_problems_list :
  ?prefix:string ->
  unification_problems list ->
  string Mebi_wrapper.mm

val debug_unification_problems_list :
  ?prefix:string ->
  unification_problems list ->
  unit Mebi_wrapper.mm

val debugstr_mebiconstrs :
  Environ.env -> Evd.evar_map -> Mebi_constr.t list -> string

val debug_mebiconstrs :
  string -> Mebi_constr.t list -> unit Mebi_wrapper.mm

val debug_buildconstrs_some :
  Evd.econstr ->
  Evd.econstr ->
  Evd.econstr ->
  Mebi_constr.t list ->
  unit Mebi_wrapper.mm

val debug_buildconstrs_none :
  Evd.econstr ->
  Evd.econstr ->
  Mebi_constr.t list ->
  unit Mebi_wrapper.mm

val debug_unify :
  Environ.env ->
  Evd.evar_map ->
  Evd.econstr ->
  Evd.econstr ->
  unit

val debug_unifyerr :
  Environ.env ->
  Evd.evar_map ->
  Evd.econstr ->
  Evd.econstr ->
  Evd.econstr ->
  Evd.econstr ->
  unit

val unify :
  ?debug:bool -> unification_pair -> bool Mebi_wrapper.mm

type lts_arg = { term : Evd.econstr; need_substl : bool }

val lts_arg : ?need_substl:bool -> Evd.econstr -> lts_arg

type lts_args = { lhs : lts_arg; act : lts_arg; rhs : lts_arg }

val lts_args_of_array :
  ?need_substl:bool -> Evd.econstr array -> lts_args

exception LtsArgTermNeedsSubstl of Evd.econstr

val get_lts_arg_term : lts_arg -> Evd.econstr
val get_lhs : lts_args -> Evd.econstr
val get_act : lts_args -> Evd.econstr
val get_rhs : lts_args -> Evd.econstr

val var_substl :
  EConstr.Vars.substl -> Evd.econstr -> Evd.econstr

val arg_substl : EConstr.Vars.substl -> lts_arg -> lts_arg
val substl_lhs : EConstr.Vars.substl -> lts_args -> lts_args
val substl_act : EConstr.Vars.substl -> lts_args -> lts_args
val substl_rhs : EConstr.Vars.substl -> lts_args -> lts_args
val args_substl : EConstr.Vars.substl -> lts_args -> lts_args
val arg_normalize : lts_arg -> lts_arg Mebi_wrapper.mm
val normalize_lhs : lts_args -> lts_args Mebi_wrapper.mm
val normalize_act : lts_args -> lts_args Mebi_wrapper.mm
val normalize_rhs : lts_args -> lts_args Mebi_wrapper.mm
val args_normalize : lts_args -> lts_args Mebi_wrapper.mm

type data = {
  ind_map : Mebi_ind.t Mebi_setup.FwdMap.t;
  lts_index : int;
}

val debugstr_data :
  data -> unification_problems list -> string Mebi_wrapper.mm

val debug_data :
  string ->
  data ->
  unification_problems list ->
  unit Mebi_wrapper.mm

val debug_update_unification_problems :
  unification_problems ->
  unification_problems list ->
  'a ->
  unit Mebi_wrapper.mm

val cross_product1 :
  unification_problem list list ->
  unification_problem list ->
  unification_problems list

val cross_product2 :
  unification_problem list list ->
  unification_problem list ->
  unification_problems list

val build_next_unification_problems :
  Mebi_constr.t list ->
  lts_args ->
  unification_problems Mebi_wrapper.mm

val update_unification_problems :
  Mebi_constr.t list ->
  lts_args ->
  unification_problems list ->
  unification_problems list Mebi_wrapper.mm

val unify_all_opt :
  ?debug:bool ->
  unification_problems ->
  Mebi_constr.Tree.t list option Mebi_wrapper.mm

val sandbox_unify_all_opt :
  ?debug:bool ->
  Evd.econstr ->
  unification_problems ->
  (Evd.econstr * Mebi_constr.Tree.t list) option
  Mebi_wrapper.mm

val build_constrs :
  ?debug:bool ->
  int * Mebi_constr.t list ->
  Evd.econstr ->
  Evd.econstr ->
  int * unification_problems list ->
  Mebi_constr.t list Mebi_wrapper.mm

val debugstr_substl :
  Environ.env -> Evd.evar_map -> EConstr.Vars.substl -> string

val debug_substl :
  string -> EConstr.Vars.substl -> unit Mebi_wrapper.mm

val debugstr_decls :
  Environ.env ->
  Evd.evar_map ->
  Rocq_utils.econstr_decls ->
  string

val debug_decls :
  string -> Rocq_utils.econstr_decls -> unit Mebi_wrapper.mm

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

val extract_args : Constr.t -> lts_args Mebi_wrapper.mm

exception
  ConstructorNameNotRecognized of (Evd.econstr * Evd.econstr)

val fail_if_unrecognized_constructor : bool

val handle_unrecognized_ctor_fn :
  Environ.env ->
  Evd.evar_map ->
  Evd.econstr ->
  Evd.econstr ->
  Evd.econstr array ->
  Evd.evar_map * ('a option * Evd.econstr array) option

exception InductiveKindNotLTS of Mebi_ind.t

val get_ind_constrs_opt :
  Evd.econstr ->
  data ->
  ((int * Rocq_utils.ind_constrs) option * Evd.econstr array)
  option
  Mebi_wrapper.mm

val debug_ind_constr :
  string -> Rocq_utils.ind_constr -> unit Mebi_wrapper.mm

val debug_ind_constrs :
  string -> Rocq_utils.ind_constrs -> unit Mebi_wrapper.mm

val debug_ind_constrs_opt :
  string ->
  Rocq_utils.ind_constrs option ->
  unit Mebi_wrapper.mm

val debug_validconstrs_begin :
  Rocq_utils.ind_constrs -> unit Mebi_wrapper.mm

val debug_validconstrs_iter :
  int -> Rocq_utils.ind_constr -> unit Mebi_wrapper.mm

val debug_validconstrs_iter_unify :
  int -> Rocq_utils.ind_constr -> bool -> unit Mebi_wrapper.mm

val debugstr_arg :
  Environ.env -> Evd.evar_map -> lts_arg -> string

val debug_args : string -> lts_args -> unit Mebi_wrapper.mm
val debug_nextconstrs_none : lts_args -> unit Mebi_wrapper.mm

val debug_nextconstrs_some_empty :
  lts_args -> unit Mebi_wrapper.mm

val debug_nextconstrs_some_next :
  lts_args -> unit Mebi_wrapper.mm

val debug_updatesigma_none :
  Evd.econstr -> unit Mebi_wrapper.mm

val debug_updatesigma_some_args :
  lts_args -> unit Mebi_wrapper.mm

val debug_updatesigma_some_pair :
  lts_args -> unit Mebi_wrapper.mm

val make_constr_tree :
  int -> int -> (int * int) Mebi_constr.Tree.tree

type substl_maker = unit -> EConstr.Vars.substl Mebi_wrapper.mm

val make_substl_fun :
  Rocq_utils.econstr_decls ->
  unit ->
  EConstr.Vars.substl Mebi_wrapper.mm

val collect_valid_constructors :
  ?action:Evd.econstr option ->
  Evd.econstr ->
  Rocq_utils.ind_constrs ->
  data ->
  Mebi_constr.t list Mebi_wrapper.mm

val check_for_next_constructors :
  int * Mebi_constr.t list ->
  lts_args ->
  data ->
  unification_problems list ->
  EConstr.Vars.substl * Rocq_utils.econstr_decls ->
  Mebi_constr.t list Mebi_wrapper.mm

val update_sigma :
  data ->
  unification_problems list ->
  EConstr.Vars.substl * EConstr.rel_declaration list ->
  unification_problems list option Mebi_wrapper.mm

val collect_next_constructors :
  Rocq_utils.ind_constrs * Evd.econstr array ->
  data ->
  unification_problems list ->
  EConstr.Vars.substl ->
  unification_problems list option Mebi_wrapper.mm
