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

val debugstr_unification_problem_list :
  unification_problem list -> string Mebi_wrapper.mm

val debug_unification_problem_list :
  ?prefix:string ->
  unification_problem list ->
  unit Mebi_wrapper.mm

val debugstr_unification_problem_list_list :
  ?prefix:string ->
  unification_problem list list ->
  string Mebi_wrapper.mm

val debug_unification_problem_list_list :
  ?prefix:string ->
  unification_problem list list ->
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

type data = {
  ind_map : Mebi_ind.t Mebi_wrapper.F.t;
  lts_enc : Mebi_setup.Enc.t;
}

val debugstr_data :
  data ->
  unification_problem list list ->
  string Mebi_wrapper.mm

val debug_data :
  string ->
  data ->
  unification_problem list list ->
  unit Mebi_wrapper.mm

val build_next_unification_problems :
  Mebi_constr.t list ->
  Evd.econstr array ->
  unification_problem list Mebi_wrapper.mm

val debug_update_unification_problems :
  unification_problem list ->
  unification_problem list list ->
  'a ->
  unit Mebi_wrapper.mm

val cross_product1 :
  unification_problem list list ->
  unification_problem list ->
  unification_problem list list

val cross_product2 :
  unification_problem list list ->
  unification_problem list ->
  unification_problem list list

val update_unification_problems :
  Mebi_constr.t list ->
  Evd.econstr array ->
  unification_problem list list ->
  unification_problem list list Mebi_wrapper.mm

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

val try_unify_constructor_args :
  ?debug:bool ->
  Evd.econstr ->
  Evd.econstr option ->
  Evd.econstr * Evd.econstr * Evd.econstr ->
  bool Mebi_wrapper.mm

val unify_all_opt :
  ?debug:bool ->
  unification_problem list ->
  Mebi_constr.Tree.t list option Mebi_wrapper.mm

val sandbox_unify_all_opt :
  ?debug:bool ->
  Evd.econstr ->
  unification_problem list ->
  (Evd.econstr * Mebi_constr.Tree.t list) option
  Mebi_wrapper.mm

val build_constrs :
  ?debug:bool ->
  int * Mebi_constr.t list ->
  Evd.econstr ->
  Evd.econstr ->
  Mebi_setup.Enc.t * unification_problem list list ->
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

val extract_args :
  ?substl:EConstr.Vars.substl ->
  Constr.t ->
  (Evd.econstr * Evd.econstr * Evd.econstr) Mebi_wrapper.mm

val normalize_args :
  Evd.econstr * Evd.econstr * Evd.econstr ->
  (Evd.econstr * Evd.econstr * Evd.econstr) Mebi_wrapper.mm

exception
  ConstructorNameNotRecognized of (Evd.econstr * Evd.econstr)

val fail_if_unrecognized_constructor : bool

val handle_unrecognized_ctor_fn :
  Environ.env ->
  Evd.evar_map ->
  Evd.econstr ->
  Evd.econstr ->
  'a ->
  Evd.evar_map * ('b option * 'a) option

exception InductiveKindNotLTS of Mebi_ind.t

val get_ind_constrs_opt :
  Evd.econstr ->
  data ->
  ((Mebi_setup.Enc.t * Rocq_utils.ind_constrs) option
  * Evd.econstr array)
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

val debug_args :
  string ->
  Evd.econstr * Evd.econstr * Evd.econstr ->
  unit Mebi_wrapper.mm

val debug_nextconstrs_none :
  Evd.econstr * Evd.econstr * Evd.econstr ->
  unit Mebi_wrapper.mm

val debug_nextconstrs_some_empty :
  Evd.econstr * Evd.econstr * Evd.econstr ->
  unit Mebi_wrapper.mm

val debug_nextconstrs_some_next :
  Evd.econstr * Evd.econstr * Evd.econstr ->
  unit Mebi_wrapper.mm

val debug_updatesigma_none :
  Evd.econstr -> unit Mebi_wrapper.mm

val debug_updatesigma_some_args :
  Evd.econstr array -> unit Mebi_wrapper.mm

val debug_updatesigma_some_pair :
  Evd.econstr array -> unit Mebi_wrapper.mm

val debug_term : string -> Evd.econstr -> unit Mebi_wrapper.mm

val make_constr_tree :
  int ->
  Mebi_setup.Enc.t ->
  (Mebi_setup.Enc.t * int) Mebi_constr.Tree.tree

val collect_valid_constructors :
  ?action:Evd.econstr option ->
  Evd.econstr ->
  Rocq_utils.ind_constrs ->
  data ->
  Mebi_constr.t list Mebi_wrapper.mm

val check_for_next_constructors :
  int * Mebi_constr.t list ->
  Evd.econstr * Evd.econstr * Evd.econstr ->
  data ->
  unification_problem list list ->
  EConstr.Vars.substl * Rocq_utils.econstr_decls ->
  Mebi_constr.t list Mebi_wrapper.mm

val update_sigma :
  data ->
  unification_problem list list ->
  EConstr.Vars.substl * EConstr.rel_declaration list ->
  unification_problem list list option Mebi_wrapper.mm

val collect_next_constructors :
  Rocq_utils.ind_constrs * Evd.econstr array ->
  data ->
  unification_problem list list ->
  unification_problem list list option Mebi_wrapper.mm
