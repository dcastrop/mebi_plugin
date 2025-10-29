type unification_pair = EConstr.t * EConstr.t

val debugstr_unification_pair : unification_pair -> string Mebi_wrapper.mm

val debug_unification_pair
  :  ?prefix:string
  -> unification_pair
  -> unit Mebi_wrapper.mm

type unification_problem = unification_pair * Mebi_constr.Tree.t

val debugstr_unification_problem : unification_problem -> string Mebi_wrapper.mm

val debug_unification_problem
  :  ?prefix:string
  -> unification_problem
  -> unit Mebi_wrapper.mm

val debug_unification_problem_list
  :  ?prefix:string
  -> unification_problem list
  -> unit Mebi_wrapper.mm

val unify : unification_pair -> bool Mebi_wrapper.mm
val debug_unify : unification_pair -> bool Mebi_wrapper.mm

val unify_all_opt
  :  ?f:(unification_pair -> bool Mebi_wrapper.mm)
  -> unification_problem list
  -> Mebi_constr.Tree.t list option Mebi_wrapper.mm

val sandbox_unify_all_opt
  :  EConstr.t
  -> unification_problem list
  -> (EConstr.t * Mebi_constr.Tree.t list) option Mebi_wrapper.mm

val build_constrs
  :  Mebi_constr.t list
  -> int
  -> EConstr.t
  -> EConstr.t
  -> int * unification_problem list list
  -> Mebi_constr.t list Mebi_wrapper.mm

type data =
  { ind_map : Mebi_ind.t Mebi_setup.F.t
  ; lts_index : int
  ; acc : unification_problem list list
  }

exception ConstructorNameNotRecognized of (EConstr.t * EConstr.t)

val fail_if_unrecognized_constructor : bool

exception InductiveKindNotLTS of Mebi_ind.t

val get_ind_constrs_opt
  :  EConstr.t
  -> data
  -> Rocq_utils.ind_constrs option Mebi_wrapper.mm

val debug_ind_constr : string -> Rocq_utils.ind_constr -> unit Mebi_wrapper.mm
val debug_ind_constrs : string -> Rocq_utils.ind_constrs -> unit Mebi_wrapper.mm

val debug_ind_constrs_opt
  :  string
  -> Rocq_utils.ind_constrs option
  -> unit Mebi_wrapper.mm

val collect_valid_constructors
  :  EConstr.t
  -> Rocq_utils.ind_constrs
  -> data
  -> Mebi_constr.t list Mebi_wrapper.mm
