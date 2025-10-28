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
