val debug_problems
  :  Environ.env
  -> Evd.evar_map
  -> Mebi_unification.Problems.t
  -> unit

val debug_problems_mm : Mebi_unification.Problems.t -> unit Mebi_wrapper.mm

val debug_problems_list
  :  Environ.env
  -> Evd.evar_map
  -> Mebi_unification.Problems.t list
  -> unit

val debug_problems_list_mm
  :  Mebi_unification.Problems.t list
  -> unit Mebi_wrapper.mm

val debug_constructors
  :  Environ.env
  -> Evd.evar_map
  -> Mebi_unification.Constructors.t
  -> unit

val debug_constructors_mm
  :  Mebi_unification.Constructors.t
  -> unit Mebi_wrapper.mm

val debug_validconstrs_start : EConstr.t -> unit Mebi_wrapper.mm

val debug_validconstrs_close
  :  EConstr.t
  -> Mebi_unification.Constructors.t
  -> unit Mebi_wrapper.mm

val debug_validconstrs_iter_start
  :  int
  -> Mebi_unification.Constructors.t
  -> unit Mebi_wrapper.mm

val debug_validconstrs_iter_close
  :  int
  -> Mebi_unification.Constructors.t
  -> unit Mebi_wrapper.mm

val debug_validconstrs_iter_success_start
  :  EConstr.t
  -> EConstr.t option
  -> Mebi_unification.constructor_args
  -> unit Mebi_wrapper.mm

val debug_validconstrs_iter_success_close
  :  EConstr.t
  -> EConstr.t option
  -> Mebi_unification.constructor_args
  -> unit Mebi_wrapper.mm

val debug_nextconstrs_start : unit -> unit Mebi_wrapper.mm

val debug_nextconstrs_close
  :  Mebi_unification.Problems.t list
  -> bool option
  -> Mebi_unification.Constructors.t
  -> unit Mebi_wrapper.mm

val debug_nextconstrs_return : unit -> unit Mebi_wrapper.mm
val debug_updtcontext_start : unit -> unit Mebi_wrapper.mm

val debug_updtcontext_close
  :  EConstr.t
  -> Mebi_ind.t option
  -> Mebi_unification.Constructors.t
  -> unit Mebi_wrapper.mm

val debug_updtcontext_close_app : EConstr.t -> unit Mebi_wrapper.mm

val debug_updtcontext_close_app_known
  :  EConstr.t
  -> Mebi_ind.t
  -> Mebi_unification.Constructors.t
  -> unit Mebi_wrapper.mm

val debug_updtcontext_return : unit -> unit Mebi_wrapper.mm
