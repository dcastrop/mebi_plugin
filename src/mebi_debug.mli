val debug_validconstrs_start :
  Evd.econstr -> unit Mebi_wrapper.mm

val debug_validconstrs_close :
  Evd.econstr ->
  Mebi_unification.Constructors.t ->
  unit Mebi_wrapper.mm

val debug_validconstrs_iter_start :
  unit -> unit Mebi_wrapper.mm

val debug_validconstrs_iter_close :
  unit -> unit Mebi_wrapper.mm

val debug_validconstrs_iter_success_start :
  Evd.econstr ->
  Evd.econstr option ->
  Mebi_unification.constructor_args ->
  unit Mebi_wrapper.mm

val debug_validconstrs_iter_success_close :
  Evd.econstr ->
  Evd.econstr option ->
  Mebi_unification.constructor_args ->
  unit Mebi_wrapper.mm

val debug_nextconstrs_start : unit -> unit Mebi_wrapper.mm

val debug_nextconstrs_close :
  Mebi_unification.Problems.t list ->
  bool option ->
  unit Mebi_wrapper.mm

val debug_nextconstrs_return : unit -> unit Mebi_wrapper.mm
val debug_updtcontext_start : unit -> unit Mebi_wrapper.mm

val debug_updtcontext_close :
  Evd.econstr ->
  Mebi_ind.t option ->
  Mebi_unification.Constructors.t ->
  unit Mebi_wrapper.mm

val debug_updtcontext_close_app :
  Evd.econstr -> unit Mebi_wrapper.mm

val debug_updtcontext_close_app_known :
  Evd.econstr ->
  Mebi_ind.t ->
  Mebi_unification.Constructors.t ->
  unit Mebi_wrapper.mm

val debug_updtcontext_return : unit -> unit Mebi_wrapper.mm
