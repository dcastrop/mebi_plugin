val debug_problems :
  Environ.env -> Mebi_unification.Problems.t -> unit

val debug_problems_mm :
  Mebi_unification.Problems.t -> unit Mebi_wrapper.mm

val debug_problems_list :
  Environ.env -> Mebi_unification.Problems.t list -> unit

val debug_problems_list_mm :
  Mebi_unification.Problems.t list -> unit Mebi_wrapper.mm

val debug_labelled_cross_product :
  Environ.env ->
  Evd.evar_map ->
  Evd.econstr ->
  Evd.econstr ->
  'a ->
  'b ->
  unit

val debug_labelled_cross_product_mm :
  Evd.econstr ->
  Evd.econstr ->
  'a ->
  'b ->
  unit Mebi_wrapper.mm

val debug_constructors :
  Environ.env ->
  Evd.evar_map ->
  Mebi_unification.Constructors.t ->
  unit

val debug_constructors_mm :
  Mebi_unification.Constructors.t -> unit Mebi_wrapper.mm

val debug_validconstrs_start :
  Evd.econstr -> unit Mebi_wrapper.mm

val debug_validconstrs_close :
  Evd.econstr ->
  Mebi_unification.Constructors.t ->
  unit Mebi_wrapper.mm

val debug_validconstrs_iter_start :
  int ->
  Mebi_unification.Constructors.t ->
  unit Mebi_wrapper.mm

val debug_validconstrs_iter_close :
  int ->
  Mebi_unification.Constructors.t ->
  unit Mebi_wrapper.mm

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
  Mebi_unification.Constructors.t ->
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
