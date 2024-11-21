val pp_list : Pp.t list -> Pp.t

val pp_transition
  :  Environ.env
  -> Evd.evar_map
  -> Constr.rel_context * Constr.t
  -> Pp.t

val pp_transitions_to_list
  :  Environ.env
  -> Evd.evar_map
  -> (Constr.rel_context * Constr.t) array
  -> Pp.t list

val pp_transitions
  :  Environ.env
  -> Evd.evar_map
  -> (Constr.rel_context * Constr.t) array
  -> Pp.t

val pp_edge : Environ.env -> Evd.evar_map -> Evd.econstr -> Pp.t

val pp_edges_to_list
  :  Environ.env
  -> Evd.evar_map
  -> Evd.econstr list
  -> Pp.t list

val pp_edges : Environ.env -> Evd.evar_map -> Evd.econstr list -> Pp.t

val pp_edges'
  :  Environ.env
  -> Evd.evar_map
  -> (Evd.econstr * Evd.econstr) list
  -> Pp.t

val pp_state : Environ.env -> Evd.evar_map -> Evd.econstr -> Pp.t

val pp_states_to_list
  :  Environ.env
  -> Evd.evar_map
  -> Evd.econstr list
  -> Pp.t list

val pp_states : Environ.env -> Evd.evar_map -> Evd.econstr list -> Pp.t
val pp_coq_lts : Mebi_structs.coq_lts -> Pp.t
