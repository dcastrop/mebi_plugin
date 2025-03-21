val handle_pp : ?coq:bool -> ?show:bool -> ?debug:bool -> string -> unit
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

val pp_edge : Environ.env -> Evd.evar_map -> EConstr.t -> Pp.t

val pp_edges_to_list
  :  Environ.env
  -> Evd.evar_map
  -> EConstr.t list
  -> Pp.t list

val pp_edges : Environ.env -> Evd.evar_map -> EConstr.t list -> Pp.t

val pp_edges'
  :  Environ.env
  -> Evd.evar_map
  -> (EConstr.t * EConstr.t) list
  -> Pp.t

val pp_state : Environ.env -> Evd.evar_map -> EConstr.t -> Pp.t

val pp_states_to_list
  :  Environ.env
  -> Evd.evar_map
  -> EConstr.t list
  -> Pp.t list

val pp_states : Environ.env -> Evd.evar_map -> EConstr.t list -> Pp.t

val pp_coq_fsm
  :  Environ.env
  -> Evd.evar_map
  -> EConstr.t list * EConstr.t list
  -> Pp.t
