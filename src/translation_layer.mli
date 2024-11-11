val default_table_size : int

type fsm_table =
  { env : Environ.env
  ; sigma : Evd.evar_map
  ; term_type : string
  ; lbls_type : string
  ; type_name : string
  ; state_map : (Evd.econstr, int) Hashtbl.t
  }

val fsm_table
  :  ?state_map:(Evd.econstr, int) Hashtbl.t
  -> Environ.env
  -> Evd.evar_map
  -> string
  -> string
  -> string
  -> fsm_table

type translate_from =
  | States of Evd.econstr list
  | Edges of (Evd.econstr * Evd.econstr) list

type translate_to =
  | State of Fsm.state
  | States of Fsm.states
  | Edge of Fsm.edge
  | Edges of Fsm.edges

type translate_map = (Evd.econstr * int) list

val get_states
  :  Environ.env
  -> Evd.evar_map
  -> Evd.econstr list
  -> Fsm.states * translate_map

val get_edges
  :  Environ.env
  -> Evd.evar_map
  -> Evd.econstr list
  -> (Evd.econstr, int) Hashtbl.t
  -> Fsm.edges * translate_map

val lts_to_fsm
  :  Environ.env
  -> Evd.evar_map
  -> Evd.econstr
  -> Evd.econstr
  -> Evd.econstr
  -> Evd.econstr
  -> (Constr.rel_context * Constr.t) array
  -> Evd.econstr list
  -> Evd.econstr list
  -> fsm_table * Fsm.fsm
