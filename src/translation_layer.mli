val default_table_size : int

type fsm_table =
  { term_type : string
  ; lbls_type : string
  ; type_name : string
  ; state_map : (Evd.econstr, int) Hashtbl.t
  }

val fsm_table
  :  ?state_map:(Evd.econstr, int) Hashtbl.t
  -> string
  -> string
  -> string
  -> fsm_table

val econstr_to_string : Environ.env -> Evd.evar_map -> Evd.econstr -> string

val lts_to_fsm
  :  Environ.env
  -> Evd.evar_map
  -> Evd.econstr
  -> Evd.econstr
  -> Evd.econstr
  -> Evd.econstr
  -> (Evd.econstr * Evd.econstr) list
  -> (Constr.rel_context * Constr.t) array
  -> fsm_table * Fsm.fsm
