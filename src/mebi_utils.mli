val split_at : int -> 'a list -> 'a list -> 'a list * 'a list
val strip_snd : (Evd.econstr * Evd.econstr) list -> Evd.econstr list

val econstr_mem
  :  Environ.env
  -> Evd.evar_map
  -> Evd.econstr
  -> Evd.econstr list
  -> bool

val econstr_tup_mem
  :  Environ.env
  -> Evd.evar_map
  -> Evd.econstr * Evd.econstr
  -> (Evd.econstr * Evd.econstr) list
  -> bool

val econstr_list_cap
  :  Environ.env
  -> Evd.evar_map
  -> Evd.econstr list
  -> Evd.econstr list
  -> Evd.econstr list

val econstr_tuplist_cap
  :  Environ.env
  -> Evd.evar_map
  -> (Evd.econstr * Evd.econstr) list
  -> (Evd.econstr * Evd.econstr) list
  -> (Evd.econstr * Evd.econstr) list

val econstr_list_merge
  :  Environ.env
  -> Evd.evar_map
  -> Evd.econstr list
  -> Evd.econstr list
  -> Evd.econstr list

val econstr_tuplist_merge
  :  Environ.env
  -> Evd.evar_map
  -> (Evd.econstr * Evd.econstr) list
  -> (Evd.econstr * Evd.econstr) list
  -> (Evd.econstr * Evd.econstr) list

val econstr_list_unique
  :  Environ.env
  -> Evd.evar_map
  -> Evd.econstr list
  -> Evd.econstr list
