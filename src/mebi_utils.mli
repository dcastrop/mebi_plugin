val split_at : int -> 'a list -> 'a list -> 'a list * 'a list
val strip_snd : (Evd.econstr * Evd.econstr) list -> Evd.econstr list
val econstr_to_string : Environ.env -> Evd.evar_map -> Evd.econstr -> string
val econstr_to_int : Environ.env -> Evd.evar_map -> Evd.econstr -> int
