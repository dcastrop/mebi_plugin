(** [econstr_to_string env sigma target] is a [string] representing [target]. *)
let econstr_to_string
  (env : Environ.env)
  (sigma : Evd.evar_map)
  (target : Evd.econstr)
  : string
  =
  Pp.string_of_ppcmds (Printer.pr_econstr_env env sigma target)
;;
