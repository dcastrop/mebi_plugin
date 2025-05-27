val split_at : int -> 'a list -> 'a list -> 'a list * 'a list
val strip_snd : (Evd.econstr * Evd.econstr) list -> Evd.econstr list
val ref_list_to_glob_list : Libnames.qualid list -> Names.GlobRef.t list
val econstr_to_string_mm : Evd.econstr -> string Mebi_monad.mm
val econstr_to_string : Evd.econstr -> string
val constr_to_string_mm : Constr.t -> string Mebi_monad.mm
val constr_to_string : Constr.t -> string
val tref_to_econstr : Constrexpr.constr_expr -> Evd.econstr Mebi_monad.mm
val normalize_econstr : Evd.econstr -> Evd.econstr Mebi_monad.mm
val type_of_econstr : Evd.econstr -> Evd.econstr Mebi_monad.mm
val type_of_tref : Constrexpr.constr_expr -> Evd.econstr Mebi_monad.mm

type keys_kind = OfEConstr of Evd.econstr Seq.t

val pstr_keys : keys_kind -> string
