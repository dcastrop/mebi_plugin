val split_at : int -> 'a list -> 'a list -> 'a list * 'a list
val strip_snd : (Evd.econstr * Evd.econstr) list -> Evd.econstr list
val ref_list_to_glob_list : Libnames.qualid list -> Names.GlobRef.t list

type 'a mm = 'a Mebi_monad.t

val econstr_to_string : Evd.econstr -> string
val constr_to_string : Constr.t -> string
val type_of_tref : Constrexpr.constr_expr -> Evd.econstr mm

type keys_kind = OfEConstr of Evd.econstr Seq.t

val pstr_keys : keys_kind -> string
