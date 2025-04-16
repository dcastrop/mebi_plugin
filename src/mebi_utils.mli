val split_at : int -> 'a list -> 'a list -> 'a list * 'a list
val strip_snd : (EConstr.t * EConstr.t) list -> EConstr.t list

val ref_list_to_glob_list :
  Libnames.qualid list -> Names.GlobRef.t list

type 'a mm = 'a Mebi_monad.t

val econstr_to_string : EConstr.t -> string
val constr_to_string : Constr.t -> string
val type_of_tref : Constrexpr.constr_expr -> EConstr.t mm
val type_of_econstr : EConstr.t -> EConstr.t mm

type keys_kind = OfEConstr of EConstr.t Seq.t

val pstr_keys : keys_kind -> string
