val split_at : int -> 'a list -> 'a list -> 'a list * 'a list
val strip_snd : (EConstr.t * EConstr.t) list -> EConstr.t list
val ref_list_to_glob_list : Libnames.qualid list -> Names.GlobRef.t list

type 'a mm = 'a Mebi_monad.t

val econstr_to_string : EConstr.t -> string
val constr_to_string : Constr.t -> string
