type t = { enc : Mebi_setup.Enc.t; pp : string option }

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
val to_string : ?args:Utils.Strfy.style_args -> t -> string
