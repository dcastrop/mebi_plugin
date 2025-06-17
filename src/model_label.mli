type meta = { is_silent : bool option; info : string option }
type t = Mebi_wrapper.IntEncoding.t * string option

val eq : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
val to_string : t -> string
val pstr : ?indents:int -> t -> string
