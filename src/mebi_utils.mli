val split_at : int -> 'a list -> 'a list -> 'a list * 'a list
val strip_snd : (Evd.econstr * Evd.econstr) list -> Evd.econstr list
val default_indent_val : int
val str_tabs : ?size:int -> int -> string
