val print : ?show:bool -> string -> unit
val default_indent_val : int
val str_tabs : ?size:int -> int -> string
val get_key_of_val : ('a, 'b) Hashtbl.t -> 'b -> 'a option
