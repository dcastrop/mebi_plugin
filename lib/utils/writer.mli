
val perm : int
val default_dir : string
val get_loc : unit -> string
val create_parent_dir : string -> unit

val file :
  ?dir:string -> string -> ('a -> Yojson.t) -> 'a -> unit
