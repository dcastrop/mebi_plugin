val swap : 'a * 'b -> 'b * 'a
val split_at : int -> 'a list -> 'a list
val compare_chain : int list -> int
val try_seq_opt : 'a -> ('a -> 'b option) list -> 'b option
val strip_snd : ('a * 'a) list -> 'a list
val get_key_of_val : ('a, 'b) Hashtbl.t -> 'b -> 'a option

val new_int_counter
  :  ?start:int
  -> unit
  -> ((unit -> int) * (unit -> int)) * int ref

val str_sep
  :  ?sep:string
  -> ?last:string
  -> ?empty:string
  -> string list
  -> string

val filter_opt : 'a option list -> 'a list
val option_str : string option -> string
val option_fstr : ('a -> string) -> 'a option -> string
val clean_string : string -> string

module FileWriter : sig
  val perm : int
  val default_dir : string

  (** Labels a dump with the source location that produced it. Reading that
      location needs Rocq's [Loc], so [src/] installs an implementation via
      [set_loc_provider] at plugin load; otherwise this returns
      ["Unknown Location"]. *)
  val set_loc_provider : (unit -> string) -> unit

  val get_loc : unit -> string
  val create_parent_dir : string -> unit
  val get_local_timestamp : string
end
