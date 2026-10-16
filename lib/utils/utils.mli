(** Small helpers on lists, options and strings, and writing files. Plain
    OCaml: no Rocq runtime. *)

(** [split_at i l] is the first [i] elements of [l] (all of them if [l] is
    shorter), {b in reverse order}. Raises nothing. *)
val split_at : int -> 'a list -> 'a list

(** [compare_chain cs] is the first non-zero comparison of [cs], or [0]: a
    lexicographic comparison from its parts, most significant first.
    Raises nothing. *)
val compare_chain : int list -> int

(** [new_int_counter ?start ()] is a new counter, from [start] (default 0):
    [((next, prev), r)], where [next ()] adds one and is the new value (so
    the first call gives [start + 1]), [prev ()] is the value and then
    subtracts one, and [r] holds the value. Raises nothing. *)
val new_int_counter
  :  ?start:int
  -> unit
  -> ((unit -> int) * (unit -> int)) * int ref

(** [str_sep ?sep ?last ?empty xs] is [xs] joined by [sep] (default
    ["; "]), with [last] (default [sep]) after the last one; [empty]
    (default [""]) if [xs] is empty. Raises nothing. *)
val str_sep
  :  ?sep:string
  -> ?last:string
  -> ?empty:string
  -> string list
  -> string

(** [filter_opt xs] is the values of the [Some]s of [xs], in order. Raises
    nothing. *)
val filter_opt : 'a option list -> 'a list

(** [option_fstr f x] is ["None"], or ["Some (s)"] with [s] being [f] of
    [x]'s value. Raises whatever [f] raises (propagated). *)
val option_fstr : ('a -> string) -> 'a option -> string

(** [clean_string s] is [s] on one line: each run of newlines, tabs and
    spaces becomes one space (a leading run is dropped), and each double
    quote becomes a single quote. Raises nothing. *)
val clean_string : string -> string

(** Writing result dumps ([MeBi Config Output "DumpResults"]). *)
module FileWriter : sig
  (** The permissions {!create_parent_dir} gives the directories it
      creates. *)
  val perm : int

  (** Where dumps go: [./_dumps/], relative to the compile directory. *)
  val default_dir : string

  (** [set_loc_provider f] makes [f] what {!get_loc} calls. Reading the
      source location needs Rocq's [Loc], so [src/] installs an
      implementation at plugin load ({!Mebi_plugin.Rocq_output}); until then
      {!get_loc} is ["Unknown Location"]. Raises nothing. *)
  val set_loc_provider : (unit -> string) -> unit

  (** [get_loc ()] is the source location a dump is labelled with, from the
      installed provider ({!set_loc_provider}). Raises whatever the provider
      raises (propagated). *)
  val get_loc : unit -> string

  (** [create_parent_dir path] creates the directory that will hold the
      file [path], and any of its parents that are missing.

      @raise Sys_error if a directory cannot be created (propagated). *)
  val create_parent_dir : string -> unit

  (** The local time, as ["yyyy mm dd - hh:mm:ss"], when the plugin was
      loaded: it is computed once, so every dump of a session has the same
      one. *)
  val get_local_timestamp : string
end
