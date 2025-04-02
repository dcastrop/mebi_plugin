val perm : int
val default_output_dir : string

type output_dir_kind =
  | Default of unit
  | Exact of string

val get_local_timestamp : string

type filename_kind =
  | Auto of unit
  | Just of string
  | LTS of string
  | FSM of string

val get_filename : filename_kind -> string

type filetype_kind = JSON of unit

val build_filename : filename_kind -> filetype_kind -> string
val build_filepath : output_dir_kind -> filename_kind -> filetype_kind -> string
val create_parent_dir : string -> unit

module JSON : sig
  val clean : string -> string
  val quoted : string -> string

  type col_kind =
    | List of string list
    | Dict of string list

  val col : col_kind -> string
  val key_val : string -> string -> string

  module LTS : sig
    val initial : string option -> string
    val transition : Lts.transition -> string
    val transitions : Lts.Transitions.t -> string
    val lts : string -> Lts.lts -> string
  end

  val of_fsm : Fsm.fsm -> string
end

type dumpable_kind =
  | LTS of Lts.lts
  | FSM of Fsm.fsm

val handle_filecontents : string -> filetype_kind -> dumpable_kind -> string

val write_to_file
  :  output_dir_kind
  -> filename_kind
  -> filetype_kind
  -> dumpable_kind
  -> string
