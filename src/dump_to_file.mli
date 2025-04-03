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

val get_name : filename_kind -> string
val get_filename : filename_kind -> bool -> string

type filetype_kind = JSON of unit

val build_filename : filename_kind -> filetype_kind -> bool -> string

val build_filepath
  :  output_dir_kind
  -> filename_kind
  -> filetype_kind
  -> bool
  -> string

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

  module FSM : sig
    val action : Fsm.action -> string
    val alphabet : Fsm.Alphabet.t -> string
    val state : Fsm.state -> string
    val states : ?key:string -> Fsm.States.t -> string
    val edge : Fsm.state -> Fsm.action -> Fsm.States.t -> string
    val edges : Fsm.States.t Fsm.Actions.t Fsm.Edges.t -> string
    val initial : Fsm.state option -> string
    val fsm : string -> Fsm.fsm -> string
  end
end

type dumpable_kind =
  | LTS of Lts.lts
  | FSM of Fsm.fsm

val handle_filecontents
  :  string
  -> filetype_kind
  -> dumpable_kind
  -> string * bool

val write_to_file
  :  output_dir_kind
  -> filename_kind
  -> filetype_kind
  -> dumpable_kind
  -> string
