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
val get_filename : filename_kind -> bool option -> string

type filetype_kind = JSON of unit

val build_filename : filename_kind -> filetype_kind -> bool option -> string

val build_filepath
  :  output_dir_kind
  -> filename_kind
  -> filetype_kind
  -> bool option
  -> string

val create_parent_dir : string -> unit

module JSON : sig
  val clean : string -> string
  val quoted : string -> string

  type col_kind =
    | List of string list
    | Dict of string list

  val handle_list_sep : string option -> string
  val handle_dict_sep : string option -> string

  val col
    :  ?prefix:string
    -> ?sep:string
    -> ?tlindent:string
    -> col_kind
    -> string

  val key_val : ?prefix:string -> string -> string -> string
  val model_info : Utils.model_info option -> string

  module LTS : sig
    val initial : string option -> string
    val states : Lts.States.t -> string
    val transition : Lts.transition -> string
    val transitions : Lts.Transitions.t -> string
    val lts : string -> Lts.lts -> string
  end

  module FSM : sig
    val state : Fsm.state -> string
    val annotation : Fsm.annotation -> string
    val annotations : Fsm.annotations -> string
    val action : Fsm.action -> string
    val alphabet : Fsm.Alphabet.t -> string
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
  -> string * bool option

val write_to_file
  :  output_dir_kind
  -> filename_kind
  -> filetype_kind
  -> dumpable_kind
  -> string
