type help_set_kind =
  | General of unit
  | Bound of unit
  | DumpToFile of unit
  | ShowDebug of unit
  | ShowDetails of unit
  | WeakMode of unit
  | Weak of unit

type help_kind =
  | Basic of unit
  | Set of help_set_kind
  | Check of unit
  | LTS of unit
  | FSM of unit
  | Saturate of unit
  | Minimize of unit
  | Bisim of unit
  | Info of unit
  | Unrecognized of unit

val show_instructions_to_toggle_weak : bool -> unit
val show_instructions_to_enable_weak : unit -> unit
val show_instructions_to_set_weak : unit -> unit
val show_help_basic : unit -> unit
val show_help_unrecognized_command : unit -> unit
val show_help_set : unit -> unit
val show_help_set_bound : unit -> unit
val show_help_set_dump_to_file : unit -> unit
val show_help_set_show_debug : unit -> unit
val show_help_set_show_details : unit -> unit
val show_help_set_weak_mode : unit -> unit
val show_help_set_weak : unit -> unit
val show_help_check : unit -> unit
val show_help_lts : unit -> unit
val show_help_fsm : unit -> unit
val show_help_saturate : unit -> unit
val show_help_minimize : unit -> unit
val show_help_bisim : unit -> unit
val show_help_info : unit -> unit
val show_guidelines_and_limitations : unit -> unit
val handle_help : help_kind -> unit
