
type help_set_kind =
  | General of unit
  | Bound of unit
  | FailIfIncomplete of unit
  | FailIfNotBisim of unit
  | DumpToFile of unit
  | ShowAny of unit
  | ShowNotices of unit
  | ShowDebug of unit
  | ShowDetails of unit
  | ShowResults of unit
  | ShowWarnings of unit
  | WeakMode of unit
  | Weak of unit

type help_kind =
  | Basic of unit
  | Set of help_set_kind
  | Reset of unit
  | See of unit
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

val show_guidelines_and_limitations : unit -> unit
val handle_help : help_kind -> unit
