val is_unit_option : unit option -> bool

type output_modes =
  | Coq of unit
  | OCaml of unit

type output_kind =
  | Normal of unit
  | Details of unit
  | Debug of unit
  | Warning of unit

val pstr_output_kind_head : output_modes -> output_kind -> string

type output_options =
  { mutable output_enabled : bool
  ; mutable show_normal_output : bool
  ; mutable show_detailed_output : bool
  ; mutable show_debug_output : bool
  ; mutable show_warning_output : bool
  }

type logging_params =
  { mode : output_modes
  ; mutable kind : output_kind
  ; mutable options : output_options
  ; mutable scope : string Stack.t
  ; mutable override : unit option
  }

val default_logging_params : ?mode:output_modes -> unit -> logging_params
val log_options : output_options -> logging_params -> unit
val log_kind : output_kind -> logging_params -> unit
val override : logging_params -> logging_params
val push_scope : string -> logging_params -> unit
val peek_scope : string -> logging_params -> string
val pop_scope : string -> logging_params -> string
val pstr_scope : string Stack.t -> string
val is_output_kind_enabled : logging_params -> bool
val log : ?params:logging_params -> string -> unit
val print : ?show:bool -> string -> unit
val default_indent_val : int
val str_tabs : ?size:int -> int -> string
val get_key_of_val : ('a, 'b) Hashtbl.t -> 'b -> 'a option
