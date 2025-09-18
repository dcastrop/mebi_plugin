type output_modes = Coq of unit | OCaml of unit

val default_mode : output_modes
val the_output_mode : output_modes ref
val set_output_mode : output_modes -> unit
val show_debug_messages : bool ref
val show_detailed_messages : bool ref

type output_kind =
  | Notice of unit
  | Details of unit
  | Debug of unit
  | Warning of unit

type output_options = {
  mutable output_enabled : bool;
  mutable show_notice_enabled : bool;
  mutable show_debug_enabled : bool;
  mutable show_detailed_enabled : bool;
  mutable show_warning_enabled : bool;
}

val default_output_options : output_options
val the_output_options : output_options ref

type params = {
  mutable options : output_options;
  mutable override : bool;
}

val the_params : params ref
val is_output_kind_enabled : output_kind -> bool
val message_prefix : output_kind -> string
val log : output_kind -> string -> unit

module Log : sig
  val override : string -> unit
  val notice : string -> unit
  val details : string -> unit
  val debug : string -> unit
  val warning : string -> unit
end

val get_output_mode : unit
val get_show_debug_messages : unit
val get_show_detailed_messages : unit
val set_show_debug_messages : bool -> unit
val set_show_detailed_messages : bool -> unit
