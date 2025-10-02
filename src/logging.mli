type output_mode = Coq of unit | OCaml of unit

val default_mode : output_mode

type output_kind =
  | Notice
  | Debug
  | Trace
  | Details
  | Result
  | Warning

type output_options = {
  mutable output_enabled : bool;
  mutable show_notice_enabled : bool;
  mutable show_debug_enabled : bool;
  mutable show_details_enabled : bool;
  mutable show_result_enabled : bool;
  mutable show_warning_enabled : bool;
}

val default_output_enabled : bool
val default_show_notice_enabled : bool
val default_show_debug_enabled : bool
val default_show_details_enabled : bool
val default_show_result_enabled : bool
val default_show_warning_enabled : bool
val default_output_options : output_options

type params = {
  mutable mode : output_mode;
  options : output_options;
  mutable override : bool;
}

val the_params : params ref
val enable_output : unit -> unit
val disable_output : unit -> unit
val set_output_mode : output_mode -> unit
val set_output_enabled : bool -> unit
val set_show_notice : bool -> unit
val set_show_debug : bool -> unit
val set_show_details : bool -> unit
val set_show_result : bool -> unit
val set_show_warning : bool -> unit
val reset_output_enabled : unit -> unit
val reset_show_notice_enabled : unit -> unit
val reset_show_debug_enabled : unit -> unit
val reset_show_details_enabled : unit -> unit
val reset_show_result_enabled : unit -> unit
val reset_show_warning_enabled : unit -> unit
val is_output_enabled : unit -> bool
val is_notice_enabled : unit -> bool
val is_debug_enabled : unit -> bool
val is_details_enabled : unit -> bool
val is_result_enabled : unit -> bool
val is_warning_enabled : unit -> bool
val is_output_kind_enabled : output_kind -> bool
val message_prefix : output_kind -> string
val log : output_kind -> string -> unit

module Log : sig
  val override : string -> unit
  val notice : string -> unit
  val debug : string -> unit
  val trace : string -> unit
  val details : string -> unit
  val result : string -> unit
  val warning : string -> unit
end

val get_output_mode : unit -> unit
