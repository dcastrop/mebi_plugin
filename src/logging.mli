type output_mode = Coq of unit | OCaml of unit

val reset_output_enabled : unit -> unit
val reset_show_notice_enabled : unit -> unit
val reset_show_debug_enabled : unit -> unit
val reset_show_details_enabled : unit -> unit
val reset_show_warning_enabled : unit -> unit

module Log : sig
  val override : string -> unit
  val notice : string -> unit
  val details : string -> unit
  val debug : string -> unit
  val warning : string -> unit
end

val is_show_debug_enabled : unit -> bool
val is_show_details_enabled : unit -> bool
val get_output_mode : unit -> unit
val set_output_mode : output_mode -> unit
val set_show_debug : bool -> unit
val set_show_details : bool -> unit

val enable_output : unit -> unit
val disable_output : unit -> unit
