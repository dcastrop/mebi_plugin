
type output_mode = Coq of unit | OCaml of unit

module Log : sig
  val override : string -> unit
  val notice : string -> unit
  val details : string -> unit
  val debug : string -> unit
  val warning : string -> unit
end

val get_output_mode : unit -> unit
val get_show_debug_messages : unit -> unit
val get_show_detailed_messages : unit -> unit
val set_output_mode : output_mode -> unit
val set_show_debug_messages : bool -> unit
val set_show_detailed_messages : bool -> unit

val enable_output : unit -> unit
val disable_output : unit -> unit
