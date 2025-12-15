type output_mode = Coq | OCaml
type level = Feedback.level

val make_level_fun :
  ?debug:bool ->
  ?info:bool ->
  ?notice:bool ->
  ?warning:bool ->
  ?error:bool ->
  unit ->
  level ->
  bool

module Output : sig
  module type OUTPUT_TYPE = sig
    val output_type_as_string : string

    val do_feedback_output :
      ?override:bool ->
      (level -> bool) ->
      level ->
      string ->
      unit
  end

  module type S = sig
    val output_type_as_string : string
    val get_output_fun : level -> string -> unit
  end

  module Make : (_ : S) -> OUTPUT_TYPE
  module Rocq : OUTPUT_TYPE
  module OCaml : OUTPUT_TYPE
end

module type LOGGER_TYPE = sig
  val enabled : bool ref
  val debug : string -> unit
  val info : string -> unit
  val notice : string -> unit
  val warning : string -> unit
  val error : string -> unit
  val trace : string -> unit
  val result : string -> unit
  val override : level -> string -> unit
  val is_level_enabled : level -> bool
end

module type S = sig
  val is_level_enabled : level -> bool
end

module Make : (_ : Output.OUTPUT_TYPE) (_ : S) -> LOGGER_TYPE
module Test : LOGGER_TYPE
