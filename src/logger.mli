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

val level_fun_preset_debug :
  ?trace:bool -> unit -> level -> bool

val level_fun_preset_results : unit -> level -> bool

module Output : sig
  module type OUTPUT_TYPE = sig
    val output_type_as_string : string

    val do_feedback_output :
      ?__FUNCTION__:string ->
      ?prefix:string option ->
      ?override:bool ->
      (level -> bool) ->
      level ->
      string ->
      unit
  end

  module type S = sig
    val output_type_as_string : string

    val get_output_fun :
      ?__FUNCTION__:string ->
      ?prefix:string option ->
      level ->
      string ->
      unit
  end

  module Make : (_ : S) -> OUTPUT_TYPE
  module Rocq : OUTPUT_TYPE
  module OCaml : OUTPUT_TYPE
end

type 'a to_string =
  | Args of (?args:Utils.Strfy.style_args -> 'a -> string)
  | Of of ('a -> string)

module type LOGGER_TYPE = sig
  val enabled : bool ref
  val prefix : string option
  val debug : ?__FUNCTION__:string -> string -> unit
  val info : ?__FUNCTION__:string -> string -> unit
  val notice : ?__FUNCTION__:string -> string -> unit
  val warning : ?__FUNCTION__:string -> string -> unit
  val error : ?__FUNCTION__:string -> string -> unit
  val trace : ?__FUNCTION__:string -> string -> unit
  val result : ?__FUNCTION__:string -> string -> unit

  val override :
    ?__FUNCTION__:string -> level -> string -> unit

  val thing :
    ?__FUNCTION__:string ->
    ?args:Utils.Strfy.style_args ->
    level ->
    string ->
    'a ->
    'a to_string ->
    unit

  val option :
    ?__FUNCTION__:string ->
    ?args:Utils.Strfy.style_args ->
    level ->
    string ->
    'a option ->
    'a to_string ->
    unit

  val is_level_enabled : level -> bool
end

module type S = sig
  val prefix : string option
  val is_level_enabled : level -> bool
end

module Make : (_ : Output.OUTPUT_TYPE) (_ : S) -> LOGGER_TYPE

val make :
  ?prefix:string option ->
  (level -> bool) ->
  (module Output.OUTPUT_TYPE) ->
  (module LOGGER_TYPE)

val debug :
  ?prefix:string option ->
  ?trace:bool ->
  (module Output.OUTPUT_TYPE) ->
  (module LOGGER_TYPE)

val results :
  ?prefix:string option ->
  (module Output.OUTPUT_TYPE) ->
  (module LOGGER_TYPE)
