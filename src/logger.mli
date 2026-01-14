module type LOGGER_TYPE = sig
  module Config : Output_config.OUTPUT_CONFIG

  val enabled : bool ref
  val prefix : string option
  val debug : ?__FUNCTION__:string -> string -> unit
  val info : ?__FUNCTION__:string -> string -> unit
  val notice : ?__FUNCTION__:string -> string -> unit
  val warning : ?__FUNCTION__:string -> string -> unit
  val error : ?__FUNCTION__:string -> string -> unit
  val trace : ?__FUNCTION__:string -> string -> unit
  val result : ?__FUNCTION__:string -> string -> unit
  val show : ?__FUNCTION__:string -> string -> unit

  val thing
    :  ?__FUNCTION__:string
    -> ?args:Utils.Strfy.style_args
    -> Output_kind.t
    -> string
    -> 'a
    -> 'a Utils.Strfy.to_string
    -> unit

  val things
    :  ?__FUNCTION__:string
    -> ?args:Utils.Strfy.style_args
    -> Output_kind.t
    -> string
    -> 'a list
    -> 'a Utils.Strfy.to_string
    -> unit

  val option
    :  ?__FUNCTION__:string
    -> ?args:Utils.Strfy.style_args
    -> Output_kind.t
    -> string
    -> 'a option
    -> 'a Utils.Strfy.to_string
    -> unit

  val options
    :  ?__FUNCTION__:string
    -> ?args:Utils.Strfy.style_args
    -> Output_kind.t
    -> string
    -> 'a list option
    -> 'a Utils.Strfy.to_string
    -> unit
end

module type S = sig
  val prefix : string option
  val level : Feedback.level -> bool
  val special : Output_kind.special -> bool
end

module Make : (_ : Output_mode.OUTPUT_MODE) (_ : S) -> LOGGER_TYPE
module MkDefault : () -> LOGGER_TYPE
module Default : LOGGER_TYPE
