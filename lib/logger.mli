module type S = sig
  module Config : Output.Config.S

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
    -> Output.Kind.t
    -> string
    -> 'a
    -> 'a Utils.Strfy.to_string
    -> unit

  val things
    :  ?__FUNCTION__:string
    -> ?args:Utils.Strfy.style_args
    -> Output.Kind.t
    -> string
    -> 'a list
    -> 'a Utils.Strfy.to_string
    -> unit

  val option
    :  ?__FUNCTION__:string
    -> ?args:Utils.Strfy.style_args
    -> Output.Kind.t
    -> string
    -> 'a option
    -> 'a Utils.Strfy.to_string
    -> unit

  val options
    :  ?__FUNCTION__:string
    -> ?args:Utils.Strfy.style_args
    -> Output.Kind.t
    -> string
    -> 'a list option
    -> 'a Utils.Strfy.to_string
    -> unit
end

module Make : (Mode : Output.Mode.S)
    (X : sig
       val prefix : string option
       val level : Feedback.level -> bool
       val special : Output.Kind.special -> bool
     end)
    -> S

module MkDefault : () -> S
module Default : S
