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
    -> Output.Kind.t
    -> string
    -> 'a
    -> ('a -> string)
    -> unit

  val things
    :  ?__FUNCTION__:string
    -> Output.Kind.t
    -> string
    -> 'a list
    -> ('a -> string)
    -> unit

  val option
    :  ?__FUNCTION__:string
    -> Output.Kind.t
    -> string
    -> 'a option
    -> ('a -> string)
    -> unit

  val options
    :  ?__FUNCTION__:string
    -> Output.Kind.t
    -> string
    -> 'a list option
    -> ('a -> string)
    -> unit
end

val default_level : Output.Kind.level -> bool
val default_special : Output.Kind.special -> bool

module Make : (Mode : Output.Mode.S)
    (X : sig
       val prefix : string option
       val level : Output.Kind.level -> bool
       val special : Output.Kind.special -> bool
     end)
    -> S

module MkDefault : () -> S
module Default : S

(** [module ReMake (Old) (New)] returns a new [Logger.S] with updated config. {b E.g.:} 
{[
  module Log = Logger.MkDefault ()
  module Log' = Logger.Remake (Log) (struct
  let level = Logger.default_level
  let special : Output.Kind.special -> bool = function
  | Trace -> false
  | Result -> true
  | Show -> true end)
]} *)
module ReMake : (Old : S)
    (New : sig
       val level : (Feedback.level -> bool) option
       val special : (Output.Kind.special -> bool) option
     end)
    -> S with module Config.Mode = Old.Config.Mode
