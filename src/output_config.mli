type t =
  { mutable enabled : bool
  ; level_defaults :
      (?debug:bool
       -> ?info:bool
       -> ?notice:bool
       -> ?warning:bool
       -> ?error:bool
       -> Output_kind.level
       -> bool)
        ref
  ; special_defaults :
      (?trace:bool -> ?result:bool -> ?show:bool -> Output_kind.special -> bool)
        ref
  }

val default : unit -> t

module type OUTPUT_CONFIG = sig
  val get : t ref
  val reset : unit -> unit
  val enable_output : unit -> unit
  val disable_output : unit -> unit
  val configure_output : Output_kind.t -> bool -> unit

  val do_output
    :  ?__FUNCTION__:string
    -> ?prefix:string option
    -> ?override:bool
    -> Output_kind.t
    -> string
    -> unit

  val show_mode : unit -> unit
  val show_enabled : unit -> unit
  val show_kind : Output_kind.t -> unit
end

module type S = sig
  val level : Feedback.level -> bool
  val special : Output_kind.special -> bool
end

type x = t

module Make : (_ : Output_mode.OUTPUT_MODE) (_ : S) -> OUTPUT_CONFIG
