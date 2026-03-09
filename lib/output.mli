module Kind : sig
  type t =
    | Debug
    | Info
    | Notice
    | Warning
    | Error
    | Trace
    | Result
    | Show

  type k =
    | Level of level
    | Special of special

  and level = Feedback.level

  and special =
    | Trace
    | Result
    | Show

  val kind : t -> k

  val default_level_fun
    :  ?debug:bool
    -> ?info:bool
    -> ?notice:bool
    -> ?warning:bool
    -> ?error:bool
    -> level
    -> bool

  val default_level
    : (?debug:bool
       -> ?info:bool
       -> ?notice:bool
       -> ?warning:bool
       -> ?error:bool
       -> level
       -> bool)
        ref

  val default_special_fun
    :  ?trace:bool
    -> ?result:bool
    -> ?show:bool
    -> special
    -> bool

  val default_special
    : (?trace:bool -> ?result:bool -> ?show:bool -> special -> bool) ref

  module type S = sig
    type t

    val defaults : t -> bool
    val config : (t, bool) Hashtbl.t
    val is_enabled : t -> bool
    val configure : t -> bool -> unit
    val reset : t -> unit
  end

  module Make : (O : sig
                   type t

                   val defaults : t -> bool
                 end)
      -> S with type t = O.t
end

module Mode : sig
  module type S = sig
    val output_mode_as_string : string

    val do_level_output
      :  ?__FUNCTION__:string
      -> ?prefix:string option
      -> ?override:bool
      -> (Kind.level -> bool)
      -> Kind.level
      -> string
      -> unit

    val do_special_output
      :  ?__FUNCTION__:string
      -> ?prefix:string option
      -> ?override:bool
      -> (Kind.special -> bool)
      -> Kind.special
      -> string
      -> unit
  end

  module Make : (_ : sig
                   val output_mode_as_string : string

                   val level_output_fun
                     :  ?__FUNCTION__:string
                     -> ?prefix:string option
                     -> Kind.level
                     -> string
                     -> unit

                   val special_output_fun
                     :  ?__FUNCTION__:string
                     -> ?prefix:string option
                     -> Kind.special
                     -> string
                     -> unit
                 end)
      -> S

  module Rocq : S
  module OCaml : S
  module Default = Rocq
end

module Config : sig
  type t =
    { mutable enabled : bool
    ; level_defaults :
        (?debug:bool
         -> ?info:bool
         -> ?notice:bool
         -> ?warning:bool
         -> ?error:bool
         -> Kind.level
         -> bool)
          ref
    ; special_defaults :
        (?trace:bool -> ?result:bool -> ?show:bool -> Kind.special -> bool) ref
    }

  val default : unit -> t

  module type S = sig
    val get : t ref
    val reset : unit -> unit
    val enable_output : unit -> unit
    val disable_output : unit -> unit
    val configure_output : Kind.t -> bool -> unit

    val do_output
      :  ?__FUNCTION__:string
      -> ?prefix:string option
      -> ?override:bool
      -> Kind.t
      -> string
      -> unit

    val show_mode : unit -> unit
    val show_enabled : unit -> unit
    val show_kind : Kind.t -> unit
  end

  type x = t

  module Make : (_ : Mode.S)
      (_ : sig
         val level : Feedback.level -> bool
         val special : Kind.special -> bool
       end)
      -> S
end
