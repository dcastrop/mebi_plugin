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

val default_level
  :  ?debug:bool
  -> ?info:bool
  -> ?notice:bool
  -> ?warning:bool
  -> ?error:bool
  -> level
  -> bool

val default_special
  :  ?trace:bool
  -> ?result:bool
  -> ?show:bool
  -> special
  -> bool

module type OUTPUT_KIND = sig
  type t

  val defaults : t -> bool
  val config : (t, bool) Hashtbl.t
  val is_enabled : t -> bool
  val configure : t -> bool -> unit
  val reset : t -> unit
end

module type S = sig
  type t

  val defaults : t -> bool
end

module Make : (O : S) -> sig
  type t = O.t

  val defaults : t -> bool
  val config : (t, bool) Hashtbl.t
  val is_enabled : t -> bool
  val configure : t -> bool -> unit
  val reset : t -> unit
end
