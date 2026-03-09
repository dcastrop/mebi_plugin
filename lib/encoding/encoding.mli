module type S = sig
  type t

  val init : t
  val next : t -> t
  val reset : unit -> unit
  val incr : unit -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
end

module Make : (Log : Logger.S)
    (X : sig
       type t

       val init : t
       val next : t -> t
       val equal : t -> t -> bool
       val compare : t -> t -> int
       val hash : t -> int
       val to_string : t -> string
     end)
    -> S

module Int : (Log : Logger.S) -> S with type t = Int.t
