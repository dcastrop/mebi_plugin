module Make : (Log : Logger.S) (Enc : Encoding.S) -> sig
  type t =
    { enc : Enc.t
    ; is_silent : bool option
    }

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val is_silent : t -> bool
end
