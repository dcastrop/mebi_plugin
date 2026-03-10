module Make : (Log : Logger.S)
    (Constructor : sig
       type t

       val json : ?as_elt:bool -> t -> Yojson.t
     end)
    -> sig
  type t = Constructor.t list

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
end
