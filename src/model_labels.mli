module Make : (Log : Logger.S)
    (Label : sig
       type t

       val json : ?as_elt:bool -> t -> Yojson.t

       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val equal : t -> t -> bool *)
       val compare : t -> t -> int

       (* val hash : t -> int *)
       val is_silent : t -> bool
     end)
    -> sig
  include Set.S with type elt = Label.t

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val non_silent : t -> t
end
