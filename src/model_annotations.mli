module Make : (Log : Logger.S)
    (Label : sig
       type t

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val hash : t -> int
       val is_silent : t -> bool
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (Note : sig
       type t

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val is_silent : t -> bool
       val has_label : Label.t -> t -> bool
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (Annotation : sig
       type t =
         { this : Note.t
         ; next : t option
         }

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val is_empty : t -> bool
       val length : t -> int
       val shorter : t -> t -> t
       val exists : Note.t -> t -> bool
       val exists_label : Label.t -> t -> bool
       val append : Note.t -> t -> t
       val last : t -> Note.t

       exception CannotDropLastOfSingleton of t

       val drop_last : t -> t
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    -> sig
  include Set.S with type elt = Annotation.t

  val extrapolate : elt -> t
  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
end
