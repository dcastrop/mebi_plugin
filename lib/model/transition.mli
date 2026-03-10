module type S = sig
  type t

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_silent : t -> bool
end

type ('a, 'b, 'c, 'd) t' =
  { from : 'a
  ; goto : 'a
  ; label : 'b
  ; tree : 'c option
  ; annotation : 'd option
  }

module Make : (Log : Logger.S)
    (Base : Base_term.S)
    (State : sig
       type t = Base.t

       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
       val equal : t -> t -> bool
       val compare : t -> t -> int
       val hash : t -> int
     end)
    (Label : sig
       type t = Base.t Label.t'

       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
       val equal : t -> t -> bool
       val compare : t -> t -> int
       val hash : t -> int
       val is_silent : t -> bool
     end)
    (Note : sig
       type t =
         { from : Base.t
         ; label : Label.t
         ; using : Base.Trees.t
         ; goto : Base.t
         }
     end)
    (Annotation : sig
       type t =
         { this : Note.t
         ; next : t option
         }

       val json : ?as_elt:bool -> t -> Yojson.t
       val equal : t -> t -> bool
       val compare : t -> t -> int
     end)
    -> S with type t = (State.t, Label.t, Base.Tree.t, Annotation.t) t'
