module Make : (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type t = Base.t)
    (WIP : sig
       type t

       val equal : t -> t -> bool
     end)
    (Trace : sig
       type t =
         { this : WIP.t
         ; next : next option
         }

       and next =
         | Next of t
         | Goto of State.t

       val json : ?as_elt:bool -> t -> Yojson.t
       val compare : t -> t -> int
       val get : WIP.t -> t -> t
     end)
    -> sig
  include Set.S with type elt = Trace.t

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val get : WIP.t -> t -> t
end
