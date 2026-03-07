module Make : (Log : Logger.S)
    (State : sig
       type t

       val json : ?as_elt:bool -> t -> Yojson.t

       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       val equal : t -> t -> bool
       val compare : t -> t -> int
       (* val hash : t -> int *)
     end)
    (Label : sig
       type t

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val equal : t -> t -> bool *)
       (* val compare : t -> t -> int *)
       (* val hash : t -> int *)
       (* val is_silent : t -> bool *)
     end)
    (Action : sig
       type t

       val json : ?as_elt:bool -> t -> Yojson.t

       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       val equal : t -> t -> bool
       val compare : t -> t -> int

       (* val hash : t -> int *)
       (* val wk_equal : t -> t -> bool *)
       val is_silent : t -> bool
       val is_labelled : Label.t -> t -> bool
       (* val shorter_annotation : t -> t -> t *)
     end)
    -> sig
  type t =
    { from : State.t
    ; goto : State.t
    ; action : Action.t
    }

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_silent : t -> bool
  val is_labelled : Label.t -> t -> bool
end
