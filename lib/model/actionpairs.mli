module Make : (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type t = Base.t)
    (States : States.S with type elt = State.t)
    (Action : sig
       type t

       val equal : t -> t -> bool
     end)
    (ActionPair : sig
       type t = Action.t * States.t

       val json : ?as_elt:bool -> t -> Yojson.t
       val compare : t -> t -> int
       val shorter_annotation : t -> t -> t
     end)
    -> sig
  include Set.S with type elt = ActionPair.t

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val destinations : t -> States.t

  exception IsEmpty

  val shortest_annotation : t -> ActionPair.t
  val merge_list : t -> ActionPair.t list -> t
end
