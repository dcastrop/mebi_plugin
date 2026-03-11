module type S = sig
  type action
  type states
  type t = action * states

  val compare : t -> t -> int
  val shorter_annotation : t -> t -> t
  val try_update : t -> t list -> t option * t list
  val merge_lists : t list -> t list -> t list
  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
end

module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (States : States.S)
    (Annotation : Annotation.S)
    (Action :
       Action.S
       with type annotation = Annotation.t
        and type trees = Base.Trees.t) :
  S
  with type action = Action.t
   and type states = States.t
   and type t = Action.t * States.t
