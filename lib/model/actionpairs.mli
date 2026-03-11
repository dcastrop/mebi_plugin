module type S = sig
  type states

  include Set.S

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val destinations : t -> states

  exception IsEmpty

  val shortest_annotation : t -> elt
  val merge_list : t -> elt list -> t
end

module Make
    (Log : Logger.S)
    (States : States.S)
    (Action : Action.S)
    (ActionPair :
       Actionpair.S with type action = Action.t and type states = States.t) :
  S
  with type states = States.t
   and type states = ActionPair.states
   and type elt = ActionPair.t
   and type elt = Action.t * States.t
