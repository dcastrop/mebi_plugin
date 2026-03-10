module Make : (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type t = Base.t)
    (States : States.S with type elt = State.t)
    (Label : Label.S with type t = Base.t Label.t')
    (Annotation : sig
       type t
     end)
    (Action : sig
       type t =
         { label : Label.t
         ; annotation : Annotation.t option
         ; constructor_trees : Base.Trees.t
         }

       val json : ?as_elt:bool -> t -> Yojson.t
       val equal : t -> t -> bool
       val hash : t -> int
     end)
    (Actions : sig
       include Set.S with type elt = Action.t
     end)
    (ActionPair : sig
       type t = Action.t * States.t
     end)
    (ActionPairs : sig
       include Set.S with type elt = ActionPair.t
     end)
    -> sig
  include Hashtbl.S with type key = Action.t

  type t' = States.t t

  val json : ?as_elt:bool -> t' -> Yojson.t
  val to_string : ?pretty:bool -> t' -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t' -> unit
  val update : t' -> Action.t -> States.t -> unit
  val destinations : t' -> States.t
  val reduce_by_label : t' -> Label.t -> t'
  val to_actions : t' -> Actions.t
  val to_actionpairs : t' -> ActionPairs.t
  val of_actionpairs : ActionPairs.t -> t'
  val merge : t' -> t' -> t'
end
