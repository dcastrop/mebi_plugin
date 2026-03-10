module Make : (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type t = Base.t)
    (States : States.S with type elt = State.t)
    (Label : Label.S with type t = Base.t Label.t')
    (Annotation : sig
       type t
     end)
    (Transition : Transition.S
                  with type t =
                    (State.t, Label.t, Base.Tree.t, Annotation.t) Transition.t')
    (Transitions : sig
       include Set.S with type elt = Transition.t
     end)
    (Action : sig
       type t =
         { label : Label.t
         ; annotation : Annotation.t option
         ; constructor_trees : Base.Trees.t
         }
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
    (ActionMap : sig
       include Hashtbl.S with type key = Action.t

       type t' = States.t t

       val json : ?as_elt:bool -> t' -> Yojson.t
       val update : t' -> Action.t -> States.t -> unit
       val destinations : t' -> States.t
       val reduce_by_label : t' -> Label.t -> t'
       val to_actionpairs : t' -> ActionPairs.t
       val of_actionpairs : ActionPairs.t -> t'
       val merge : t' -> t' -> t'
     end)
    (Edge : sig
       type t =
         { from : State.t
         ; goto : State.t
         ; action : Action.t
         }
     end)
    (Edges : sig
       include Set.S with type elt = Edge.t
     end)
    -> sig
  include Hashtbl.S with type key = State.t

  type t' = ActionMap.t' t

  val json : ?as_elt:bool -> t' -> Yojson.t
  val to_string : ?pretty:bool -> t' -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t' -> unit
  val update : t' -> State.t -> Action.t -> States.t -> unit
  val destinations : t' -> State.t -> States.t
  val get_actions : t' -> State.t -> Actions.t
  val reduce_by_label : t' -> Label.t -> t'
  val get_edges : t' -> State.t -> Edges.t
  val to_edges : t' -> Edges.t
  val of_edges : Edges.t -> t'
  val of_transitions : Transitions.t -> t'
  val merge : t' -> t' -> t'
end
