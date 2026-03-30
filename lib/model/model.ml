module type S = sig
  type base
  type tree
  type trees
  type constructorbindings

  module State : State.S with type base = base
  module States : States.S with type elt = State.t
  module Label : Label.S with type base = base
  module Labels : Labels.S with type elt = Label.t

  module Note :
    Annotation_note.S
    with type state = State.t
     and type label = Label.t
     and type trees = trees

  module Annotation :
    Annotation.S with type label = Label.t and type note = Note.t

  module Annotations : Annotations.S with type elt = Annotation.t

  module Transition :
    Transition.S
    with type state = State.t
     and type label = Label.t
     and type tree = tree
     and type annotation = Annotation.t

  module Transitions :
    Transitions.S with type elt = Transition.t and type labels = Labels.t

  module Action :
    Action.S
    with type label = Label.t
     and type annotation = Annotation.t
     and type trees = trees

  module Actions :
    Actions.S
    with type elt = Action.t
     and type label = Label.t
     and type labels = Labels.t

  module ActionPair :
    Actionpair.S with type action = Action.t and type states = States.t

  module ActionPairs :
    Actionpairs.S with type states = States.t and type elt = ActionPair.t

  module ActionMap :
    Actionmap.S
    with type label = Label.t
     and type action = Action.t
     and type actions = Actions.t
     and type states = States.t
     and type actionpairs = ActionPairs.t

  module Edge :
    Edge.S
    with type state = State.t
     and type label = Label.t
     and type action = Action.t

  module Edges : Edges.S with type elt = Edge.t and type label = Edge.label

  module EdgeMap :
    Edgemap.S
    with type state = State.t
     and type states = States.t
     and type label = Label.t
     and type transitions = Transitions.t
     and type action = Action.t
     and type actions = Actions.t
     and type actionmap = ActionMap.t'
     and type edges = Edges.t

  module Partition :
    State_partition.S
    with type elt = States.t
     and type state = State.t
     and type label = Label.t
     and type edgemap = EdgeMap.t'

  module Info :
    Info.S
    with type base = base
     and type constructorbindings = constructorbindings
     and type labels = Labels.t

  module LTS :
    LTS.S
    with type state = State.t
     and type states = States.t
     and type labels = Labels.t
     and type transitions = Transitions.t
     and type info = Info.t

  module FSM :
    FSM.S
    with type state = State.t
     and type states = States.t
     and type labels = Labels.t
     and type edgemap = EdgeMap.t'
     and type info = Info.t
     and type lts = LTS.t

  module Minimize :
    Minimize.S
    with type state = State.t
     and type states = States.t
     and type label = Label.t
     and type labels = Labels.t
     and type edgemap = EdgeMap.t'
     and type partition = Partition.t
     and type fsm = FSM.t

  module Bisimilarity :
    Bisimilarity.S
    with type states = States.t
     and type partition = Partition.t
     and type fsm = FSM.t
end

module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (ConstructorBindings : Constructor_bindings.S) :
  S
  with type base = Base.t
   and type tree = Base.Tree.t
   and type trees = Base.Trees.t
   and type constructorbindings = ConstructorBindings.t = struct
  type base = Base.t
  type tree = Base.Tree.t
  type trees = Base.Trees.t
  type constructorbindings = ConstructorBindings.t

  (** [module State] ... *)
  module State = State.Make (Log) (Base)

  (** [module States] is an extended [Set.S] of [State.t]. *)
  module States = States.Make (Log) (State)

  (** [module Label] ... *)
  module Label = Label.Make (Log) (Base)

  (** [module Labels] is an extended [Set.S] of [Label.t]. *)
  module Labels = Labels.Make (Log) (Label)

  (** [module Note] ... *)
  module Note = Annotation_note.Make (Log) (Base) (State) (Label)

  (** [module Annotation] ... *)
  module Annotation = Annotation.Make (Log) (Base) (Label) (Note)

  (** [module Annotations] is an extended [Set.S] of [Annotation.t]. *)
  module Annotations = Annotations.Make (Log) (Note) (Annotation)

  (** [module Transition] ... *)
  module Transition = Transition.Make (Log) (Base) (State) (Label) (Annotation)

  (** [module Transitions] is an extended [Set.S] of [Transition.t]. *)
  module Transitions = Transitions.Make (Log) (Labels) (Transition)

  (** [module Action] ... *)
  module Action = Action.Make (Log) (Base) (Label) (Annotation)

  (** [module Actions] is an extended [Set.S] of [Action.t]. *)
  module Actions = Actions.Make (Log) (Label) (Labels) (Action)

  (** [module ActionPair] ... *)
  module ActionPair =
    Actionpair.Make (Log) (Base) (States) (Annotation) (Action)

  (** [module ActionPairs] is an extended [Set.S] of [ActionPair.t]. *)
  module ActionPairs = Actionpairs.Make (Log) (States) (Action) (ActionPair)

  (** [module ActionMap] is a [Hashtbl.S] for mapping an [Action.t] to set of destination [States.t].
  *)
  module ActionMap =
    Actionmap.Make (Log) (Base) (States) (Label) (Action) (Actions)
      (ActionPairs)

  (** [module Edge] ... *)
  module Edge = Edge.Make (Log) (State) (Label) (Action)

  module Edges = Edges.Make (Log) (Edge)

  (** [module EdgeMap] ... *)
  module EdgeMap =
    Edgemap.Make (Log) (Base) (State) (States) (Transition) (Transitions)
      (Action)
      (Actions)
      (ActionPairs)
      (ActionMap)
      (Edge)
      (Edges)

  (** [module Partition] ... *)
  module Partition =
    State_partition.Make (Log) (State) (States) (ActionMap) (EdgeMap)

  (** [module Info] ... *)
  module Info = Info.Make (Log) (Base) (Labels) (ConstructorBindings)

  (** [module LTS] ... *)
  module LTS = LTS.Make (Log) (State) (States) (Labels) (Transitions) (Info)

  (** [module Saturate] ...
      (* TODO: the idea of [Traces] needs to be revisited. It does provide optimizations to examples with a lot of silent actions, where the saturated FSM is considerably larger, but i believe that there are areas where this can still be improved. *)
  *)
  module Saturate =
    Saturate.Make (Log) (Base) (State) (States) (Label) (Labels) (Note)
      (Annotation)
      (Annotations)
      (Action)
      (ActionPair)
      (ActionPairs)
      (ActionMap)
      (EdgeMap)

  (** [module FSM] ... *)
  module FSM =
    FSM.Make (Log) (State) (States) (Labels) (EdgeMap) (Info) (LTS) (Saturate)

  (** [module Minimize] ... *)
  module Minimize =
    Minimize.Make (Log) (Base) (State) (States) (Label) (Labels) (Action)
      (ActionMap)
      (EdgeMap)
      (Partition)
      (Info)
      (FSM)

  (** [module Bisimilarity] ... *)
  module Bisimilarity =
    Bisimilarity.Make (Log) (State) (States) (Label) (Labels) (Action)
      (ActionMap)
      (EdgeMap)
      (Partition)
      (Info)
      (FSM)
      (Minimize)
end
