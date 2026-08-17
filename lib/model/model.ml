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

  module Saturation :
    Saturation.S
    with type state = State.t
     and type states = States.t
     and type labels = Labels.t
     and type edgemap = EdgeMap.t'

  module Minimization :
    Minimization.S
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

module Make (Base : Base_term.S) (ConstructorBindings : Json.S) :
  S
  with type base = Base.t
   and type tree = Base.Tree.t
   and type trees = Base.Trees.t
   and type constructorbindings = ConstructorBindings.k = struct
  type base = Base.t
  type tree = Base.Tree.t
  type trees = Base.Trees.t
  type constructorbindings = ConstructorBindings.k

  module State = State.Make (Base)
  module States = States.Make (State)
  module Label = Label.Make (Base)
  module Labels = Labels.Make (Label)
  module Note = Annotation_note.Make (Base) (State) (Label)
  module Annotation = Annotation.Make (Base) (Label) (Note)
  module Annotations = Annotations.Make (Note) (Annotation)
  module Transition = Transition.Make (Base) (State) (Label) (Annotation)
  module Transitions = Transitions.Make (Labels) (Transition)
  module Action = Action.Make (Base) (Label) (Annotation)
  module Actions = Actions.Make (Label) (Labels) (Action)
  module ActionPair = Actionpair.Make (Base) (States) (Annotation) (Action)
  module ActionPairs = Actionpairs.Make (States) (Action) (ActionPair)

  module ActionMap =
    Actionmap.Make (Base) (States) (Label) (Action) (Actions) (ActionPairs)

  module Edge = Edge.Make (State) (Label) (Action)
  module Edges = Edges.Make (Edge)

  module EdgeMap =
    Edgemap.Make (Base) (State) (States) (Transition) (Transitions) (Action)
      (Actions)
      (ActionPairs)
      (ActionMap)
      (Edge)
      (Edges)

  module Partition = State_partition.Make (State) (States) (ActionMap) (EdgeMap)
  module Info = Info.Make (Base) (Labels) (ConstructorBindings)
  module LTS = LTS.Make (State) (States) (Labels) (Transitions) (Info)

  (* TODO: the idea of [Traces] needs to be revisited. It does provide optimizations to examples with a lot of silent actions, where the saturated FSM is considerably larger, but i believe that there are areas where this can still be improved. *)
  module Saturation =
    Saturation.Make (Base) (State) (States) (Label) (Labels) (Note) (Annotation)
      (Annotations)
      (Action)
      (ActionPair)
      (ActionPairs)
      (ActionMap)
      (EdgeMap)

  module FSM =
    FSM.Make (State) (States) (Labels) (EdgeMap) (Info) (LTS) (Saturation)

  module Minimization =
    Minimization.Make (Base) (State) (States) (Label) (Labels) (Action)
      (ActionMap)
      (EdgeMap)
      (Partition)
      (Info)
      (FSM)

  module Bisimilarity =
    Bisimilarity.Make (State) (States) (Label) (Labels) (Action) (ActionMap)
      (EdgeMap)
      (Partition)
      (Info)
      (FSM)
      (Minimization)
end
