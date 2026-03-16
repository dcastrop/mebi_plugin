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
   and type constructorbindings = ConstructorBindings.t
