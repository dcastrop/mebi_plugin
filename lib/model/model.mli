(** This file provides [module Model] which encapsulates everything necessary for the {b OCaml} models {i (i.e., with no {b Rocq} terms)}. This is unlike [module Graph] that is used when building an initial model of {b Rocq} and then extracting it into a pure {b OCaml} model, using the [module Encoding].
*)

(** Type signature of [module Model]. *)
module type S = sig
  (** The base unit used by the model {i (e.g., [int])}. Used for [State.t] and [Label.t]. Must have functions for equality, comparison and hashing. Can be derived from an [Encoding.S].
  *)
  type base

  (** A [Tree.t] of [type base]. *)
  type tree

  (** A [Set.S with type elt = tree]. *)
  type trees

  (** [type t] of [Constructor_bindings.S]. Only used in [module Info] to store information that will be necessary when solving the {b Rocq} proofs in [module Proof_solver]. {i (This is the {b only} piece of information relating to {b Rocq} that makes it's way into this module, which we allow since since a model's [info] field is optional.)}
  *)
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
