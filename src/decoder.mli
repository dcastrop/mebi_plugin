module type S = sig
  type enc
  type state
  type states
  type partition
  type label
  type labels
  type note
  type annotation
  type annotations
  type transition
  type transitions
  type action
  type actions
  type actionmap
  type edgemap
  type rocqlts
  type info
  type lts
  type fsm
  type result
  type bisimilarity

  val enc : enc -> EConstr.t
  val handle : enc -> exn -> EConstr.t

  exception CouldNotDecode_State of state

  val state : state -> EConstr.t

  exception CouldNotDecode_Label of label

  val label : label -> EConstr.t

  exception CouldNotDecode_LTS_Constructor of rocqlts

  val lts_constructor : rocqlts -> EConstr.t

  module Base : Json.S with type k = enc
  module State : Json.S with type k = state
  module States : Json.S with type k = states
  module Partition : Json.S with type k = partition
  module Label : Json.S with type k = label
  module Labels : Json.S with type k = labels
  module Note : Json.S with type k = note
  module Annotation : Json.S with type k = annotation
  module Annotations : Json.S with type k = annotations
  module Transition : Json.S with type k = transition
  module Transitions : Json.S with type k = transitions
  module Action : Json.S with type k = action
  module ActionMap : Json.S with type k = actionmap
  module EdgeMap : Json.S with type k = edgemap
  module Info : Json.S with type k = info
  module LTS : Json.S with type k = lts
  module FSM : Json.S with type k = fsm
  module Result : Json.S with type k = result
  module Bisimilarity : Json.S with type k = bisimilarity
end

module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (M : Rocq_monad_utils.S with type enc = Enc.t)
    (ConstructorBindings : Constructor_bindings.S with type 'a mm = 'a M.mm)
    (Model :
       Model.S
       with type base = Enc.t
        and type tree = Enc.Tree.t
        and type trees = Enc.Trees.t
        and type constructorbindings = ConstructorBindings.t) :
  S
  with type enc = Enc.t
   and type state = Model.State.t
   and type states = Model.States.t
   and type partition = Model.Partition.t
   and type label = Model.Label.t
   and type labels = Model.Labels.t
   and type note = Model.Note.t
   and type annotation = Model.Annotation.t
   and type annotations = Model.Annotations.t
   and type transition = Model.Transition.t
   and type transitions = Model.Transitions.t
   and type action = Model.Action.t
   and type actions = Model.Actions.t
   and type actionmap = Model.ActionMap.t'
   and type edgemap = Model.EdgeMap.t'
   and type rocqlts = Model.Info.Meta.RocqLTS.t
   and type info = Model.Info.t
   and type lts = Model.LTS.t
   and type fsm = Model.FSM.t
   and type result = Model.Bisimilarity.Result.t
   and type bisimilarity = Model.Bisimilarity.t
