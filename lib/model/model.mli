(** This file provides {!Model.Make} which encapsulates everything necessary for the {b OCaml} models {i (i.e., with no {b Rocq} terms)}. This is unlike {!Mebi_plugin.Graph} that is used when building an initial model of {b Rocq} and then extracting it into a pure {b OCaml} model, using {!Encoding}.
*)

(** Type signature of {!Model}. A *)
module type S = sig
  (** {1 Signature Types} *)

  (** A {!Base_term.S.t} used by the models for {!State.t} and {!Label.t}. Must have functions for equality, comparison and hashing. Can be derived from an {!Encoding.S}.
  *)
  type base

  (** A {!Tree.S.t} of {!base}. *)
  type tree

  (** A {!Trees.S.t} {i (i.e., a [Set.S] of {!tree})}. *)
  type trees

  (** [type t] of {!Constructor_bindings.S}. Only used in {!Info.Meta.t} to store information {i (in {!Info.Meta.RocqLTS.t})} that will be necessary when solving the {b Rocq} proofs in {!Mebi_plugin.Proof_solver}. {i {b Note:} This is the {b only} piece of information relating to {b Rocq} that makes it's way into this module, which we allow since since a model's [info.meta] field is always optional.}
  *)
  type constructorbindings

  (** {1 Model Components} *)

  (** {2 States} *)

  (** A {!State} is essnetially just {!base}. *)
  module State : State.S with type base = base

  (** {!States} is a [Set.S] of {!State.t} and contains other useful functions.
  *)
  module States : States.S with type elt = State.t

  (** {2 Labels} *)

  (** A {!Label.t} has a {!base} in addition to {!Label.t.is_silent}. *)
  module Label : Label.S with type base = base

  (** {!Labels} is a [Set.S] of {!Label.t} and contains other useful functions.
  *)
  module Labels : Labels.S with type elt = Label.t

  (** {2 Actions, Transition & Edges} *)

  (** {3 Annotations} *)

  module Note :
    Annotation_note.S
    with type state = State.t
     and type label = Label.t
     and type trees = trees

  module Annotation :
    Annotation.S with type label = Label.t and type note = Note.t

  (** {!Annotations} is a [Set.S] of {!Annotation.t} and contains other useful functions.
  *)
  module Annotations : Annotations.S with type elt = Annotation.t

  (** {3 Transitions} *)

  (** {!Transition} ... *)
  module Transition :
    Transition.S
    with type state = State.t
     and type label = Label.t
     and type tree = tree
     and type annotation = Annotation.t

  (** {!Transitions} is a [Set.S] of {!Transition.t} and contains other useful functions.
  *)
  module Transitions :
    Transitions.S with type elt = Transition.t and type labels = Labels.t

  (** {3 Actions} *)

  (** {!Action} ... *)
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

  (** {4 ActionPairs} *)

  (** {!ActionPair} ... *)
  module ActionPair :
    Actionpair.S with type action = Action.t and type states = States.t

  module ActionPairs :
    Actionpairs.S with type states = States.t and type elt = ActionPair.t

  (** {4 ActionMap} *)

  (** {!ActionMap} ... *)
  module ActionMap :
    Actionmap.S
    with type label = Label.t
     and type action = Action.t
     and type actions = Actions.t
     and type states = States.t
     and type actionpairs = ActionPairs.t

  (** {3 Edges} *)

  (** {!Edge} ... *)
  module Edge :
    Edge.S
    with type state = State.t
     and type label = Label.t
     and type action = Action.t

  module Edges : Edges.S with type elt = Edge.t and type label = Edge.label

  (** {4 EdgeMap} *)

  (** {!EdgeMap} ... *)
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

  (** {2 State Partitions} *)

  (** {!Partition} ... *)
  module Partition :
    State_partition.S
    with type elt = States.t
     and type state = State.t
     and type label = Label.t
     and type edgemap = EdgeMap.t'

  (** {2 Info} *)

  (** {!Info} ... *)
  module Info :
    Info.S
    with type base = base
     and type constructorbindings = constructorbindings
     and type labels = Labels.t

  (** {1 Models} *)

  (** {2 LTS} *)

  (** {!LTS} has an *)
  module LTS :
    LTS.S
    with type state = State.t
     and type states = States.t
     and type labels = Labels.t
     and type transitions = Transitions.t
     and type info = Info.t

  (** {2 FSM} *)

  (** {!FSM} ... *)
  module FSM :
    FSM.S
    with type state = State.t
     and type states = States.t
     and type labels = Labels.t
     and type edgemap = EdgeMap.t'
     and type info = Info.t
     and type lts = LTS.t

  (** {1 Algorithms} *)

  (** {2 Saturation} *)

  (** {!Saturation} provides {!Saturation.edges} which returns a saturated {!EdgeMap.t'} and a {!States.t} of now-terminating states. {i {b Note:} See {!FSM.saturate}.}
  *)
  module Saturation :
    Saturation.S
    with type state = State.t
     and type states = States.t
     and type labels = Labels.t
     and type edgemap = EdgeMap.t'
  (** {i See {!FSM.saturate}.} *)

  (** {2 Minimization} *)

  (** {!Minimization} provides {!val:Minimization.fsm} which returns the result of a given {!FSM.t} after being first {i saturated (by {!FSM.saturate})} and then {i minimized}. Minimization involves splitting the states of the {!FSM.t} into {i state-partitions ({!Partition.t})} where states are grouped such that those within the same partition are each able to perform the same actions, where each action reaches a destination state in the same state-partition as the others in their partition.
  *)
  module Minimization :
    Minimization.S
    with type state = State.t
     and type states = States.t
     and type label = Label.t
     and type labels = Labels.t
     and type edgemap = EdgeMap.t'
     and type partition = Partition.t
     and type fsm = FSM.t

  (** {2 Bisimilarity} *)

  (** {!Bisimilarity} provides {!val:Bisimilarity.fsm} which takes two {!FSM.t}s and: (i) saturates them both (using {!FSM.saturate}); (ii) merges them into a single {!FSM.t} (using {!FSM.merge}); (iii) minimizes the merged FSM; and, (iv) splits the resulting {!Partition.t} into two {!States.t}, one containing states that originated in {b {i both}} FSMs {i (hence are {b bisimilar states})}, and another for {i non-bisimilar states} that only originate from a single state.
  *)
  module Bisimilarity :
    Bisimilarity.S
    with type states = States.t
     and type partition = Partition.t
     and type fsm = FSM.t
end

(** Make A *)
module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (ConstructorBindings : Constructor_bindings.S) :
  S
  with type base = Base.t
   and type tree = Base.Tree.t
   and type trees = Base.Trees.t
   and type constructorbindings = ConstructorBindings.t
(** Make B *)

(** @author Jonah Pears *)
