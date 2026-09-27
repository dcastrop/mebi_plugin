(** This file provides {!Model.Make} which encapsulates everything necessary for the {b OCaml} models {i (i.e., with no {b Rocq} terms)}. This is unlike {!Mebi_plugin.Graph} that is used when building an initial model of {b Rocq} and then extracting it into a pure {b OCaml} model, using {!Encoding}.
*)

(** Type signature of {!Model}. *)
module type S = sig
  (** @canonical Model.S *)

  (** {1 Signature Types} *)

  (** A {!Base_term.S.t} used by the models for {!Components.S.State.t} and {!Components.S.Label.t}. Must have functions for equality, comparison and hashing. Can be derived from an {!Encoding.S}.
  *)
  type base

  (** A {!Tree.S.t} of {!base}. *)
  type tree

  (** A {!Trees.S.t} {i (i.e., a [Set.S] of {!tree})}. *)
  type trees

  (** [type t] of {!Constructor_bindings.S}. Only used in {!Components.S.Info.Meta.t} to store information {i (in {!Components.S.Info.Meta.RocqLTS.t})} that will be necessary when solving the {b Rocq} proofs in {!Mebi_plugin.Proof_solver}. {i {b Note:} This is the {b only} piece of information relating to {b Rocq} that makes it's way into this module, which we allow since since a model's [info.meta] field is always optional.}
  *)
  type constructorbindings

  (** {1 Model Components}

      [State], [States], [Label], [Labels], [Note], [Annotation],
      [Annotations], [Transition], [Transitions], [Action], [Actions],
      [ActionPair], [ActionPairs], [ActionMap], [Edge], [Edges], [EdgeMap],
      [Partition] and [Info] -- see {!Components.S} for each.
  *)
  include
    Components.S
    with type base := base
     and type tree := tree
     and type trees := trees
     and type constructorbindings := constructorbindings

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

(** Builds a model from a term's [base] representation, plus a source of constructor-bindings used for the proof solver. *)
module Make (Base : Base_term.S) (ConstructorBindings : Json.S) :
  S
  with type base = Base.t
   and type tree = Base.Tree.t
   and type trees = Base.Trees.t
   and type constructorbindings = ConstructorBindings.k

(** @author Jonah Pears *)
