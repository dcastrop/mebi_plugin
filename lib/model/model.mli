(** This file provides {!Model.Make} which encapsulates everything necessary for the {b OCaml} models {i (i.e., with no {b Rocq} terms)}. This is unlike {!Mebi_plugin.Graph} that is used when building an initial model of {b Rocq} and then extracting it into a pure {b OCaml} model, using {!Encoding}.
*)

(** Type signature of {!Model}. *)
module type S = Model_intf.S

(** Builds a model from a term's [base] representation, plus a source of constructor-bindings used for the proof solver.
*)
module Make (Base : Base_term.S) (ConstructorBindings : Json.S) :
  S
  with type base = Base.t
   and type tree = Base.Tree.t
   and type trees = Base.Trees.t
   and type constructorbindings = ConstructorBindings.k

(** @author Jonah Pears *)
