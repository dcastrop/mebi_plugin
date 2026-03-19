exception NothingToDo

module type S = sig
  type tactic

  include Proof_solver_wrapper.S

  module Theory :
    Proof_solver_theory.S with type enc = enc and type 'a im = 'a mm

  module Tacs :
    Proof_solver_tactics.S
    with type 'a mm = 'a mm
     and type enc = enc
     and type tactic = tactic
     and type econstrset = EConstrSet.t

  val step : unit -> tactic
end

module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (Tactic : Proof_solver_tactic.S)
    (W :
       Wrapper_results.S
       with type enc = Enc.t
        and type node = Enc.Tree.Node.t
        and type tree = Enc.Tree.t
        and type trees = Enc.Trees.t)
    (ProofState :
       Proof_solver_statem.S
       with type enc = Enc.t
        and type node = Enc.Tree.Node.t
        and type state = W.Model.State.t
        and type label = W.Model.Label.t
        and type annotation = W.Model.Annotation.t
        and type transition = W.Model.Transition.t)
    (TheoryMaker : (I : Proof_solver_wrapper.S
                        with type enc = Enc.t
                         and type tree = Enc.Tree.t) ->
       Proof_solver_theory.S
       with type 'a mm = 'a W.M.mm
        and type 'a im = 'a I.mm
        and type enc = Enc.t
        and type fsm = W.Model.FSM.t)
    (X : Proof_solver_wrapper.Args) :
  S with type enc = Enc.t and type tree = Enc.Tree.t and type tactic = Tactic.t
