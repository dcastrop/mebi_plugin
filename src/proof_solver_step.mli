(** One step of the proof search: reading the goal in focus and the proof's
    state machine ({!Proof_solver_statem}), the tactic to run next. Built
    afresh for each goal by {!Proof_solver.Make}. *)

(** Raised by {!S.step}: the state machine is [Done], so there is nothing
    to do. {!Proof_solver.NothingToDo} is the same exception, which
    {!Proof_solver.solve} stops on. *)
exception NothingToDo

(** A proof step over one goal: its monad and helpers
    ({!Proof_solver_wrapper.S}), the theory and the tactics read through it,
    and {!step}. *)
module type S = sig
  type tactic

  include Proof_solver_wrapper.S

  (** The plugin's theory ([weak_sim], [weak], ...) as read in this goal. *)
  module Theory :
    Proof_solver_theory.S with type enc = enc and type 'a im = 'a mm

  (** The tactics the step chooses from. *)
  module Tacs :
    Proof_solver_tactics.S
    with type 'a mm = 'a mm
     and type enc = enc
     and type tactic = tactic
     and type econstrset = EConstrSet.t

  (** [step ()] is the tactic for the goal in focus, chosen by the proof's
      state: [NewProof] unfolds the two systems; [OpenBlock] opens the mutual
      cofix block; [WeakSim] closes the goal by reflexivity or a coinduction
      hypothesis, or introduces one, else inverts or unfolds a hypothesis;
      [Exists] reads the move from the hypotheses and answers it;
      [ApplyConstructors] applies the answer's next constructor. The state
      machine is updated for the next step as it goes, and a state with nothing
      to do here passes to the next state within the same step.

      @raise NothingToDo if the state is [Done] (raised here).

      @raise CErrors.UserError
        if the goal is one the solver cannot continue
        from: a pair outside the mutual block, a premise it cannot prove, a
        constructor whose goal is not an LTS step (raised here). Also raises the
        step's internal failures, such as no hypothesis reading as a transition
        (raised here or propagated; {!Proof_solver.guard} reports them). *)
  val step : unit -> tactic
end

(** [Make (Enc) (Tactic) (W) (ProofState) (TheoryMaker) (X)] is the proof
    step for the goal [X.gl], over the bisimilarity result [W] and the
    proof's state machine [ProofState]. *)
module Make
    (Enc : Encoding.S)
    (Tactic : Proof_solver_tactic.S)
    (W :
       Results.S
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
