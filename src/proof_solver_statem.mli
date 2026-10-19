module type S = sig
  type enc
  type node
  type state
  type label
  type annotation
  type transition

  (** The constructors still to apply to answer one step: the label and
      destination to reach, the constructors of the strong step being applied
      ([current]), the steps of the witness still to go ([remaining]), and the
      strong step's target, once bound ([step_goto]). *)
  module ApplicableConstructors : sig
    (** A constructor tree's nodes, the constructors to apply in turn. *)
    module Nodes : sig
      type t = node list

      include Json.S with type k = t (** @closed *)
    end

    type t =
      { label : label
      ; destination : state
      ; current : Nodes.t option
      ; remaining : annotation option
      ; step_goto : state option
      }

    include Json.S with type k = t (** @closed *)

    (** Raised by {!init}: the transition has neither a witness nor a
        derivation tree. *)
    exception TransitionHasNoConstructorsToApply

    (** [init t] is what is left to apply to take the transition [t]: its
        witness, or, for a strong transition, the one step its derivation
        tree gives.

        @raise TransitionHasNoConstructorsToApply
          if [t] has neither (raised here). *)
    val init : transition -> t
  end

  (** The proof solver's state machine. Some states take several steps,
      because the proof must be updated by one tactic before the next can be
      chosen (constructors are applied one at a time).
      - [NewProof]: the start; unfold what can be unfolded, then [WeakSim].
      - [OpenBlock]: open the bisimulation's pair of obligations.
      - [WeakSim]: close the goal by a coinduction hypothesis, or introduce
        a new one; stay to invert or unfold hypotheses, or go to [Exists] or
        [Done].
      - [Exists t]: the conclusion is [exists a n2]: (1) read FSM a's step
        from the hypotheses (inverting first if needed), (2) choose FSM b's
        answer, (3) instantiate [n2] ([ex_intro], [split]). A silent answer
        that stays put re-enters [Exists]; otherwise go to
        [ApplyConstructors].
      - [ApplyConstructors]: apply the constructors that take FSM b to a state
        bisimilar to FSM a's.
      - [Done]: the proof is finished. *)
  module StateM : sig
    type t =
      | Done
      | NewProof of (Constrexpr.constr_expr * Constrexpr.constr_expr)
      | OpenBlock
      | WeakSim
      | Exists of transition option
      | ApplyConstructors of ApplicableConstructors.t

    include Json.S with type k = t (** @closed *)
  end

  (** The proof being solved and its state. *)
  type t =
    { p : Declare.Proof.t
    ; x : StateM.t
    }

  (** The proof being solved, if any. *)
  val the_state : t ref option ref

  (** Raised when no proof is being solved. *)
  exception NoStateFound

  (** [get ()] is {!the_state}.

      @raise NoStateFound if no proof is being solved (raised here). *)
  val get : unit -> t ref

  (** [set p x] makes [p], in the state [x], the proof being solved. Raises
      nothing. *)
  val set : Declare.Proof.t -> StateM.t -> unit

  (** [init p (t1, t2)] starts solving [p], a proof about the terms [t1]
      and [t2], in the state [NewProof]. Raises nothing. *)
  val init
    :  Declare.Proof.t
    -> Constrexpr.constr_expr * Constrexpr.constr_expr
    -> unit

  (** [get_pstate ()] is the proof being solved.

      @raise NoStateFound if none (propagated). *)
  val get_pstate : unit -> Declare.Proof.t

  (** [get_statem ()] is its state.

      @raise NoStateFound if none (propagated). *)
  val get_statem : unit -> StateM.t

  (** [update_pstate p] replaces the proof being solved by [p], keeping its
      state.

      @raise NoStateFound if none (propagated). *)
  val update_pstate : Declare.Proof.t -> unit

  (** [update_statem x] sets the state of the proof being solved to [x].

      @raise NoStateFound if none (propagated). *)
  val update_statem : StateM.t -> unit

  (** [is_done ()] is whether the proof is finished: its state is [Done],
      or Rocq reports no goals left.

      @raise NoStateFound if none (propagated). *)
  val is_done : unit -> bool

  (** [log ()] logs the state at [Debug].

      @raise NoStateFound if none (propagated). *)
  val log : ?__FUNCTION__:string -> unit -> unit
end

module Make
    (Enc : Encoding.S)
    (W :
       Results.S
       with type enc = Enc.t
        and type node = Enc.Tree.Node.t
        and type tree = Enc.Tree.t
        and type trees = Enc.Trees.t) :
  S
  with type enc = Enc.t
   and type node = Enc.Tree.Node.t
   and type state = W.Model.State.t
   and type label = W.Model.Label.t
   and type annotation = W.Model.Annotation.t
   and type transition = W.Model.Transition.t
