(** The proof solver's entry points: the [MeBi Sim] commands ([Begin],
    [Step], [Solve]) and [MeBi Run Bisim ... As], over a solver for one
    encoding built per proof ({!Make}) and kept in a cache ({!make}). A step
    hands the goal in focus to {!Proof_solver_step}, which chooses the
    tactic. *)

(** Raised by {!S.step} (and {!step}): the proof has no goals left, so
    there is nothing to do. {!solve} stops on it. *)
exception NothingToDo

(** A proof solver for one encoding: the command-time results it reads
    ({!W}), the proof's state machine ({!ProofState}), and the step built
    afresh for each goal ({!Step}). *)
module type S = sig
  type enc
  type node
  type tree
  type trees

  (** Tactics with an optional message, sequenced and unpacked
      ({!Proof_solver_tactic}). *)
  module Tactic : Proof_solver_tactic.S

  (** The bisimilarity check's results, which the search follows. *)
  module W :
    Results.S
    with type enc = enc
     and type node = node
     and type tree = tree
     and type trees = trees

  (** The proof being solved and its state machine. *)
  module ProofState :
    Proof_solver_statem.S
    with type enc = enc
     and type node = node
     and type state = W.Model.State.t
     and type label = W.Model.Label.t
     and type annotation = W.Model.Annotation.t
     and type transition = W.Model.Transition.t

  (** One proof step, over the goal in focus given as [Args]. *)
  module Step : (_ : Proof_solver_wrapper.Args) ->
    Proof_solver_step.S with type tactic = Tactic.t

  (** [get_updated_pstate x] is the proof being solved
      ({!ProofState.get_pstate}) after the tactic [x] runs on it; a tactic
      Rocq reports as unsafe is logged as a warning.

      @raise Proof_solver_statem.S.NoStateFound
        if no proof is being solved (propagated). Also raises whatever [x]
        raises when it runs (propagated). *)
  val get_updated_pstate : unit Proofview.tactic -> Declare.Proof.t

  (** [step pstate] is [pstate] after one solver step: the tactic
      {!Proof_solver_step.S.step} chooses for the goal in focus, followed by
      [simpl in *] and [subst] everywhere. [pstate] becomes the proof being
      solved first.

      @raise NothingToDo
        if [pstate] has no goals left (raised here; the state machine is
        set to [Done] first). Also raises whatever the step raises
        (propagated). *)
  val step : Declare.Proof.t -> Declare.Proof.t
end

(** [Make (Enc)] is the proof solver for the encoding [Enc]. *)
module Make (Enc : Encoding.S) :
  S
  with type enc = Enc.t
   and type node = Enc.Tree.Node.t
   and type tree = Enc.Tree.t
   and type trees = Enc.Trees.t

(** The cached solver: the one {!make} built last, which every command of
    the proof then uses. *)
type t = { solver : (module S) }

(** Raised by {!is_done}, {!step} and {!solve}: no solver has been built
    ({!make}, run by {!init}). *)
exception NoCachedModules

(** [make (module Enc) ()] is the cache, set to a fresh solver for [Enc]
    ({!Make}). Raises nothing. *)
val make : (module Encoding.S) -> unit -> t ref

(** [is_done ()] is whether the cached solver's proof is finished
    ({!Proof_solver_statem.S.is_done}).

    @raise NoCachedModules if no solver has been built (propagated). *)
val is_done : unit -> bool

(** [init ?enc pstate refs (x, a) (y, b)] begins the proof search on
    [pstate] ([MeBi Sim Begin]), whose goal is [weak_sim a b x y] or
    [weak_bisimilar a b x y], and is [pstate] unchanged. It builds a fresh
    solver for [enc ()] ({!make}) and checks the two systems
    ({!Results.S.check_bisimilarity}, extracting with the LTSs [refs]).
    Then, before any proof step runs, it:
    - for a [weak_sim] goal whose states are not bisimilar, falls back on
      the greatest weak simulation ({!Results.S.simulators});
    - plans every answer ({!Results.S.plan}) when the answer policy is not
      [Default];
    - under [Auto], chooses the cofix strategy by estimating both
      ({!Model.Product.estimate}), and announces the mutual path;
    - starts the state machine at [NewProof] with [x] and [y].
      On an FSM saturated on demand, planning or estimating would walk the
      whole game, so it is done only within [MeBi Config Bounds Game], and a
      setting made explicitly ([MutualCofix True], an answer policy other
      than [Default]) is refused without that bound or past it.

    @raise CErrors.UserError
      if a [weak_sim] goal's left state is not even simulated by the right
      one while [FailIf NotBisimilar] is set, or an explicit whole-game
      setting is refused (raised here). Also raises as
      {!Results.S.check_bisimilarity} (propagated; among others, when a
      [weak_bisimilar] goal's states are not bisimilar). *)
val init
  :  ?enc:(unit -> (module Encoding.S))
  -> Declare.Proof.t
  -> Libnames.qualid list
  -> Constrexpr.constr_expr * Libnames.qualid
  -> Constrexpr.constr_expr * Libnames.qualid
  -> Declare.Proof.t

(** Which goal {!start} states: [weak_bisimilar] ([Bisim], for
    [MeBi Run Bisim ... As]) or [weak_sim] ([Sim]). *)
type goal_kind =
  | Bisim
  | Sim

(** [start ~kind ~name refs (x, a) (y, b)] is a new proof named [name] (an
    [Example]) of [weak_bisimilar a b x y] ([Bisim]) or [weak_sim a b x y]
    ([Sim]), with the proof search begun on it as
    [MeBi Sim Begin a x And b y Using refs] would ({!init}), so
    [MeBi Sim Solve] can follow at once.

    @raise CErrors.UserError
      if the statement does not typecheck (propagated from Rocq's
      interpretation). Also raises as {!init} (propagated; it refuses, and
      no proof is opened, if the two are not bisimilar, or not similar). *)
val start
  :  kind:goal_kind
  -> name:Names.Id.t
  -> Libnames.qualid list
  -> Constrexpr.constr_expr * Libnames.qualid
  -> Constrexpr.constr_expr * Libnames.qualid
  -> Declare.Proof.t

(** [step pstate] is [pstate] after one step of the cached solver
    ({!S.step}; [MeBi Sim Step]).

    @raise NoCachedModules if no solver has been built (propagated).
    @raise NothingToDo
      if [pstate] has no goals left (propagated). Also raises whatever the
      step raises (propagated). *)
val step : Declare.Proof.t -> Declare.Proof.t

(** [solve ?bound pstate] is [pstate] after the cached solver's steps
    ([MeBi Sim Solve]), until the proof is finished, there is nothing to do,
    or [bound + 1] steps have run ([bound] defaults to 10). It reports
    ["(Stopped) Solved after N iterations."] (or [Unsolved]), and, when it
    stops on the bound unsolved, which cofix strategy was in force and how
    to go on.

    @raise NoCachedModules
      if no solver has been built (propagated). Also
      raises whatever a step raises other than {!NothingToDo}
      (propagated). *)
val solve : ?bound:int -> Declare.Proof.t -> Declare.Proof.t

(** [guard f] is [f ()], run as a [MeBi Sim] command: a plugin exception
    nothing handled, which Rocq would report as an Anomaly in Rocq itself,
    becomes a user error that names it.

    @raise CErrors.UserError
      in place of a non-critical exception Rocq has no printer for, looked
      for through a tactic's wrapper (raised here). Other exceptions pass
      through unchanged (propagated). *)
val guard : (unit -> 'a) -> 'a
