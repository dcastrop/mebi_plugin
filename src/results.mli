module type S = sig
  include Wrapper.S

  (** The bisimilarity result the proof now being solved works from, set by
      {!check_bisimilarity}. *)
  val the_result : Decode.bisimilarity ref option ref

  (** Raised by {!get_the_result}: no result has been set. *)
  exception NoResultFound

  (** [get_the_result ()] is {!the_result}.

      @raise NoResultFound if none is set (raised here). *)
  val get_the_result : unit -> Model.Bisimilarity.t

  (** Whether the two systems' roles are swapped for the goal in focus: [true]
      while answering the right-hand obligation ([bisim_r]) of a
      [weak_bisimilar] goal. {!get_fsm_a} and {!get_fsm_b} follow it. *)
  val swapped : bool ref

  (** FSM "a": the system whose move is being answered -- the left one,
      unless {!swapped}. *)
  val get_fsm_a : ?saturated:bool -> unit -> Model.FSM.t

  (** FSM "b": the system that answers -- the right one, unless
      {!swapped}. *)
  val get_fsm_b : ?saturated:bool -> unit -> Model.FSM.t

  (** Raised by {!set_the_result}: a result is already set. *)
  exception CannotOverrideResult of Model.Bisimilarity.t

  (** [set_the_result r] sets {!the_result} to [r].

      @raise CannotOverrideResult if one is already set (raised here). *)
  val set_the_result : Model.Bisimilarity.t -> unit

  (** Raised by {!check_bisimilarity}: the check gave no result. *)
  exception BisimilarityResultNotFound

  (** For a [weak_sim] goal whose two states are not bisimilar: each left
      state's simulators ({!Model.Product.simulation}), the answers the
      solver falls back on when no bisimilar one exists. [None] otherwise. *)
  val simulators : (Model.State.t -> Model.State.Set.t) option ref

  (** The answer plan for the proof now being solved, when the answer policy
      is not [Default] ({!Model.Product.Policy.type-plan}); [None] otherwise, and
      the solver answers with {!Model.Product.val-answer}. *)
  val plan : Model.Product.Policy.plan option ref

  (** [check_bisimilarity ?fail_if_not_bisim using (t1, lts1) (t2, lts2)]
      sets {!the_result} to the bisimilarity check of the two systems
      ({!Command.run} on [CheckBisim]), with the encoding table emptied
      first. With [fail_if_not_bisim] unset (it is set by default), a
      negative verdict is not an error whatever [FailIf] says: for a
      [weak_sim] goal, where bisimilarity is not what is asked.

      @raise BisimilarityResultNotFound
        if the check gives no result
        (raised here).
      @raise CannotOverrideResult
        if a result is already set (propagated
        from {!set_the_result}).
        Also raises whatever {!Command.run} raises (propagated). *)
  val check_bisimilarity
    :  ?fail_if_not_bisim:bool
    -> Libnames.qualid list
    -> Constrexpr.constr_expr * Libnames.qualid
    -> Constrexpr.constr_expr * Libnames.qualid
    -> unit

  (** [get_bisimilar_partition ()] is the bisimilarity classes of
      {!the_result}.

      @raise NoResultFound if none is set (propagated). *)
  val get_bisimilar_partition : unit -> Model.Partition.t

  (** [get_bisimilar_states ?pi s] is the class of [pi] (by default
      {!get_bisimilar_partition}) holding [s], or none if no class does.

      @raise NoResultFound
        if [pi] is not given and no result is set
        (propagated). *)
  val get_bisimilar_states
    :  ?pi:Model.Partition.t
    -> Model.State.t
    -> Model.State.Set.t
end

module Make (Enc : Encoding.S) :
  S
  with type enc = Enc.t
   and type node = Enc.Tree.Node.t
   and type tree = Enc.Tree.t
   and type trees = Enc.Trees.t
