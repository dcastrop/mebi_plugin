(** The signatures of {!Results}, declared once for its [.ml] and [.mli]. *)

module type S = sig
  include Wrapper.S

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

  (** [set_the_result r] sets {!the_result} to [r].

      @raise CannotOverrideResult if one is already set (raised here). *)
  val set_the_result : Model.Bisimilarity.t -> unit

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

end
