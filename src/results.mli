module type S = sig
  include Wrapper.S

  (* module Command :
     Command.S
     with type weak = Weak.t
     and type 'a mm = 'a M.mm
     and type lts = Model.LTS.t
     and type fsm = Model.FSM.t
     and type bisimilarity = Model.Bisimilarity.t
     and type result = Model.Bisimilarity.Result.t *)

  val the_result : Decode.bisimilarity ref option ref

  exception NoResultFound

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

  exception CannotOverrideResult of Model.Bisimilarity.t

  val set_the_result : Model.Bisimilarity.t -> unit

  exception BisimilarityResultNotFound

  (** For a [weak_sim] goal whose two states are not bisimilar: each left
      state's simulators ({!Model.Product.simulation}), the answers the
      solver falls back on when no bisimilar one exists. [None] otherwise. *)
  val simulators : (Model.State.t -> Model.State.Set.t) option ref

  val check_bisimilarity
    :  ?fail_if_not_bisim:bool
    -> Libnames.qualid list
    -> Constrexpr.constr_expr * Libnames.qualid
    -> Constrexpr.constr_expr * Libnames.qualid
    -> unit

  val get_bisimilar_partition : unit -> Model.Partition.t

  val get_bisimilar_states
    :  ?pi:Model.Partition.t
    -> Model.State.t
    -> Model.State.Set.t

  val are_states_bisimilar : Model.State.t -> Model.State.t -> bool

  (* val get_candidates : Model.State.t -> Model.Label.t -> Model.EdgeMap.t' -> Model.State.t -> Model.State.Set.t *)
end

module Make (Enc : Encoding.S) :
  S
  with type enc = Enc.t
   and type node = Enc.Tree.Node.t
   and type tree = Enc.Tree.t
   and type trees = Enc.Trees.t
