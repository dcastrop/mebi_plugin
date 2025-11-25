val do_nothing : unit -> unit Proofview.tactic
val do_simplify : Proofview.Goal.t -> unit Proofview.tactic
val do_rt1n_refl : Proofview.Goal.t -> unit Proofview.tactic

module PState : sig
  type t =
    | NewProof
    | NewWeakSim
    | NewCofix
    | NewTransition of Model_transition_opt.t
    | GoalTransition of Model_transition_opt.t
    | ApplyConstructors of applicable_constructors

  and applicable_constructors =
    { annotation : Model_note.annotation
    ; tactics : tactic_to_apply list option
    }

  and tactic_to_apply = unit -> unit Proofview.tactic

  val to_string : t -> string
end

val default_proof_state : PState.t
val the_proof_state : PState.t ref
val reset_the_proof_state : unit -> unit
val mfsm : unit -> Model.Fsm.t
val nfsm : unit -> Model.Fsm.t

module type HYP_S = sig
  type atomic_pair = Evd.econstr * Evd.econstr array

  exception
    Mebi_proof_HypIsNot_Atomic of
      (Evd.evar_map * Rocq_utils.hyp * EConstr.kind_of_type)

  val hyp_to_atomic : Evd.evar_map -> Rocq_utils.hyp -> atomic_pair

  exception
    Mebi_proof_Hypothesis_Hyp of (Evd.evar_map * Rocq_utils.hyp * atomic_pair)

  exception Mebi_proof_Hypothesis_HTy of (Evd.evar_map * atomic_pair)

  module type HYP_TYPE = sig
    type t

    val hty_is_a : Evd.evar_map -> atomic_pair -> bool
    val of_hyp : Evd.evar_map -> Rocq_utils.hyp -> t
    val opt_of_hyp : Evd.evar_map -> Rocq_utils.hyp -> t option
    val hyp_is_a : Evd.evar_map -> Rocq_utils.hyp -> bool
  end

  module Cofix : HYP_TYPE
  module Invertible : HYP_TYPE
  module TransOpt : HYP_TYPE
end

module Hyp : HYP_S

val hyp_is_something : Evd.evar_map -> Rocq_utils.hyp -> bool
val hyps_is_essentially_empty : Proofview.Goal.t -> bool
val hyps_is_empty : Proofview.Goal.t -> bool

exception Mebi_proof_NewProof of unit
exception Mebi_proof_NewWeakSim of unit

val handle_new_proof : Proofview.Goal.t -> unit Proofview.tactic
val handle_new_weak_sim : Proofview.Goal.t -> unit Proofview.tactic
val handle_new_cofix : Proofview.Goal.t -> unit Proofview.tactic

val handle_new_transition
  :  Proofview.Goal.t
  -> Model_transition_opt.t
  -> unit Proofview.tactic

val handle_goal_transition
  :  Proofview.Goal.t
  -> Model_transition_opt.t
  -> unit Proofview.tactic

val handle_apply_constructors
  :  Proofview.Goal.t
  -> PState.applicable_constructors
  -> unit Proofview.tactic

val handle_proof_state : Proofview.Goal.t -> unit Proofview.tactic
val step : unit -> unit Proofview.tactic
val solve : int -> Declare.Proof.t -> Declare.Proof.t
