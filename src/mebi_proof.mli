val nothing : unit -> unit Proofview.tactic

module PState : sig
  type t =
    | NewProof
    | NewWeakSim
    | NewCofix
    | NewTransition of Model.Transition_opt.t
    | GoalTransition of Model.Transition_opt.t
    | ApplyConstructors of applicable_constructors

  and applicable_constructors =
    { annotation : Model.Note.annotation
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

module Hyp : sig
  type atomic_pair = EConstr.t * EConstr.t array

  exception
    Mebi_proof_HypIsNot_Atomic of
      (Evd.evar_map * Rocq_utils.hyp * EConstr.kind_of_type)

  val hyp_to_atomic : Evd.evar_map -> Rocq_utils.hyp -> atomic_pair

  exception
    Mebi_proof_Hypothesis_Hyp of (Evd.evar_map * Rocq_utils.hyp * atomic_pair)

  exception Mebi_proof_Hypothesis_HTy of (Evd.evar_map * atomic_pair)

  module type HTY_TYPE = sig
    type t

    val of_hty : Evd.evar_map -> atomic_pair -> t
    val opt_of_hty : Evd.evar_map -> atomic_pair -> t option
    val hty_is_a : Evd.evar_map -> atomic_pair -> bool
  end

  module type HTY_S = sig
    type t

    val of_hty : Evd.evar_map -> atomic_pair -> t
  end

  module MakeHTy : (_ : HTY_S) -> HTY_TYPE

  module type HYP_TYPE = sig
    type t

    val hty_is_a : Evd.evar_map -> atomic_pair -> bool
    val of_hyp : Evd.evar_map -> Rocq_utils.hyp -> t
    val opt_of_hyp : Evd.evar_map -> Rocq_utils.hyp -> t option
    val hyp_is_a : Evd.evar_map -> Rocq_utils.hyp -> bool
  end

  module type HYP_S = sig
    module HTy : HTY_TYPE

    type t

    val of_hyp : Evd.evar_map -> Rocq_utils.hyp -> t
  end

  module MakeHyp : (_ : HYP_S) -> HYP_TYPE
  module Make : (_ : HTY_S) -> HYP_TYPE
  module Cofix : HYP_TYPE
  module Invertible : HYP_TYPE
  module TransOpt : HYP_TYPE
end

val hyp_is_something : Evd.evar_map -> Rocq_utils.hyp -> bool
val hyps_is_essentially_empty : Proofview.Goal.t -> bool
val hyps_is_empty : Proofview.Goal.t -> bool

exception Mebi_proof_NewProof of unit
exception Mebi_proof_NewWeakSim of unit

val handle_new_proof : Proofview.Goal.t -> unit Proofview.tactic
val handle_proof_state : Proofview.Goal.t -> unit Proofview.tactic
val step : unit -> unit Proofview.tactic
val solve : int -> Declare.Proof.t -> Declare.Proof.t
