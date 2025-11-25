module Hyp = Mebi_hypothesis

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

module Cofix : Hyp.HYP_TYPE
module Invertible : Hyp.HYP_TYPE
module TransOpt : Hyp.HYP_TYPE

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
