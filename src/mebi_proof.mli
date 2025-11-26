module Hyp = Mebi_hypothesis

module PState : sig
  type t =
    | NewProof
    | NewWeakSim
    | NewCofix
    | NewTransition of Model_transition_opt.t
    | GoalTransition of Model_transition_opt.t
    | ApplyConstructors of applicable_constructors
    | DetectState

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
val the_result : unit -> Algorithms.Bisimilar.result
val mfsm : unit -> Model.Fsm.t
val nfsm : unit -> Model.Fsm.t
val the_bisim_states : unit -> Model.Partition.t

exception
  Mebi_proof_StatesNotBisimilar of
    (Model_state.t * Model_state.t * Model.Partition.t)

val get_constructor_annotations
  :  Proofview.Goal.t
  -> Model_state.t
  -> Model_state.t
  -> Model_label.t
  -> Model_state.t
  -> PState.applicable_constructors

exception
  Mebi_proof_TyDoesNotMatchTheories of (Evd.evar_map * Rocq_utils.kind_pair)

val do_nothing : unit -> unit Proofview.tactic
val do_simplify : Proofview.Goal.t -> unit Proofview.tactic
val do_rt1n_refl : Proofview.Goal.t -> unit Proofview.tactic
val do_solve_cofix : Proofview.Goal.t -> unit Proofview.tactic

val do_weak_silent_transition
  :  Proofview.Goal.t
  -> Model_state.t
  -> Model_state.t
  -> unit Proofview.tactic

val do_weak_visible_transition
  :  Proofview.Goal.t
  -> Model_state.t
  -> Model_state.t
  -> Model_label.t
  -> Model_state.t
  -> unit Proofview.tactic

val try_decode : Evd.evar_map -> EConstr.t -> Mebi_setup.Enc.t option
val typ_is_weak_sim : Evd.evar_map -> Rocq_utils.kind_pair -> bool
val typ_is_weak_transition : Evd.evar_map -> Rocq_utils.kind_pair -> bool
val typ_is_silent_transition : Evd.evar_map -> Rocq_utils.kind_pair -> bool
val typ_is_silent1_transition : Evd.evar_map -> Rocq_utils.kind_pair -> bool
val typ_is_lts_transition : Evd.evar_map -> Rocq_utils.kind_pair -> bool

val try_find_state
  :  Evd.evar_map
  -> EConstr.t
  -> Model.States.t
  -> Model_state.t option

exception
  Mebi_proof_CouldNotDecodeState of (Evd.evar_map * EConstr.t * Model.States.t)

val find_state : Evd.evar_map -> EConstr.t -> Model.States.t -> Model_state.t

exception
  Mebi_proof_CouldNotDecodeLabel of
    (Evd.evar_map * EConstr.t * Model.Alphabet.t)

val try_find_label
  :  Evd.evar_map
  -> EConstr.t
  -> Model.Alphabet.t
  -> Model_label.t option

val find_label : Evd.evar_map -> EConstr.t -> Model.Alphabet.t -> Model_label.t

exception
  Mebi_proof_CouldNotDecodeTransition of
    (Evd.evar_map * EConstr.t * Model.Fsm.t)

val get_transition
  :  Evd.evar_map
  -> EConstr.t
  -> EConstr.t
  -> EConstr.t
  -> Model.Fsm.t
  -> Model_transition_opt.t

val get_lts_transition
  :  Evd.evar_map
  -> Model.Fsm.t
  -> Rocq_utils.kind_pair
  -> Model_transition_opt.t

val get_weak_transition
  :  Evd.evar_map
  -> Model.Fsm.t
  -> Rocq_utils.kind_pair
  -> Model_transition_opt.t

val get_silent_transition
  :  Evd.evar_map
  -> Model.Fsm.t
  -> Rocq_utils.kind_pair
  -> Model_transition_opt.t

val get_silent1_transition
  :  Evd.evar_map
  -> Model.Fsm.t
  -> Rocq_utils.kind_pair
  -> Model_transition_opt.t

exception Mebi_proof_CouldNotGetStateM of (Evd.evar_map * Rocq_utils.kind_pair)

val weak_sim_get_m_state : Evd.evar_map -> Rocq_utils.kind_pair -> Model_state.t

module Cofix : Hyp.HYP_TYPE
module Invertible : Hyp.HYP_TYPE
module TransOpt : Hyp.HYP_TYPE

val hyp_is_something : Evd.evar_map -> Rocq_utils.hyp -> bool
val hyps_is_essentially_empty : Proofview.Goal.t -> bool
val hyps_is_empty : Proofview.Goal.t -> bool

exception Mebi_proof_ConclIsNot_Exists of (Evd.evar_map * Rocq_utils.kind_pair)

val concl_get_eexists
  :  Evd.evar_map
  -> Rocq_utils.kind_pair
  -> Model_transition_opt.t * Model_state.t

val hyps_has_cofix : Evd.evar_map -> EConstr.t -> Rocq_utils.hyp list -> bool

exception Mebi_proof_NewProof of unit
exception Mebi_proof_NewWeakSim of unit
exception Mebi_proof_NewTransition of unit
exception Mebi_proof_GoalTransition of unit
exception Mebi_proof_ApplyConstructors of unit

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
val detect_proof_state : Proofview.Goal.t -> unit Proofview.tactic
val step : unit -> unit Proofview.tactic
val solve : int -> Declare.Proof.t -> Declare.Proof.t
