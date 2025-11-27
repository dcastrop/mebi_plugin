module Hyp = Mebi_hypothesis

module PState : sig
  type t =
    | NewProof
    | NewWeakSim
    | NewCofix
    | GoalTransition of Model_transition_opt.t
    | ApplyConstructors of applicable_constructors
    | DetectState

  and applicable_constructors =
    { annotation : Model_note.annotation
    ; tactics : tactic_to_apply list option
    }

  and tactic_to_apply = unit -> unit Proofview.tactic

  val empty_tactics : tactic_to_apply list option -> bool
  val finished_applying_constructors : applicable_constructors -> bool
  val to_string : t -> string
end

val default_proof_state : PState.t
val the_proof_state : PState.t ref
val reset_the_proof_state : unit -> unit
val the_result : unit -> Algorithms.Bisimilar.result
val mfsm : unit -> Model.Fsm.t
val nfsm : unit -> Model.Fsm.t
val the_bisim_states : unit -> Model.Partition.t
val warn_model_action_hasnoannotations : Model_action.t -> unit

exception
  Mebi_proof_StatesNotBisimilar of
    (Model_state.t * Model_state.t * Model.Partition.t)

val are_states_bisimilar : Model_state.t -> Model_state.t -> bool
val get_naction : Model_state.t -> Model_label.t -> Model_action.t

val get_constructor_annotation
  :  Model_state.t
  -> Model_label.t
  -> Model_note.annotation

val get_annotation_constructor
  :  Model_state.t
  -> Model_label.t
  -> Mebi_constr.Tree.node list

exception
  Mebi_proof_TyDoesNotMatchTheories of (Evd.evar_map * Rocq_utils.kind_pair)

val do_nothing : unit -> unit Proofview.tactic
val do_simplify : Proofview.Goal.t -> unit Proofview.tactic
val do_rt1n_refl : Proofview.Goal.t -> unit Proofview.tactic
val do_rt1n_trans : Proofview.Goal.t -> unit Proofview.tactic
val do_rt1n_via : Proofview.Goal.t -> Model_label.t -> unit Proofview.tactic
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

val get_tactic : Mebi_constr.Tree.node -> unit -> unit Proofview.tactic

val get_tactics_to_apply
  :  Mebi_constr.Tree.node list
  -> PState.tactic_to_apply list

val do_build_constructor_tactics
  :  Proofview.Goal.t
  -> Model_note.annotation
  -> unit Proofview.tactic

val try_decode : Evd.evar_map -> Evd.econstr -> Mebi_setup.Enc.t option
val typ_is_exists : Evd.evar_map -> Rocq_utils.kind_pair -> bool
val typ_is_weak_sim : Evd.evar_map -> Rocq_utils.kind_pair -> bool
val typ_is_weak_transition : Evd.evar_map -> Rocq_utils.kind_pair -> bool
val typ_is_silent_transition : Evd.evar_map -> Rocq_utils.kind_pair -> bool
val typ_is_silent1_transition : Evd.evar_map -> Rocq_utils.kind_pair -> bool
val typ_is_lts_transition : Evd.evar_map -> Rocq_utils.kind_pair -> bool

val try_find_state
  :  Evd.evar_map
  -> Evd.econstr
  -> Model.States.t
  -> Model_state.t option

exception
  Mebi_proof_CouldNotDecodeState of
    (Evd.evar_map * Evd.econstr * Model.States.t)

val find_state : Evd.evar_map -> Evd.econstr -> Model.States.t -> Model_state.t

exception
  Mebi_proof_CouldNotDecodeLabel of
    (Evd.evar_map * Evd.econstr * Model.Alphabet.t)

val try_find_label
  :  Evd.evar_map
  -> Evd.econstr
  -> Model.Alphabet.t
  -> Model_label.t option

val find_label
  :  Evd.evar_map
  -> Evd.econstr
  -> Model.Alphabet.t
  -> Model_label.t

exception
  Mebi_proof_CouldNotDecodeTransition of
    (Evd.evar_map * Evd.econstr * Model.Fsm.t)

val get_transition
  :  Evd.evar_map
  -> Evd.econstr
  -> Evd.econstr
  -> Evd.econstr
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

module Cofix_HTy : Hyp.HTY_S
module Cofix : Hyp.HYP_TYPE

module Invertible_HTy : sig
  type t =
    | Full
    | Layer

  val of_hty : Evd.evar_map -> Rocq_utils.kind_pair -> t
end

module Invertible : sig
  type t =
    { kind : k
    ; tactic : unit Proofview.tactic
    }

  and k =
    | Full
    | Layer

  val of_hty : Evd.evar_map -> Rocq_utils.kind_pair -> k
  val opt_of_hty : Evd.evar_map -> Rocq_utils.kind_pair -> k option
  val hty_is_a : Evd.evar_map -> Rocq_utils.kind_pair -> bool
  val of_hyp : Evd.evar_map -> Rocq_utils.hyp -> t
  val opt_of_hyp : Evd.evar_map -> Rocq_utils.hyp -> t option
  val hyp_is_a : Evd.evar_map -> Rocq_utils.hyp -> bool
end

module TransOpt : Hyp.HYP_TYPE

val hyp_is_something : Evd.evar_map -> Rocq_utils.hyp -> bool
val hyps_is_essentially_empty : Proofview.Goal.t -> bool
val hyps_is_empty : Proofview.Goal.t -> bool

exception Mebi_proof_ConclIsNot_Exists of (Evd.evar_map * Rocq_utils.kind_pair)

val concl_get_eexists
  :  Evd.evar_map
  -> Rocq_utils.kind_pair
  -> Model_transition_opt.t * Model_state.t

val hyp_is_invertible : Evd.evar_map -> Rocq_utils.hyp -> bool

val hyps_get_invertibles
  :  Evd.evar_map
  -> Rocq_utils.hyp list
  -> Invertible.t list

val hyps_get_invertible : Invertible.t list -> Invertible.t
val do_inversion : Proofview.Goal.t -> unit Proofview.tactic
val get_ngoto : Model_state.t -> Model_transition_opt.t -> Model_state.t
val do_ex_intro : Model_state.t -> unit Proofview.tactic

exception Mebi_proof_ExIntro_NotBisimilar of (Model_state.t * Model_state.t)
exception Mebi_proof_ExIntro_NEqStateM of (Model_state.t * Model_state.t)

exception
  Mebi_proof_ExIntro_NEqLabel of
    (Model_transition_opt.t * Model_transition_opt.t)

exception
  Mebi_proof_ExIntro_Transitions of
    (Model_transition_opt.t * Model_transition_opt.t)

val do_eexists_transition : Proofview.Goal.t -> unit Proofview.tactic
val hyps_has_cofix : Evd.evar_map -> Evd.econstr -> Rocq_utils.hyp list -> bool
val hyps_has_invertible : Evd.evar_map -> Rocq_utils.hyp list -> bool

exception Mebi_proof_NewProof of unit
exception Mebi_proof_NewWeakSim of unit
exception Mebi_proof_NewCofix of unit
exception Mebi_proof_NewTransition of unit
exception Mebi_proof_GoalTransition of unit
exception Mebi_proof_ApplyConstructors of unit

val handle_new_proof : Proofview.Goal.t -> unit Proofview.tactic
val handle_new_weak_sim : Proofview.Goal.t -> unit Proofview.tactic
val handle_new_cofix : Proofview.Goal.t -> unit Proofview.tactic

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
