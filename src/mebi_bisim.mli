exception Invalid_KindOf_EConstr_Expected_Atomic of EConstr.t

val get_atomic_type : Evd.evar_map -> EConstr.t -> EConstr.t * EConstr.t array

exception Invalid_KindOf_EConstr_Expected_Lambda of EConstr.t

val get_lambda
  :  Evd.evar_map
  -> EConstr.t
  -> (Names.Name.t, Evd.erelevance) Context.pbinder_annot
     * EConstr.t
     * EConstr.t

exception Invalid_KindOf_EConstr_Expected_App of EConstr.t

val get_app : Evd.evar_map -> EConstr.t -> EConstr.t * EConstr.t array

type hyp_cofix =
  { m : Model.State.t
  ; n : Model.State.t
  }

type transition =
  { from : Model.State.t
  ; action : Model.Action.t
  ; dest : Model.State.t option
  }

type proof_state =
  | ST_NewProof
  | ST_WeakSim
  | ST_Begin_Inversion
  | ST_Inversion of (Mebi_theories.hyp option * Mebi_theories.hyp)
  | ST_Hmt of transition
  | GoalTransition of transition
  | Constructors of
      (Model.Action.annotation * (unit -> unit Proofview.tactic) list option)

val the_proof_state : proof_state ref
val reset_the_proof_state : unit -> unit
val proof_state_str : unit -> string
val is_concl_weak_sim : Evd.evar_map -> Evd.econstr -> bool
val is_concl_weak_transition : Evd.evar_map -> Evd.econstr -> bool
val the_cached_result : Algorithms.Bisimilar.result ref option ref
val reset_the_cached_result : unit -> unit

exception MissingBisimResult of unit

val get_the_result : unit -> Algorithms.Bisimilar.result ref
val get_m : unit -> Fsm.t
val get_n : unit -> Fsm.t
val get_bisim_states : unit -> Model.Partition.t
val get_non_bisim_states : unit -> Model.Partition.t
val set_the_result : Algorithms.Bisimilar.result -> unit
val pstr_transition : transition -> string

exception Enc_Of_EConstr_NotFound of (EConstr.t * Model.States.t)
exception State_Of_Enc_NotFound of (Mebi_wrapper.Enc.t * Model.States.t)

exception
  Error_Multiple_States_Of_Enc_Found of (Mebi_wrapper.Enc.t * Model.States.t)

val find_state_of_enc : Mebi_wrapper.Enc.t -> Model.States.t -> Model.State.t

val find_state_of_enc_opt
  :  Mebi_wrapper.Enc.t
  -> Model.States.t
  -> Model.State.t option

exception Label_Of_Enc_NotFound of (Mebi_wrapper.Enc.t * Model.Alphabet.t)

exception
  Error_Multiple_Labels_Of_Enc_Found of (Mebi_wrapper.Enc.t * Model.Alphabet.t)

val find_label_of_enc
  :  Mebi_wrapper.Enc.t
  -> Model.Alphabet.t
  -> Model.Alphabet.elt

val econstr_to_enc : EConstr.t -> Mebi_wrapper.Enc.t
val run_econstr_to_enc_opt : EConstr.t -> Mebi_wrapper.Enc.t option
val enc_to_econstr : Mebi_wrapper.Enc.t -> EConstr.t
val econstr_to_enc_opt : Evd.evar_map -> EConstr.t -> Mebi_wrapper.Enc.t option

val get_concl_transition
  :  ?abort_on_failed_dest_enc:bool
  -> Fsm.t
  -> Evd.evar_map
  -> EConstr.t * EConstr.t * EConstr.t
  -> transition

val get_weak_transition : Fsm.t -> Evd.evar_map -> EConstr.t array -> transition
val get_lts_transition : Fsm.t -> Evd.evar_map -> EConstr.t array -> transition

val get_hyp_transition
  :  Fsm.t
  -> Evd.evar_map
  -> EConstr.t array
  -> transition option

val get_cofix : Fsm.t -> Fsm.t -> 'a -> 'b -> EConstr.t array -> hyp_cofix

exception UnhandledConcl of EConstr.t

type concl_transition =
  | Weak_Transition
  | Silent_Transition
  | Silent1_Transition
  | LTS_Transition

val try_get_weak_transition
  :  Evd.evar_map
  -> Fsm.t
  -> Evd.econstr * EConstr.t array
  -> (concl_transition * transition) option

val try_get_silent_transition
  :  Evd.evar_map
  -> Fsm.t
  -> Evd.econstr * EConstr.t array
  -> (concl_transition * transition) option

val try_get_lts_transition
  :  Evd.evar_map
  -> Fsm.t
  -> Evd.econstr * EConstr.t array
  -> (concl_transition * transition) option

val try_get_concl_transition
  :  Proofview.Goal.t
  -> Fsm.t
  -> EConstr.t
  -> (concl_transition * transition) option

val try_get_weak_sim : Proofview.Goal.t -> EConstr.t -> EConstr.t option

val try_get_m_state_weak_sim
  :  Proofview.Goal.t
  -> Fsm.t
  -> EConstr.t
  -> Model.State.t option

val try_get_exists
  :  Proofview.Goal.t
  -> Fsm.t
  -> Fsm.t
  -> EConstr.t
  -> (transition * Model.State.t) option

type concl_result =
  | New_Weak_Sim of EConstr.t
  | Exists of (transition * Model.State.t)
  | Transition of (concl_transition * transition)

val handle_concl
  :  Proofview.Goal.t
  -> Algorithms.Bisimilar.result
  -> concl_result

type hyp_transition =
  | Full
  | Layer

type hyp_kind =
  | Cofix of hyp_cofix
  | H_Inversion of (hyp_transition * Mebi_theories.hyp)
  | H_Transition of transition
  | Pass

exception ExpectedOnlyOne_H_ToBeInverted of Mebi_theories.hyp list

val handle_hyp
  :  Algorithms.Bisimilar.result
  -> 'a
  -> Evd.evar_map
  -> Mebi_theories.hyp
  -> hyp_kind

type hyp_result =
  | Do_Inversion of Mebi_theories.hyp
  | H_Transition of transition
  | Cofixes of hyp_cofix list
  | Empty

val warning_multiple_h_transitions_to_invert
  :  transition
  -> transition list
  -> unit

val handle_the_hyps
  :  Algorithms.Bisimilar.result
  -> 'a
  -> Evd.evar_map
  -> Mebi_theories.hyp list
  -> (Mebi_theories.hyp * bool) option * hyp_cofix list * transition list

val handle_hyps : Proofview.Goal.t -> Algorithms.Bisimilar.result -> hyp_result
val get_all_cofix_names : Proofview.Goal.t -> Names.Id.Set.t

val find_cofix_opt
  :  Proofview.Goal.t
  -> EConstr.t
  -> (Names.variable * EConstr.t) option

val get_all_non_cofix : Proofview.Goal.t -> Names.Id.Set.t
val clear_old_hyps : Proofview.Goal.t -> unit Proofview.tactic
val do_new_cofix : Proofview.Goal.t -> unit Proofview.tactic

exception CannotUnpackTransitionsOfMN of unit

val get_bisim_states_of
  :  Model.State.t
  -> Model.State.t option
  -> Model.States.t

val get_n_candidate_actions
  :  'a
  -> Fsm.t
  -> Model.Action.t
  -> Model.State.t
  -> Model.States.t
  -> Model.States.t Model.Actions.t

val get_n_candidate_action_list
  :  'a
  -> Fsm.t
  -> Model.Action.t
  -> Model.State.t
  -> Model.States.t
  -> (Model.Action.t * Model.State.t) list

val warning_multiple_n_candidates
  :  (Model.Action.t * Model.State.t) list
  -> unit

val get_n_candidate_action
  :  'a
  -> Fsm.t
  -> Model.Action.t
  -> Model.State.t
  -> Model.States.t
  -> Model.Action.t * Model.State.t

val get_n_candidate
  :  'a
  -> Fsm.t
  -> Model.Action.t
  -> Model.State.t
  -> Model.States.t
  -> Model.State.t

val handle_eexists
  :  Proofview.Goal.t
  -> transition
  -> transition
  -> Model.State.t
  -> unit Proofview.tactic

val handle_weak_silent_transition
  :  Proofview.Goal.t
  -> Model.State.t
  -> Model.State.t
  -> unit Proofview.tactic

val warning_multiple_n_dests : Model.States.t -> unit
val debug_constrs : Proofview.Goal.t -> unit

val get_from_state_of_relation
  :  Proofview.Goal.t
  -> Model.States.t
  -> EConstr.t
  -> Model.State.t

val build_tactics_from_constr_tree
  :  'a
  -> (Mebi_setup.Enc.t * int) Mebi_constr_tree.tree
  -> (unit -> unit Proofview.tactic) list

val build_constructors
  :  Proofview.Goal.t
  -> Model.Action.annotation
  -> unit Proofview.tactic

val handle_weak_visible_transition
  :  Proofview.Goal.t
  -> Model.State.t * Model.Action.t * Model.State.t
  -> Model.State.t * Model.Action.t * Model.State.t
  -> unit Proofview.tactic

val handle_weak_transition
  :  Proofview.Goal.t
  -> transition
  -> transition
  -> unit Proofview.tactic

exception CouldNotHandle_ST_Hmt of unit

val handle_new_transition
  :  Proofview.Goal.t
  -> transition
  -> unit Proofview.tactic

exception CouldNotHandle_GoalTransition of unit

val handle_goal_transition
  :  Proofview.Goal.t
  -> transition
  -> unit Proofview.tactic

exception CouldNotHandle_BeginInversion of unit
exception ErrorUpdatingProofState of proof_state

val handle_begin_inversion : Proofview.Goal.t -> unit Proofview.tactic
val handle_inversion : Proofview.Goal.t -> unit Proofview.tactic

exception CouldNotHandle_ST_WeakSim of unit

val handle_new_weak_sim : Proofview.Goal.t -> unit Proofview.tactic
val do_simplify : Proofview.Goal.t -> unit Proofview.tactic

val handle_constuctors
  :  Proofview.Goal.t
  -> Model.Action.annotation * (unit -> unit Proofview.tactic) list option
  -> unit Proofview.tactic

exception CouldNotHandle_ST_NewProof of unit

val handle_new_proof : Proofview.Goal.t -> unit Proofview.tactic
val handle_proof_state : unit -> unit Proofview.tactic
val loop_test : unit -> unit Proofview.tactic
