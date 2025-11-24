type hyp_cofix = { m : Model_state.t; n : Model_state.t }

type proof_state =
  | NewProof
  | NewWeakSim
  | NewCofix
  | NewTransition of Model_transition_opt.t
  | GoalTransition of Model_transition_opt.t
  | Constructors of
      (Model_note.annotations
      * (unit -> unit Proofview.tactic) list option)

val proof_state_to_string : proof_state -> string
val the_proof_state : proof_state ref
val reset_the_proof_state : unit -> unit

val the_cached_result :
  Algorithms.Bisimilar.result ref option ref

val reset_the_cached_result : unit -> unit

exception MissingBisimResult of unit

val get_the_result : unit -> Algorithms.Bisimilar.result ref
val get_n : unit -> Model.Fsm.t
val get_bisim_states : unit -> Model.Partition.t
val set_the_result : Algorithms.Bisimilar.result -> unit

exception
  Enc_Of_EConstr_NotFound of (Evd.econstr * Model.States.t)

exception
  State_Of_Enc_NotFound of (Mebi_setup.Enc.t * Model.States.t)

exception
  Error_Multiple_States_Of_Enc_Found of
    (Mebi_setup.Enc.t * Model.States.t)

val find_state_of_enc :
  Mebi_setup.Enc.t -> Model.States.t -> Model_state.t

val find_state_of_enc_opt :
  Mebi_setup.Enc.t -> Model.States.t -> Model_state.t option

exception
  Label_Of_Enc_NotFound of
    (Mebi_setup.Enc.t * Model.Alphabet.t)

exception
  Error_Multiple_Labels_Of_Enc_Found of
    (Mebi_setup.Enc.t * Model.Alphabet.t)

val find_label_of_enc :
  Mebi_setup.Enc.t -> Model.Alphabet.t -> Model_label.t

val econstr_to_enc : Evd.econstr -> Mebi_setup.Enc.t

val run_econstr_to_enc_opt :
  Evd.econstr -> Mebi_setup.Enc.t option

val enc_to_econstr : Mebi_setup.Enc.t -> Evd.econstr

val econstr_to_enc_opt :
  Evd.evar_map -> Evd.econstr -> Mebi_setup.Enc.t option

val get_concl_transition :
  ?abort_on_failed_dest_enc:bool ->
  Model.Fsm.t ->
  Evd.evar_map ->
  Evd.econstr * Evd.econstr * Evd.econstr ->
  Model_transition_opt.t

val get_weak_transition :
  Model.Fsm.t ->
  Evd.evar_map ->
  Evd.econstr array ->
  Model_transition_opt.t

val get_lts_transition :
  Model.Fsm.t ->
  Evd.evar_map ->
  Evd.econstr array ->
  Model_transition_opt.t

val get_hyp_transition :
  Model.Fsm.t ->
  Environ.env ->
  Evd.evar_map ->
  Evd.econstr array ->
  Model_transition_opt.t option

val get_cofix :
  Model.Fsm.t ->
  Model.Fsm.t ->
  'a ->
  'b ->
  Evd.econstr array ->
  hyp_cofix

exception Invalid_KindOf_EConstr_Expected_Atomic of Evd.econstr

val get_atomic_type :
  Evd.evar_map ->
  Evd.econstr ->
  Evd.econstr * Evd.econstr array

exception Invalid_KindOf_EConstr_Expected_Lambda of Evd.econstr

val get_lambda :
  Evd.evar_map ->
  Evd.econstr ->
  (Names.Name.t, Evd.erelevance) Context.pbinder_annot
  * Evd.econstr
  * Evd.econstr

exception Invalid_KindOf_EConstr_Expected_App of Evd.econstr

val get_app :
  Evd.evar_map ->
  Evd.econstr ->
  Evd.econstr * Evd.econstr array

exception UnhandledConcl of Evd.econstr

type concl_transition =
  | Weak_Transition
  | Silent_Transition
  | Silent1_Transition
  | LTS_Transition

val try_get_weak_transition :
  Evd.evar_map ->
  Model.Fsm.t ->
  Evd.econstr * Evd.econstr array ->
  (concl_transition * Model_transition_opt.t) option

val try_get_silent_transition :
  Evd.evar_map ->
  Model.Fsm.t ->
  Evd.econstr * Evd.econstr array ->
  (concl_transition * Model_transition_opt.t) option

val try_get_lts_transition :
  Evd.evar_map ->
  Model.Fsm.t ->
  Evd.econstr * Evd.econstr array ->
  (concl_transition * Model_transition_opt.t) option

val try_get_concl_transition :
  Proofview.Goal.t ->
  Model.Fsm.t ->
  Evd.econstr ->
  (concl_transition * Model_transition_opt.t) option

val try_get_weak_sim :
  Proofview.Goal.t -> Evd.econstr -> Evd.econstr option

val try_get_m_state_weak_sim :
  Proofview.Goal.t ->
  Model.Fsm.t ->
  Evd.econstr ->
  Model_state.t option

val try_get_exists :
  Proofview.Goal.t ->
  Model.Fsm.t ->
  Model.Fsm.t ->
  Evd.econstr ->
  (Model_transition_opt.t * Model_state.t) option

type concl_result =
  | New_Weak_Sim of Evd.econstr
  | Exists of (Model_transition_opt.t * Model_state.t)
  | Transition of (concl_transition * Model_transition_opt.t)

val concl_result_string : concl_result -> string

val handle_concl :
  Proofview.Goal.t ->
  Algorithms.Bisimilar.result ->
  concl_result

type hyp_transition = Full | Layer

type hyp_kind =
  | Cofix of hyp_cofix
  | H_Inversion of (hyp_transition * unit Proofview.tactic)
  | H_Transition of Model_transition_opt.t
  | Pass

exception ExpectedOnlyOne_H_ToBeInverted of Rocq_utils.hyp list

val handle_hyp :
  Algorithms.Bisimilar.result ->
  Environ.env ->
  Evd.evar_map ->
  Rocq_utils.hyp ->
  hyp_kind

type hyp_result =
  | Do_Inversion of unit Proofview.tactic
  | H_Transition of Model_transition_opt.t
  | Cofixes of hyp_cofix list
  | Empty

val hyp_result_string : hyp_result -> string
val warning_multiple_h_transitions_to_invert : 'a -> 'b -> unit

val handle_the_hyps :
  Algorithms.Bisimilar.result ->
  Environ.env ->
  Evd.evar_map ->
  Rocq_utils.hyp list ->
  (unit Proofview.tactic * bool) option
  * hyp_cofix list
  * Model_transition_opt.t list

val handle_hyps :
  Proofview.Goal.t -> Algorithms.Bisimilar.result -> hyp_result

val get_all_cofix_names : Proofview.Goal.t -> Names.Id.Set.t

val find_cofix_opt :
  Proofview.Goal.t ->
  Evd.econstr ->
  (Names.variable * Evd.econstr) option

val get_all_non_cofix : Proofview.Goal.t -> Names.Id.Set.t
val clear_old_hyps : Proofview.Goal.t -> unit Proofview.tactic
val do_new_cofix : Proofview.Goal.t -> unit Proofview.tactic

exception CannotUnpackTransitionsOfMN of unit

val get_bisim_states_of :
  Model_state.t -> Model_state.t option -> Model.States.t

val get_n_candidate_actions :
  'a ->
  Model.Fsm.t ->
  Model_action.t ->
  Model_state.t ->
  Model.States.t ->
  Model.States.t Model.Actions.t

val get_n_candidate_action_list :
  'a ->
  Model.Fsm.t ->
  Model_action.t ->
  Model_state.t ->
  Model.States.t ->
  (Model_action.t * Model_state.t) list

val warning_multiple_n_candidates :
  (Model_action.t * Model_state.t) list -> unit

val get_n_candidate_action :
  'a ->
  Model.Fsm.t ->
  Model_action.t ->
  Model_state.t ->
  Model.States.t ->
  Model_action.t * Model_state.t

val get_n_candidate :
  Model_state.t ->
  Model.Fsm.t ->
  Model_action.t ->
  Model_state.t ->
  Model.States.t ->
  Model_state.t

val handle_eexists :
  Proofview.Goal.t ->
  Model_transition_opt.t ->
  Model_transition_opt.t ->
  Model_state.t ->
  unit Proofview.tactic

val handle_weak_silent_transition :
  Proofview.Goal.t ->
  Model_state.t ->
  Model_state.t ->
  unit Proofview.tactic

val warning_multiple_n_dests : Model.States.t -> unit
val debug_constrs : Proofview.Goal.t -> unit

val get_from_state_of_relation :
  Proofview.Goal.t ->
  Model.States.t ->
  Evd.econstr ->
  Model_state.t

val build_tactics_from_constr_tree :
  'a ->
  (Mebi_setup.Enc.t * int) Mebi_constr.Tree.tree ->
  (unit -> unit Proofview.tactic) list

val build_constructors :
  Proofview.Goal.t ->
  Model_note.annotations ->
  unit Proofview.tactic

val handle_weak_visible_transition :
  Proofview.Goal.t ->
  Model_state.t * Model_action.t * Model_state.t ->
  Model_state.t * Model_action.t * Model_state.t ->
  unit Proofview.tactic

val handle_weak_transition :
  Proofview.Goal.t ->
  Model_transition_opt.t ->
  Model_transition_opt.t ->
  unit Proofview.tactic

exception CouldNotHandle_NewTransition of unit

val handle_new_transition_exists :
  Proofview.Goal.t ->
  Model_transition_opt.t ->
  Model_transition_opt.t ->
  Model_state.t ->
  unit Proofview.tactic

val handle_new_transition :
  Proofview.Goal.t ->
  Model_transition_opt.t ->
  unit Proofview.tactic

exception CouldNotHandle_GoalTransition of unit

val handle_goal_transition :
  Proofview.Goal.t ->
  Model_transition_opt.t ->
  unit Proofview.tactic

exception CouldNotHandle_NewCofix of unit

val handle_new_cofix :
  Proofview.Goal.t -> unit Proofview.tactic

exception CouldNotHandle_NewWeakSim of unit

val handle_new_weak_sim :
  Proofview.Goal.t -> unit Proofview.tactic

val do_simplify : Proofview.Goal.t -> unit Proofview.tactic
val do_rt1n_refl : Proofview.Goal.t -> unit Proofview.tactic

val handle_constuctors :
  Proofview.Goal.t ->
  Model_note.annotations
  * (unit -> unit Proofview.tactic) list option ->
  unit Proofview.tactic

exception CouldNotHandle_NewProof of unit

val handle_new_proof :
  Proofview.Goal.t -> unit Proofview.tactic

val handle_proof_state : unit -> unit Proofview.tactic
val loop_iter : unit -> unit Proofview.tactic
val solve : int -> Declare.Proof.t -> Declare.Proof.t
