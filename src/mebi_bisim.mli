type hyp_cofix = { m : Model.State.t; n : Model.State.t }

type transition = {
  from : Model.State.t;
  action : Model.Action.t;
  dest : Model.State.t option;
}

type proof_state =
  | NewProof
  | NewWeakSim
  | NewCofix
  | NewTransition of transition
  | GoalTransition of transition
  | Constructors of
      (Model.Action.annotation
      * (unit -> unit Proofview.tactic) list option)

val the_proof_state : proof_state ref
val reset_the_proof_state : unit -> unit

val the_cached_result :
  Algorithms.Bisimilar.result ref option ref

val reset_the_cached_result : unit -> unit

exception MissingBisimResult of unit

val get_the_result : unit -> Algorithms.Bisimilar.result ref
val get_m : unit -> Fsm.t
val get_n : unit -> Fsm.t
val get_bisim_states : unit -> Model.Partition.t
val get_non_bisim_states : unit -> Model.Partition.t
val set_the_result : Algorithms.Bisimilar.result -> unit
val pstr_transition : transition -> string

exception
  Enc_Of_EConstr_NotFound of (Evd.econstr * Model.States.t)

exception State_Of_Enc_NotFound of (int * Model.States.t)

exception
  Error_Multiple_States_Of_Enc_Found of (int * Model.States.t)

val find_state_of_enc : int -> Model.States.t -> Model.State.t

val find_state_of_enc_opt :
  int -> Model.States.t -> Model.State.t option

exception Label_Of_Enc_NotFound of (int * Model.Alphabet.t)

exception
  Error_Multiple_Labels_Of_Enc_Found of
    (int * Model.Alphabet.t)

val find_label_of_enc :
  int -> Model.Alphabet.t -> Model.Alphabet.elt

val econstr_to_enc : Evd.econstr -> int
val run_econstr_to_enc_opt : Evd.econstr -> int option
val enc_to_econstr : int -> Evd.econstr

val econstr_to_enc_opt :
  Evd.evar_map -> Evd.econstr -> int option

val get_concl_transition :
  ?abort_on_failed_dest_enc:bool ->
  Fsm.t ->
  Evd.evar_map ->
  Evd.econstr * Evd.econstr * Evd.econstr ->
  transition

val get_weak_transition :
  Fsm.t -> Evd.evar_map -> Evd.econstr array -> transition

val get_lts_transition :
  Fsm.t -> Evd.evar_map -> Evd.econstr array -> transition

val get_hyp_transition :
  Fsm.t ->
  Evd.evar_map ->
  Evd.econstr array ->
  transition option

val get_cofix :
  Fsm.t -> Fsm.t -> 'a -> 'b -> Evd.econstr array -> hyp_cofix

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
  Fsm.t ->
  Evd.econstr * Evd.econstr array ->
  (concl_transition * transition) option

val try_get_silent_transition :
  Evd.evar_map ->
  Fsm.t ->
  Evd.econstr * Evd.econstr array ->
  (concl_transition * transition) option

val try_get_lts_transition :
  Evd.evar_map ->
  Fsm.t ->
  Evd.econstr * Evd.econstr array ->
  (concl_transition * transition) option

val try_get_concl_transition :
  Proofview.Goal.t ->
  Fsm.t ->
  Evd.econstr ->
  (concl_transition * transition) option

val try_get_weak_sim :
  Proofview.Goal.t -> Evd.econstr -> Evd.econstr option

val try_get_m_state_weak_sim :
  Proofview.Goal.t ->
  Fsm.t ->
  Evd.econstr ->
  Model.State.t option

val try_get_exists :
  Proofview.Goal.t ->
  Fsm.t ->
  Fsm.t ->
  Evd.econstr ->
  (transition * Model.State.t) option

type concl_result =
  | New_Weak_Sim of Evd.econstr
  | Exists of (transition * Model.State.t)
  | Transition of (concl_transition * transition)

val handle_concl :
  Proofview.Goal.t ->
  Algorithms.Bisimilar.result ->
  concl_result

type hyp_transition = Full | Layer

type hyp_kind =
  | Cofix of hyp_cofix
  | H_Inversion of (hyp_transition * unit Proofview.tactic)
  | H_Transition of transition
  | Pass

exception
  ExpectedOnlyOne_H_ToBeInverted of Mebi_theories.hyp list

val handle_hyp :
  Algorithms.Bisimilar.result ->
  'a ->
  Evd.evar_map ->
  Mebi_theories.hyp ->
  hyp_kind

type hyp_result =
  | Do_Inversion of unit Proofview.tactic
  | H_Transition of transition
  | Cofixes of hyp_cofix list
  | Empty

val warning_multiple_h_transitions_to_invert :
  transition -> transition list -> unit

val handle_the_hyps :
  Algorithms.Bisimilar.result ->
  'a ->
  Evd.evar_map ->
  Mebi_theories.hyp list ->
  (unit Proofview.tactic * bool) option
  * hyp_cofix list
  * transition list

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
  Model.State.t -> Model.State.t option -> Model.States.t

val get_n_candidate_actions :
  'a ->
  Fsm.t ->
  Model.Action.t ->
  Model.State.t ->
  Model.States.t ->
  Model.States.t Model.Actions.t

val get_n_candidate_action_list :
  'a ->
  Fsm.t ->
  Model.Action.t ->
  Model.State.t ->
  Model.States.t ->
  (Model.Action.t * Model.State.t) list

val warning_multiple_n_candidates :
  (Model.Action.t * Model.State.t) list -> unit

val get_n_candidate_action :
  'a ->
  Fsm.t ->
  Model.Action.t ->
  Model.State.t ->
  Model.States.t ->
  Model.Action.t * Model.State.t

val get_n_candidate :
  'a ->
  Fsm.t ->
  Model.Action.t ->
  Model.State.t ->
  Model.States.t ->
  Model.State.t

val handle_eexists :
  Proofview.Goal.t ->
  transition ->
  transition ->
  Model.State.t ->
  unit Proofview.tactic

val handle_weak_silent_transition :
  Proofview.Goal.t ->
  Model.State.t ->
  Model.State.t ->
  unit Proofview.tactic

val warning_multiple_n_dests : Model.States.t -> unit
val debug_constrs : Proofview.Goal.t -> unit

val get_from_state_of_relation :
  Proofview.Goal.t ->
  Model.States.t ->
  Evd.econstr ->
  Model.State.t

val build_tactics_from_constr_tree :
  'a ->
  (int * int) Mebi_constr_tree.tree ->
  (unit -> unit Proofview.tactic) list

val build_constructors :
  Proofview.Goal.t ->
  Model.Action.annotation ->
  unit Proofview.tactic

val handle_weak_visible_transition :
  Proofview.Goal.t ->
  Model.State.t * Model.Action.t * Model.State.t ->
  Model.State.t * Model.Action.t * Model.State.t ->
  unit Proofview.tactic

val handle_weak_transition :
  Proofview.Goal.t ->
  transition ->
  transition ->
  unit Proofview.tactic

exception CouldNotHandle_NewTransition of unit

val handle_new_transition :
  Proofview.Goal.t -> transition -> unit Proofview.tactic

exception CouldNotHandle_GoalTransition of unit

val handle_goal_transition :
  Proofview.Goal.t -> transition -> unit Proofview.tactic

exception CouldNotHandle_NewCofix of unit

val handle_new_cofix :
  Proofview.Goal.t -> unit Proofview.tactic

exception CouldNotHandle_NewWeakSim of unit

val handle_new_weak_sim :
  Proofview.Goal.t -> unit Proofview.tactic

val do_simplify : Proofview.Goal.t -> unit Proofview.tactic

val handle_constuctors :
  Proofview.Goal.t ->
  Model.Action.annotation
  * (unit -> unit Proofview.tactic) list option ->
  unit Proofview.tactic

exception CouldNotHandle_BuildConstructors of unit
exception CouldNotHandle_NewProof of unit

val handle_new_proof :
  Proofview.Goal.t -> unit Proofview.tactic

val handle_proof_state : unit -> unit Proofview.tactic
val loop_test : unit -> unit Proofview.tactic

val iter_loop :
  Algorithms.Bisimilar.result -> unit Proofview.tactic

val _loop_test : unit -> unit Proofview.tactic
