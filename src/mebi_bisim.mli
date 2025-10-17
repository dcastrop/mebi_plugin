val the_cached_result :
  Algorithms.Bisimilar.result ref option ref

val reset_the_cached_result : unit -> unit

exception MissingBisimResult of unit

val get_the_result : unit -> Algorithms.Bisimilar.result ref
val set_the_result : Algorithms.Bisimilar.result -> unit

type hyp_cofix = { m : Model.State.t; n : Model.State.t }

type transition = {
  from : Model.State.t;
  action : Model.Action.t;
  dest : Model.State.t option;
}

exception
  State_Of_Enc_NotFound of
    (Mebi_wrapper.Enc.t * Model.States.t)

exception
  Error_Multiple_States_Of_Enc_Found of
    (Mebi_wrapper.Enc.t * Model.States.t)

val find_state_of_enc :
  Mebi_wrapper.Enc.t -> Model.States.t -> Model.State.t

val find_state_of_enc_opt :
  Mebi_wrapper.Enc.t option ->
  Model.States.t ->
  Model.State.t option

exception
  Label_Of_Enc_NotFound of
    (Mebi_wrapper.Enc.t * Model.Alphabet.t)

exception
  Error_Multiple_Labels_Of_Enc_Found of
    (Mebi_wrapper.Enc.t * Model.Alphabet.t)

val find_label_of_enc :
  Mebi_wrapper.Enc.t -> Model.Alphabet.t -> Model.Alphabet.elt

val econstr_to_enc : EConstr.t -> Mebi_wrapper.Enc.t

val econstr_to_enc_opt :
  Evd.evar_map -> EConstr.t -> Mebi_wrapper.Enc.t option

val get_weak_transition :
  Fsm.t -> Evd.evar_map -> EConstr.t array -> transition

val get_lts_transition :
  Fsm.t -> Evd.evar_map -> EConstr.t array -> transition

val get_cofix :
  Fsm.t -> Fsm.t -> 'a -> 'b -> EConstr.t array -> hyp_cofix

exception Invalid_KindOf_EConstr_Expected_Atomic of EConstr.t

val get_atomic_type :
  Evd.evar_map -> EConstr.t -> EConstr.t * EConstr.t array

exception Invalid_KindOf_EConstr_Expected_Lambda of EConstr.t

val get_lambda :
  Evd.evar_map ->
  EConstr.t ->
  (Names.Name.t, Evd.erelevance) Context.pbinder_annot
  * EConstr.t
  * EConstr.t

exception Invalid_KindOf_EConstr_Expected_App of EConstr.t

val get_app :
  Evd.evar_map -> EConstr.t -> EConstr.t * EConstr.t array

exception UnhandledConcl of EConstr.t

val try_get_weak_transition :
  Proofview.Goal.t -> Fsm.t -> EConstr.t -> transition option

val try_get_weak_sim :
  Proofview.Goal.t -> EConstr.t -> unit option

val try_get_m_state_weak_sim :
  Proofview.Goal.t ->
  Fsm.t ->
  EConstr.t ->
  Model.State.t option

val try_get_exists :
  Proofview.Goal.t ->
  Fsm.t ->
  Fsm.t ->
  EConstr.t ->
  (transition * Model.State.t) option

val try_get_lts_transition :
  Proofview.Goal.t -> Fsm.t -> EConstr.t -> transition option

type concl_result =
  | New_Weak_Sim
  | Exists of (transition * Model.State.t)
  | Weak_Transition of transition
  | LTS_Transition of transition

val handle_concl :
  Proofview.Goal.t ->
  Algorithms.Bisimilar.result ->
  concl_result

type hyp_kind =
  | Cofix of hyp_cofix
  | H_Inversion of unit Proofview.tactic
  | H_Transition of transition
  | Pass

exception ExpectedOnlyOne_H_ToBeInverted of Mebi_theories.hyp list

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

val handle_hyps :
  Proofview.Goal.t -> Algorithms.Bisimilar.result -> hyp_result

val handle_new_cofix :
  Proofview.Goal.t -> unit Proofview.tactic

val get_all_cofix : Proofview.Goal.t -> Names.Id.Set.t

type proof_state = NewProof | NewCofix | NewTransition

val determine_proof_state :
  Proofview.Goal.t -> proof_state option

val iter_loop :
  Algorithms.Bisimilar.result -> unit Proofview.tactic

val loop_test : unit -> unit Proofview.tactic
