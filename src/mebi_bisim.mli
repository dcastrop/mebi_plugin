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

exception State_Of_Enc_NotFound of (int * Model.States.t)

exception
  Error_Multiple_States_Of_Enc_Found of (int * Model.States.t)

val find_state_of_enc : int -> Model.States.t -> Model.State.t

val find_state_of_enc_opt :
  int option -> Model.States.t -> Model.State.t option

exception Label_Of_Enc_NotFound of (int * Model.Alphabet.t)

exception
  Error_Multiple_Labels_Of_Enc_Found of
    (int * Model.Alphabet.t)

val find_label_of_enc :
  int -> Model.Alphabet.t -> Model.Alphabet.elt

val econstr_to_enc : Evd.econstr -> int

val econstr_to_enc_opt :
  Evd.evar_map -> Evd.econstr -> int option

val get_weak_transition :
  Fsm.t -> Evd.evar_map -> Evd.econstr array -> transition

val get_lts_transition :
  Fsm.t -> Evd.evar_map -> Evd.econstr array -> transition

val get_cofix :
  Fsm.t -> Fsm.t -> 'a -> 'b -> Evd.econstr array -> hyp_cofix

exception Invalid_KindOf_EConstr_Expected_Atomic of Evd.econstr
exception Invalid_KindOf_EConstr_Expected_Lambda of Evd.econstr
exception Invalid_KindOf_EConstr_Expected_App of Evd.econstr

exception
  ExpectedAppToContainWkTransAndSim of Evd.econstr array

exception UnhandledMebiTheoryKind of Mebi_theories.theory_kind
exception UnhandledConcl of Evd.econstr

val try_get_weak_transition :
  Proofview.Goal.t -> Fsm.t -> Evd.econstr -> transition option

val try_get_weak_sim :
  Proofview.Goal.t -> Evd.econstr -> unit option

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

val try_get_lts_transition :
  Proofview.Goal.t -> Fsm.t -> Evd.econstr -> transition option

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

exception ExpectedOnlyOne_H_ToBeInverted of Mebi_setup.hyp list

val handle_hyp :
  Algorithms.Bisimilar.result ->
  'a ->
  Evd.evar_map ->
  Mebi_setup.hyp ->
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
