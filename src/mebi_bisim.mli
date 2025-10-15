val the_cached_result : Algorithms.Bisimilar.result ref option ref
val reset_the_cached_result : unit -> unit
val get_the_result : unit -> Algorithms.Bisimilar.result ref Mebi_wrapper.mm
val set_the_result : Algorithms.Bisimilar.result -> unit

type proof_state =
  | CheckCofix
  | NewIter

type transition =
  { from : Model.State.t
  ; action : Model.Action.t
  ; dest : Model.State.t option
  }

exception State_Of_Enc_NotFound of (int * Model.States.t)
exception Error_Multiple_States_Of_Enc_Found of (int * Model.States.t)

val find_state_of_enc : int -> Model.States.t -> Model.State.t
val find_state_of_enc_opt : int option -> Model.States.t -> Model.State.t option

exception Label_Of_Enc_NotFound of (int * Model.Alphabet.t)
exception Error_Multiple_Labels_Of_Enc_Found of (int * Model.Alphabet.t)

val find_label_of_enc : int -> Model.Alphabet.t -> Model.Alphabet.elt
val econstr_to_enc : Evd.econstr -> int
val econstr_to_enc_opt : Evd.evar_map -> Evd.econstr -> int option

val get_weak_transition
  :  Fsm.t
  -> Evd.evar_map
  -> Evd.econstr array
  -> transition

val get_lts_transition
  :  Fsm.t
  -> Evd.evar_map
  -> Evd.econstr array
  -> transition

exception Invalid_KindOf_EConstr_Expected_Atomic of Evd.econstr
exception UnhandledMebiTheoryKind of Mebi_theories.theory_kind

type concl_kind =
  | Weak_Sim of Evd.econstr array
  | Weak_Transition of transition
  | LTS_Transition of transition
  | Pass

val handle_concl : Proofview.Goal.t -> Algorithms.Bisimilar.result -> concl_kind
val get_cofix : Proofview.Goal.t -> Names.Id.Set.t

type hyp_kind =
  | Cofix of Mebi_setup.hyp
  | H_To_Invert of Mebi_setup.hyp
  | H_Transition of transition
  | Pass

val handle_hyp
  :  Algorithms.Bisimilar.result
  -> Environ.env
  -> Evd.evar_map
  -> Mebi_setup.hyp
  -> hyp_kind

exception ExpectedOnlyOne_H_ToBeInverted of Mebi_setup.hyp list

val do_invert : hyp_kind list -> unit Proofview.tactic
val handle_hyps : Proofview.Goal.t -> Algorithms.Bisimilar.result -> unit
val get_test : unit -> unit Proofview.tactic Mebi_wrapper.mm
