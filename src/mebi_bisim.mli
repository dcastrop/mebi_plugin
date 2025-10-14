val the_cached_result :
  Algorithms.Bisimilar.result ref option ref

val reset_the_cached_result : unit -> unit

val get_the_result :
  unit -> Algorithms.Bisimilar.result ref Mebi_wrapper.mm

val set_the_result : Algorithms.Bisimilar.result -> unit

type hyp_cofix = { m : Model.State.t; n : Model.State.t }

type transition = {
  from : Model.State.t;
  action : Model.Action.t;
  dest : Model.State.t option;
}

exception
  State_Of_Enc_NotFound of (Mebi_setup.Enc.t * Model.States.t)

exception
  Error_Multiple_States_Of_Enc_Found of
    (Mebi_setup.Enc.t * Model.States.t)

val find_state_of_enc :
  Mebi_setup.Enc.t -> Model.States.t -> Model.State.t

val find_state_of_enc_opt :
  Mebi_setup.Enc.t option ->
  Model.States.t ->
  Model.State.t option

exception
  Label_Of_Enc_NotFound of
    (Mebi_setup.Enc.t * Model.Alphabet.t)

exception
  Error_Multiple_Labels_Of_Enc_Found of
    (Mebi_setup.Enc.t * Model.Alphabet.t)

val find_label_of_enc :
  Mebi_setup.Enc.t -> Model.Alphabet.t -> Model.Alphabet.elt

val econstr_to_enc : EConstr.t -> Mebi_setup.Enc.t

val econstr_to_enc_opt :
  Evd.evar_map -> EConstr.t -> Mebi_setup.Enc.t option

exception Invalid_KindOf_EConstr_Expected_Atomic of EConstr.t

type econstr_enc = Mebi_setup.Enc.t * EConstr.t
type econstr_evar = Mebi_setup.Enc.t option * EConstr.t

val get_transition :
  Fsm.t -> 'a -> Evd.evar_map -> EConstr.t -> transition

exception
  Invalid_KindOfTypeEConstr_Expected_Atomic of EConstr.t

val get_test : unit -> unit Proofview.tactic Mebi_wrapper.mm
