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

exception
  Label_Of_Enc_NotFound of
    (Mebi_setup.Enc.t * Model.Alphabet.t)

exception
  Error_Multiple_Labels_Of_Enc_Found of
    (Mebi_setup.Enc.t * Model.Alphabet.t)

val find_label_of_enc :
  Mebi_setup.Enc.t -> Model.Alphabet.t -> Model.Alphabet.elt

val get_concl_transition :
  Fsm.t ->
  'a ->
  Evd.evar_map ->
  EConstr.t ->
  transition Mebi_wrapper.mm

exception Invalid_KindOf_EConstr_Expected_Atomic of EConstr.t

val get_econstr_transition :
  Fsm.t ->
  'a ->
  Evd.evar_map ->
  EConstr.t ->
  EConstr.t * EConstr.t * EConstr.t

exception
  Invalid_KindOfTypeEConstr_Expected_Atomic of EConstr.t

val get_test : unit -> unit Proofview.tactic Mebi_wrapper.mm
