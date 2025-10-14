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

exception Invalid_KindOf_EConstr_Expected_Atomic of Evd.econstr

type econstr_enc = int * Evd.econstr
type econstr_evar = int option * Evd.econstr

val get_weak_sim_transition :
  Fsm.t -> Evd.evar_map -> Evd.econstr array -> transition

exception UnhandledTysSize of int

val get_transition :
  Fsm.t ->
  Environ.env ->
  Evd.evar_map ->
  Evd.econstr ->
  transition

exception
  Invalid_KindOfTypeEConstr_Expected_Atomic of Evd.econstr

val get_test : unit -> unit Proofview.tactic Mebi_wrapper.mm
