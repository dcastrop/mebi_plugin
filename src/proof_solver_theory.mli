module type S = sig
  type 'a im
  type 'a mm
  type enc
  type fsm

  val is_any_theory : EConstr.t -> bool
  val is_theory : EConstr.t -> EConstr.t -> bool im
  val is_exists : EConstr.t -> bool im
  val is_weak_sim : EConstr.t -> bool im
  val is_weak : EConstr.t -> bool im
  val is_tau : EConstr.t -> bool im
  val is_silent : EConstr.t -> bool im
  val is_silent1 : EConstr.t -> bool im
  val is_LTS : EConstr.t -> bool im
  val is_None : EConstr.t -> bool im
  val is_Some : EConstr.t -> bool im

  exception EnsureFail

  val ensure : EConstr.t -> (EConstr.t -> bool im) -> unit im
  val get_theory_enc : (EConstr.t -> bool im) -> enc mm

  exception NoEncodingFoundFor_TheoriesNone

  val get_None_enc : unit -> enc mm

  exception NoEncodingFoundFor_TheoriesSome

  val get_Some_enc : unit -> enc mm

  exception NotEqTheory

  val get_theory_enc_if_eq : EConstr.t -> (EConstr.t -> bool im) -> enc mm
  val get_None_enc_if_eq : EConstr.t -> enc mm
  val get_Some_enc_if_eq : EConstr.t -> enc mm

  exception FSM_HasNoSilentLabel of fsm

  val is_fsm_silent_label : EConstr.t -> fsm -> bool

  exception FSM_HasNoVisibleLabel of fsm

  val is_fsm_visible_label : EConstr.t -> fsm -> bool

  exception FSM_HasNoWeakLabels of fsm

  val is_fsm_weak_labels : EConstr.t -> fsm -> bool

  exception FSM_HasNoConstructors of fsm

  val is_fsm_constructor : EConstr.t -> fsm -> bool
end

module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (W :
       Wrapper.S
       with type enc = Enc.t
        and type node = Enc.Tree.Node.t
        and type tree = Enc.Tree.t
        and type trees = Enc.Trees.t)
    (I : Rocq_monad_utils.S with type enc = Enc.t and type tree = Enc.Tree.t) :
  S
  with type 'a mm = 'a W.M.mm
   and type 'a im = 'a I.mm
   and type enc = Enc.t
   and type fsm = W.Model.FSM.t
