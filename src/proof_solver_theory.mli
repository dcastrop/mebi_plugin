module type S = sig
  include Theories_enc.S

  type fsm

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
       Wrapper_.S
       with type enc = Enc.t
        and type node = Enc.Tree.Node.t
        and type tree = Enc.Tree.t
        and type trees = Enc.Trees.t)
    (I : Proof_solver_wrapper.S with type enc = Enc.t and type tree = Enc.Tree.t) :
  S
  with type 'a mm = 'a W.M.mm
   and type 'a im = 'a I.mm
   and type enc = Enc.t
   and type fsm = W.Model.FSM.t
