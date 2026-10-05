module type S = sig
  include Theories_enc.S

  type fsm

  (** Raised by {!is_fsm_silent_label}: the FSM has no silent label. *)
  exception FSM_HasNoSilentLabel of fsm

  (** [is_fsm_silent_label x m] is whether [x] is one of [m]'s silent
      labels.

      @raise FSM_HasNoSilentLabel if [m] has none (raised here). *)
  val is_fsm_silent_label : EConstr.t -> fsm -> bool

  (** Raised by {!is_fsm_visible_label}: the FSM has no visible weak label. *)
  exception FSM_HasNoVisibleLabel of fsm

  (** [is_fsm_visible_label x m] is whether [x] is one of [m]'s weak labels
      that are not silent.

      @raise FSM_HasNoVisibleLabel if [m] has none (raised here). *)
  val is_fsm_visible_label : EConstr.t -> fsm -> bool

  exception FSM_HasNoWeakLabels of fsm

  val is_fsm_weak_labels : EConstr.t -> fsm -> bool

  (** Raised by {!is_fsm_constructor}: the FSM has no metadata, so no
      constructors. *)
  exception FSM_HasNoConstructors of fsm

  (** [is_fsm_constructor x m] is whether [x] is one of the LTSs [m] was
      extracted with (not a theory term).

      @raise FSM_HasNoConstructors if [m] has no metadata (raised here). *)
  val is_fsm_constructor : EConstr.t -> fsm -> bool
end

module Make
    (Enc : Encoding.S)
    (W :
       Wrapper.S
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
