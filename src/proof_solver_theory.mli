module type S = sig
  include Theories_enc.S

  type fsm

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
