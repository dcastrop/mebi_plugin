module type S = Proof_solver_statem_intf.S

module Make
    (Enc : Encoding.S)
    (W :
       Results.S
       with type enc = Enc.t
        and type node = Enc.Tree.Node.t
        and type tree = Enc.Tree.t
        and type trees = Enc.Trees.t) :
  S
  with type enc = Enc.t
   and type node = Enc.Tree.Node.t
   and type state = W.Model.State.t
   and type label = W.Model.Label.t
   and type annotation = W.Model.Annotation.t
   and type transition = W.Model.Transition.t
