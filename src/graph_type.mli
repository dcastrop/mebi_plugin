module type S = Graph_type_intf.S
module type Args = Graph_type_intf.Args

module Make
    (Enc : Encoding.S)
    (M : Rocq_monad_utils.S with type enc = Enc.t and type tree = Enc.Tree.t)
    (Weak : Weak.S with type enc = Enc.t)
    (Theory :
       Theories_enc.S
       with type enc = Enc.t
        and type 'a mm = 'a M.mm
        and type 'a im = 'a M.mm)
    (ConstructorBindings :
       Constructor_bindings.S with type 'a mm = 'a M.mm and type ind = M.Ind.t)
    (Model :
       Model.S
       with type base = Enc.t
        and type tree = Enc.Tree.t
        and type trees = Enc.Trees.t
        and type constructorbindings = ConstructorBindings.t)
    (X : Args with type enc = Enc.t and type tree = Enc.Tree.t) :
  S
  with type enc = Enc.t
   and type tree = Enc.Tree.t
   and type ind = M.Ind.t
   and type action = Model.Action.t
   and type weak = Weak.t
   and module B = M.B
   and module F = M.F
   and type 'a mm = 'a M.mm
