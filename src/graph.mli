module type S = sig
  type weak
  type 'a mm
  type t
  type lts

  val build
    :  ?weak:weak option
    -> Constrexpr.constr_expr
    -> Libnames.qualid
    -> Names.GlobRef.t list
    -> t mm

  val extract : t -> lts mm
end

module Make
    (Log : Logger.S)
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
    (X : Graph_type.Args with type enc = Enc.t and type tree = Enc.Tree.t) :
  S with type weak = Weak.t and type 'a mm = 'a M.mm and type lts = Model.LTS.t
