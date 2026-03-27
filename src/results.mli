module Make (Log : Logger.S) (Ctx : Rocq_context.S) (Enc : Encoding.S) :
  Results_.S
  with module M.Ctx = Ctx
   and type enc = Enc.t
   and type node = Enc.Tree.Node.t
   and type tree = Enc.Tree.t
   and type trees = Enc.Trees.t
