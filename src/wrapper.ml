module type S = sig
  module M : Rocq_monad.Utils.S

  module Model :
    Model.S
    with module Tree.Enc = M.Enc
     and type Tree.Enc.t = M.Enc.t
     and module Enc = M.Enc
     and type Enc.t = M.Enc.t
end

module Make (Ctx : Rocq_context.SRocq_context) (E : Encoding.SEncoding) :
  S
  with module M.M.BiEnc.Enc = E
   and type M.M.BiEnc.Enc.t = E.t
   and module Model.Tree.Enc = E
   and type Model.Tree.Enc.t = E.t
   and type Model.Tree.TreeNode.t = E.t * int = struct
  module Enc : Encoding.SEncoding = E

  module M :
    Rocq_monad.Utils.S with module M.BiEnc.Enc = E and type M.BiEnc.Enc.t = E.t =
    Rocq_monad.Utils.Make (Ctx) (E)

  module Model :
    Model.S
    with module Tree.Enc = M.Enc
     and type Tree.Enc.t = M.Enc.t
     and type Enc.t = M.BiEnc.Enc.t
     and type Enc.t = M.BiEnc.Enc.t
     and type Tree.TreeNode.t = E.t * int =
    Model.Make (E)
end
