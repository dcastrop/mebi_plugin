module Make (C : Rocq_context.SRocq_context) (E : Encoding.SEncoding) = struct
  module Enc : Encoding.SEncoding = E
  module M = Rocq_monad_utils.Make (C) (E)
  module Model = Model.Make (E)
end
