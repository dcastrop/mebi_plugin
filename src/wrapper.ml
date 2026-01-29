module Make (C : Rocq_context.SRocq_context) (E : Encoding.SEncoding) = struct
  module M = Rocq_monad_utils.Make (C) (E)
  module Model = Model.Make (M.Enc)
end
