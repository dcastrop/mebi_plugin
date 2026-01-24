let make_wrapper () : (module Rocq_monad.TYPE) =
  (module Rocq_monad.Make (Rocq_monad.Context.Default) (Rocq_monad.Encoding.Int))
;;
