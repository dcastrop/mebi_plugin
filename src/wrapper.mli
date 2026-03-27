module Make (Log : Logger.S) (Ctx : Rocq_context.S) (Enc : Encoding.S) :
  Wrapper_.S
  with type enc = Enc.t
   and type node = Enc.Tree.Node.t
   and type tree = Enc.Tree.t
   and type trees = Enc.Trees.t

val make
  :  ?log:(unit -> (module Logger.S))
  -> ?enc:((module Logger.S) -> (module Encoding.S))
  -> ?ctx:(module Rocq_context.S)
  -> unit
  -> (module Wrapper_.S)
