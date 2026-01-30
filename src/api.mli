val default_encoding : unit -> (module Encoding.SEncoding)

val default_context :
  unit -> (module Rocq_context.SRocq_context)

val fresh_basic : unit -> unit

