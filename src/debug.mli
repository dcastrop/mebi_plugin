module Scope : sig
  val default_show_debug_scope : bool
  val show_debug_scope : bool ref
  val scope_counter : int ref
  val the_counter_funs : ((unit -> int) * (unit -> int)) ref
  val incr : unit -> int
  val decr : unit -> int
  val start : string -> unit -> unit
  val close : string -> unit -> unit
end
