module Control : sig
  exception ExitDevelopmentTest of unit

  val get_fresh_counter :
    unit -> ((unit -> int) * (unit -> int)) * int ref

  val default_target : int

  val the_counter_funs :
    ((((unit -> int) * (unit -> int)) * int ref) * int) ref

  val set_new_target : int -> unit
  val incr : unit -> int
  val decr : unit -> int
  val getvalue : unit -> int
  val gettarget : unit -> int
  val tick : unit -> unit
end

module Scope : sig
  val default_show_debug_scope : bool
  val show_debug_scope : bool ref

  val the_counter_funs :
    (((unit -> int) * (unit -> int)) * int ref) ref

  val incr : unit -> int
  val decr : unit -> int
  val getvalue : unit -> int

  val msg :
    ?prefix:string ->
    ?infix:string ->
    ?suffix:string ->
    ?info:string option ->
    int ->
    string ->
    unit

  val return :
    ?prefix:string ->
    ?infix:string ->
    ?suffix:string ->
    ?info:string option ->
    unit ->
    unit

  val start :
    ?prefix:string ->
    ?infix:string ->
    ?suffix:string ->
    ?info:string option ->
    unit ->
    unit

  val close :
    ?prefix:string ->
    ?infix:string ->
    ?suffix:string ->
    ?info:string option ->
    unit ->
    unit
end
