module Scope : sig
  val default_show_debug_scope : bool
  val show_debug_scope : bool ref

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
