type example =
  { name : string
  ; s : Fsm.fsm
  ; t : Fsm.fsm
  }

val exa : string -> Fsm.fsm -> Fsm.fsm -> example
val exa_1 : example
val exa_2 : example
