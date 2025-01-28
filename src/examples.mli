type example =
  { name : string
  ; s : Fsm.fsm
  ; t : Fsm.fsm
  ; are_bisimilar : bool
  }

val exa : string -> Fsm.fsm -> Fsm.fsm -> bool -> example
val exa_1 : example
val exa_2 : example
val exa_mc : example
val exa_self_rec_nondet : example
val exa_self_rec_nondet_inf : example
val exa_self_rec_det : example
val exa_self_rec_det_inf : example
val exa_rec_1 : example
val exa_rec_2 : example
val exa_par_1 : example
