type bisim_exa =
  { s : Fsm.fsm
  ; t : Fsm.fsm
  ; are_bisimilar : bool
  }

type minim_exa = { the_fsm : Fsm.fsm }

type exa_kind =
  | Bisim of bisim_exa
  | Minim of minim_exa
  | Weak of bisim_exa

type example =
  { name : string
  ; kind : exa_kind
  }

val exa : string -> exa_kind -> example
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
val exa_self_act1 : example
val exa_weak1 : example
