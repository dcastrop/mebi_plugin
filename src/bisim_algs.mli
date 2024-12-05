module RCP : sig
  module Examples : sig
    val exa_1 : Fsm.fsm * Fsm.fsm
    val exa_2 : Fsm.fsm * Fsm.fsm
  end

  module KS90 : sig
    val s : int
  end

  module PT87 : sig
    val s : int
  end
end

val bisim_foo : int
