module RCP : sig
  module Examples : sig
    val exa_1 : 'a * 'b
    val exa_2 : 'a * 'b
  end

  module KS90 : sig
    val reachable_partition : 'a -> 'b -> 'c
    val split : 'a -> 'b -> 'c -> 'd -> 'e
    val run : 'a -> 'b -> bool * 'c
  end

  module PT87 : sig end
end

val bisim_foo : int
