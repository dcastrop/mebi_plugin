module Minimize : sig
  type t = Fsm.t
  type result = Fsm.t * Model.Partition.t
end

module Bisimilar : sig
  type t = Fsm.pair * Fsm.t * Model.Partition.t
  type result = Fsm.pair * Fsm.t * (Model.Partition.t * Model.Partition.t)
end

type t =
  | Minim of (bool * Fsm.t)
  | Bisim of (bool * Fsm.pair)

type result =
  | Minim of Minimize.result
  | Bisim of Bisimilar.result

val run : t -> result
val bisim_result_to_bool : Bisimilar.result -> bool
val pstr : result -> string
