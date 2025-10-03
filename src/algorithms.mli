module Minimize : sig
  type result = Fsm.t * Model.Partition.t

  val run : ?weak:bool -> Fsm.t -> result
  val pstr : result -> string
end

module Bisimilar : sig
  type result =
    (Fsm.pair * Fsm.t) * (Model.Partition.t * Model.Partition.t)

  val run : ?weak:bool -> Fsm.pair -> result
  val result_to_bool : result -> bool
  val pstr : result -> string
end
