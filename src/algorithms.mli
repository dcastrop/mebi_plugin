module Minimize : sig
  type result = Fsm.t * Model.Partition.t

  val run : ?weak:bool -> Fsm.t -> result
  val pstr : result -> string
end

module Bisimilar : sig
  type result =
    { the_fsm_1 : Fsm.t
    ; the_fsm_2 : Fsm.t
    ; merged_fsm : Fsm.t
    ; bisim_states : Model.Partition.t
    ; non_bisim_states : Model.Partition.t
    }

  val run : ?weak:bool -> Fsm.pair -> result
  val result_to_bool : result -> bool
  val pstr : result -> string
end
