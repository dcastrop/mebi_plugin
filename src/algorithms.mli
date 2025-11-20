module Minimize : sig
  type result = Model.Fsm.t * Model.Partition.t

  val run : ?weak:bool -> Model.Fsm.t -> result
  val to_string : result -> string
end

module Bisimilar : sig
  type result =
    { the_fsm_1 : Model.Fsm.t
    ; the_fsm_2 : Model.Fsm.t
    ; merged_fsm : Model.Fsm.t
    ; bisim_states : Model.Partition.t
    ; non_bisim_states : Model.Partition.t
    }

  val run : ?weak:bool -> Model.Fsm.pair -> result
  val result_to_bool : result -> bool
  val to_string : result -> string
end
