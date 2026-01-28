module type S = sig
  module Model : Model.S

  module type SActionPair = sig
    type t = Model.Action.t * Model.States.t

    val to_string : t -> string
    val compare : t -> t -> int
  end

  module ActionPair : SActionPair

  val fsm : ?only_if_weak:bool option -> Model.FSM.t -> Model.FSM.t
end

module Make : (_ : Model.S) -> S
