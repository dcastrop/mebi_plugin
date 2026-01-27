module type S = sig
  module Model : Model.S

  val fsm : Model.FSM.t -> Model.FSM.t
end

module Make : (_ : Model.S) -> S
