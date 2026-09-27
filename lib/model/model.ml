module type S = sig
  type base
  type tree
  type trees
  type constructorbindings

  include
    Components.S
    with type base := base
     and type tree := tree
     and type trees := trees
     and type constructorbindings := constructorbindings

  module LTS :
    LTS.S
    with type state = State.t
     and type states = State.Set.t
     and type labels = Label.Set.t
     and type transitions = Transition.Set.t
     and type info = Info.t

  module FSM :
    FSM.S
    with type state = State.t
     and type states = State.Set.t
     and type labels = Label.Set.t
     and type edgemap = EdgeMap.t'
     and type info = Info.t
     and type lts = LTS.t

  module Saturation :
    Saturation.S
    with type state = State.t
     and type states = State.Set.t
     and type labels = Label.Set.t
     and type edgemap = EdgeMap.t'

  module Minimization :
    Minimization.S
    with type state = State.t
     and type states = State.Set.t
     and type label = Label.t
     and type labels = Label.Set.t
     and type edgemap = EdgeMap.t'
     and type partition = Partition.t
     and type fsm = FSM.t

  module Bisimilarity :
    Bisimilarity.S
    with type states = State.Set.t
     and type partition = Partition.t
     and type fsm = FSM.t
end

module Make (Base : Base_term.S) (ConstructorBindings : Json.S) = struct
  module C = Components.Make (Base) (ConstructorBindings)
  include C

  (* TODO: the idea of [Traces] needs to be revisited. It does provide optimizations to examples with a lot of silent actions, where the saturated FSM is considerably larger, but i believe that there are areas where this can still be improved. *)
  module LTS = LTS.Make (C)
  module Saturation = Saturation.Make (Base) (C)
  module FSM = FSM.Make (C) (LTS) (Saturation)
  module Minimization = Minimization.Make (C) (FSM)
  module Bisimilarity = Bisimilarity.Make (C) (FSM) (Minimization)
end
