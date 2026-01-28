module type S = sig
  module Model : Model.S

  type t =
    { the_fsm_1 : fsm_pair
    ; the_fsm_2 : fsm_pair
    ; merged_fsm : Model.FSM.t
    ; bisim_states : Model.Partition.t
    ; non_bisim_states : Model.Partition.t
    }

  and fsm_pair =
    { original : Model.FSM.t
    ; saturated : Model.FSM.t
    }

  (* val fsm : Model.FSM.t -> Model.FSM.t *)
end

module Make (M : Model.S) : S = struct
  module Model : Model.S = M

  (* *)
  module Tree = Model.Tree
  module Trees = Model.Trees
  module State = Model.State
  module Label = Model.Label
  module Labels = Model.Labels
  module Note = Model.Note
  module Annotation = Model.Annotation
  module Annotations = Model.Annotations
  module Action = Model.Action
  module States = Model.States
  module ActionMap = Model.ActionMap
  module EdgeMap = Model.EdgeMap
  module FSM = Model.FSM

  type t =
    { the_fsm_1 : fsm_pair
    ; the_fsm_2 : fsm_pair
    ; merged_fsm : Model.FSM.t
    ; bisim_states : Model.Partition.t
    ; non_bisim_states : Model.Partition.t
    }

  and fsm_pair =
    { original : Model.FSM.t
    ; saturated : Model.FSM.t
    }
end
