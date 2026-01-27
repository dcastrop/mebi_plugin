module type S = sig
  module Model : Model.S

  type t =
    { fsm : Model.FSM.t
    ; pi : Model.Partition.t
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
    { fsm : Model.FSM.t
    ; pi : Model.Partition.t
    }
end
