(** {i See {!Model.S.Bisimilarity}.} *)
module type S = Bisimilarity_intf.S

module Make
    (C : Components.S)
    (FSM :
       FSM.S
       with type state = C.State.t
        and type states = C.State.Set.t
        and type labels = C.Label.Set.t
        and type edgemap = C.EdgeMap.t'
        and type info = C.Info.t)
    (Minimization :
       Minimization.S
       with type state = C.State.t
        and type states = C.State.Set.t
        and type label = C.Label.t
        and type labels = C.Label.Set.t
        and type edgemap = C.EdgeMap.t'
        and type partition = C.Partition.t
        and type fsm = FSM.t) :
  S
  with type states = C.State.Set.t
   and type partition = C.Partition.t
   and type fsm = FSM.t
