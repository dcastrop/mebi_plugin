module type S = Decoder_intf.S

module Make
    (Enc : Encoding.S)
    (M : Rocq_monad_utils.S with type enc = Enc.t)
    (ConstructorBindings : Constructor_bindings.S with type 'a mm = 'a M.mm)
    (Model :
       Model.S
       with type base = Enc.t
        and type tree = Enc.Tree.t
        and type trees = Enc.Trees.t
        and type constructorbindings = ConstructorBindings.t) :
  S
  with type enc = Enc.t
   and type state = Model.State.t
   and type states = Model.State.Set.t
   and type partition = Model.Partition.t
   and type label = Model.Label.t
   and type labels = Model.Label.Set.t
   and type note = Model.Note.t
   and type annotation = Model.Annotation.t
   and type annotations = Model.Annotation.Set.t
   and type transition = Model.Transition.t
   and type transitions = Model.Transition.Set.t
   and type action = Model.Action.t
   and type actions = Model.Action.Set.t
   and type actionmap = Model.Action.Map.t'
   and type edgemap = Model.EdgeMap.t'
   and type rocqlts = Model.Info.Meta.RocqLTS.t
   and type info = Model.Info.t
   and type lts = Model.LTS.t
   and type fsm = Model.FSM.t
   and type result = Model.Bisimilarity.Result.t
   and type bisimilarity = Model.Bisimilarity.t
