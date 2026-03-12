module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (Bindings : sig
       module Instructions : sig
         type t =
           | Undefined
           | Done
           | Arg of
               { root : Constr.t
               ; index : int
               ; cont : t
               }
       end

       module ConstrMap : sig
         include Hashtbl.S with type key = Constr.t

         type v = Names.Name.t * Instructions.t
         type t' = v t
       end

       type t =
         | No_Bindings
         | Use_Bindings of
             { from : ConstrMap.t' option
             ; action : ConstrMap.t' option
             ; goto : ConstrMap.t' option
             }
     end)
    (ConstructorBindings : sig
       type t =
         { index : int
         ; name : string
         ; bindings : Bindings.t
         }

       val json : ?as_elt:bool -> t -> Yojson.t
     end) =
struct
  module State : State.S with type base = Base.t = State.Make (Log) (Base)
  module States : States.S with type elt = State.t = States.Make (Log) (State)
  module Label : Label.S with type base = Base.t = Label.Make (Log) (Base)
  module Labels : Labels.S with type elt = Label.t = Labels.Make (Log) (Label)

  (* TODO: working on how to give the rest of them signatures to make maintaining this easier *)
  module Note :
    Annotation_note.S
    with type state = State.t
     and type label = Label.t
     and type trees = Base.Trees.t =
    Annotation_note.Make (Log) (Base) (State) (Label)

  module Annotation :
    Annotation.S with type label = Label.t and type note = Note.t =
    Annotation.Make (Log) (Base) (Label) (Note)

  module Annotations : Annotations.S with type elt = Annotation.t =
    Annotations.Make (Log) (Note) (Annotation)

  module Transition :
    Transition.S
    with type state = State.t
     and type label = Label.t
     and type tree = Base.Tree.t
     and type annotation = Annotation.t =
    Transition.Make (Log) (Base) (State) (Label) (Annotation)

  module Transitions :
    Transitions.S with type elt = Transition.t and type labels = Labels.t =
    Transitions.Make (Log) (Labels) (Transition)

  module Action :
    Action.S
    with type label = Label.t
     and type annotation = Annotation.t
     and type trees = Base.Trees.t =
    Action.Make (Log) (Base) (Label) (Annotation)

  module Actions :
    Actions.S
    with type elt = Action.t
     and type label = Label.t
     and type labels = Labels.t =
    Actions.Make (Log) (Label) (Labels) (Action)

  module ActionPair :
    Actionpair.S with type action = Action.t and type states = States.t =
    Actionpair.Make (Log) (Base) (States) (Annotation) (Action)

  module ActionPairs :
    Actionpairs.S with type states = States.t and type elt = ActionPair.t =
    Actionpairs.Make (Log) (States) (Action) (ActionPair)

  module ActionMap :
    Actionmap.S
    with type label = Label.t
     and type action = Action.t
     and type actions = Actions.t
     and type states = States.t
     and type actionpairs = ActionPairs.t =
    Actionmap.Make (Log) (Base) (States) (Label) (Action) (Actions)
      (ActionPairs)

  module Edge :
    Edge.S
    with type state = State.t
     and type label = Label.t
     and type action = Action.t =
    Edge.Make (Log) (State) (Label) (Action)

  module Edges : Edges.S with type elt = Edge.t and type label = Edge.label =
    Edges.Make (Log) (Edge)

  module EdgeMap :
    Edgemap.S
    with type state = State.t
     and type states = States.t
     and type label = Label.t
     and type transitions = Transitions.t
     and type action = Action.t
     and type actions = Actions.t
     and type actionmap = ActionMap.t'
     and type edges = Edges.t =
    Edgemap.Make (Log) (Base) (State) (States) (Transition) (Transitions)
      (Action)
      (Actions)
      (ActionPairs)
      (ActionMap)
      (Edge)
      (Edges)

  module Partition :
    State_partition.S
    with type elt = States.t
     and type state = State.t
     and type label = Label.t
     and type edgemap = EdgeMap.t' =
    State_partition.Make (Log) (State) (States) (ActionMap) (EdgeMap)

  module Info :
    Info.S
    with type base = Base.t
     and type constructorbindings = ConstructorBindings.t
     and type labels = Labels.t =
    Info.Make (Log) (Base) (Labels) (Bindings) (ConstructorBindings)

  module LTS :
    LTS.S
    with type state = State.t
     and type states = States.t
     and type labels = Labels.t
     and type transitions = Transitions.t
     and type info = Info.t =
    LTS.Make (Log) (State) (States) (Labels) (Transitions) (Info)

  (** [module Saturate ] ...
      (* TODO: the idea of [Traces] needs to be revisited. It does provide optimizations to examples with a lot of silent actions, where the saturated FSM is considerably larger, but i believe that there are areas where this can still be improved. *)
  *)
  module Saturate :
    Saturate.S
    with type state = State.t
     and type states = States.t
     and type label = Label.t
     and type labels = Labels.t
     and type annotation = Annotation.t
     and type trees = Base.Trees.t
     and type action = Action.t
     and type actionpairs = ActionPairs.t
     and type actionmap = ActionMap.t'
     and type edgemap = EdgeMap.t' =
    Saturate.Make (Log) (Base) (State) (States) (Label) (Labels) (Note)
      (Annotation)
      (Annotations)
      (Action)
      (ActionPair)
      (ActionPairs)
      (ActionMap)
      (EdgeMap)

  module FSM :
    FSM.S
    with type state = State.t
     and type states = States.t
     and type labels = Labels.t
     and type edgemap = EdgeMap.t'
     and type info = Info.t
     and type lts = LTS.t =
    FSM.Make (Log) (State) (States) (Labels) (EdgeMap) (Info) (LTS) (Saturate)

  module Minimize :
    Minimize.S
    with type state = State.t
     and type states = States.t
     and type label = Label.t
     and type labels = Labels.t
     and type edgemap = EdgeMap.t'
     and type partition = Partition.t
     and type fsm = FSM.t =
    Minimize.Make (Log) (Base) (State) (States) (Label) (Labels) (Action)
      (ActionMap)
      (EdgeMap)
      (Partition)
      (Info)
      (FSM)

  module Bisimilarity :
    Bisimilarity.S
    with type states = States.t
     and type partition = Partition.t
     and type fsm = FSM.t =
    Bisimilarity.Make (Log) (State) (States) (Label) (Labels) (Action)
      (ActionMap)
      (EdgeMap)
      (Partition)
      (Info)
      (FSM)
      (Minimize)
end
