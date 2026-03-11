module Make : (Log : Logger.S)
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
     end)
    -> sig
  module State : State.S with type base = Base.t
  module States : States.S with type elt = State.t
  module Label : Label.S with type base = Base.t
  module Labels : Labels.S with type elt = Label.t

  module Note :
    Annotation_note.S
    with type state = State.t
     and type label = Label.t
     and type trees = Base.Trees.t

  module Annotation :
    Annotation.S with type label = Label.t and type note = Note.t

  module Annotations : Annotations.S with type elt = Annotation.t

  module Transition :
    Transition.S
    with type state = State.t
     and type label = Label.t
     and type tree = Base.Tree.t
     and type annotation = Annotation.t

  module Transitions :
    Transitions.S with type elt = Transition.t and type labels = Labels.t

  module Action :
    Action.S
    with type label = Label.t
     and type annotation = Annotation.t
     and type trees = Base.Trees.t

  module Actions :
    Actions.S
    with type elt = Action.t
     and type label = Label.t
     and type labels = Labels.t

  module ActionPair :
    Actionpair.S with type action = Action.t and type states = States.t

  module ActionPairs :
    Actionpairs.S with type states = States.t and type elt = ActionPair.t

  module ActionMap :
    Actionmap.S
    with type label = Label.t
     and type action = Action.t
     and type actions = Actions.t
     and type states = States.t
     and type actionpairs = ActionPairs.t

  module Edge :
    Edge.S
    with type state = State.t
     and type label = Label.t
     and type action = Action.t

  module Edges : Edges.S with type elt = Edge.t and type label = Edge.label

  module EdgeMap :
    Edgemap.S
    with type state = State.t
     and type states = States.t
     and type label = Label.t
     and type transitions = Transitions.t
     and type action = Action.t
     and type actions = Actions.t
     and type actionmap = ActionMap.t'
     and type edges = Edges.t

  module Partition :
    State_partition.S
    with type elt = States.t
     and type state = State.t
     and type label = Label.t
     and type edgemap = EdgeMap.t'

  module Info :
    Info.S
    with type base = Base.t
     and type constructorbindings = ConstructorBindings.t
     and type labels = Labels.t

  module LTS : sig
    type t =
      { init : State.t option
      ; terminals : Partition.elt
      ; alphabet : Labels.t
      ; states : Partition.elt
      ; transitions : Transitions.t
      ; info : Info.t
      }

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module FSM : sig
    type t =
      { init : State.t option
      ; terminals : Partition.elt
      ; alphabet : Labels.t
      ; states : Partition.elt
      ; edges : EdgeMap.t'
      ; info : Info.t
      }

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    val of_lts : LTS.t -> t
    val merge : t -> t -> t
    val is_weak_mode : t -> bool
    val saturate : ?only_if_weak:bool -> t -> t
  end

  module Minimize :
      module type of
        Minimize.Make (Log) (Base) (State) (States) (Label) (Labels) (Action)
          (ActionMap)
          (EdgeMap)
          (Partition)
          (Info)
          (FSM)

  module Bisimilarity :
      module type of
        Bisimilarity.Make (Log) (State) (States) (Label) (Labels) (Action)
          (ActionMap)
          (EdgeMap)
          (Partition)
          (Info)
          (FSM)
          (Minimize)
end
