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

  module LTS = struct
    type t =
      { init : State.t option
      ; terminals : States.t
      ; alphabet : Labels.t
      ; states : States.t
      ; transitions : Transitions.t
      ; info : Info.t
      }

    include
      Json.Thing.Make
        (Log)
        (struct
          type k = t

          let name = "LTS"

          let json ?as_elt (x : t) : Yojson.t =
            `Assoc
              [ "init", Json.option ~as_elt:true State.json x.init
              ; "info", Info.json ~as_elt:true x.info
              ; "terminals", States.json ~as_elt:true x.terminals
              ; "alphabet", Labels.json ~as_elt:true x.alphabet
              ; "states", States.json ~as_elt:true x.states
              ; "transitions", Transitions.json ~as_elt:true x.transitions
              ]
          ;;
        end)
  end

  module FSM = struct
    type t =
      { init : State.t option
      ; terminals : States.t
      ; alphabet : Labels.t
      ; states : States.t
      ; edges : EdgeMap.t'
      ; info : Info.t
      }

    include
      Json.Thing.Make
        (Log)
        (struct
          type k = t

          let name = "FSM"

          let json ?as_elt (x : t) : Yojson.t =
            `Assoc
              [ "init", Json.option ~as_elt:true State.json x.init
              ; "info", Info.json ~as_elt:true x.info
              ; "terminals", States.json ~as_elt:true x.terminals
              ; "alphabet", Labels.json ~as_elt:true x.alphabet
              ; "states", States.json ~as_elt:true x.states
              ; "edges", EdgeMap.json ~as_elt:true x.edges
              ]
          ;;
        end)

    let of_lts (x : LTS.t) : t =
      Log.trace __FUNCTION__;
      { init = x.init
      ; terminals = x.terminals
      ; alphabet = x.alphabet
      ; states = x.states
      ; edges = EdgeMap.of_transitions x.transitions
      ; info = x.info
      }
    ;;

    let merge (a : t) (b : t) : t =
      let init : State.t option = None in
      let terminals : States.t = States.union a.terminals b.terminals in
      let alphabet : Labels.t = Labels.union a.alphabet b.alphabet in
      let states : States.t = States.union a.states b.states in
      let edges : EdgeMap.t' = EdgeMap.merge a.edges b.edges in
      let info : Info.t = Info.merge a.info b.info in
      { init; terminals; alphabet; states; edges; info }
    ;;

    let is_weak_mode (x : t) : bool =
      Bool.not (Labels.is_empty x.info.weak_labels)
    ;;

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

    let saturate ?(only_if_weak : bool = true) (x : t) : t =
      Log.trace __FUNCTION__;
      if only_if_weak && Bool.not (is_weak_mode x)
      then (
        Log.debug ~__FUNCTION__ "Not weak, returning unchanged";
        x)
      else
        { x with
          edges = Saturate.edges x.alphabet x.states (EdgeMap.copy x.edges)
        }
    ;;
  end

  module Minimize =
    Minimize.Make (Log) (Base) (State) (States) (Label) (Labels) (Action)
      (ActionMap)
      (EdgeMap)
      (Partition)
      (Info)
      (FSM)

  module Bisimilarity =
    Bisimilarity.Make (Log) (State) (States) (Label) (Labels) (Action)
      (ActionMap)
      (EdgeMap)
      (Partition)
      (Info)
      (FSM)
      (Minimize)
end
