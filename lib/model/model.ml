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
  module State : State.S with type t = Base.t = State.Make (Log) (Base)
  module States : States.S with type elt = State.t = States.Make (Log) (State)
  module Label : Label.S with type t = Base.t Label.t' = Label.Make (Log) (Base)
  module Labels : Labels.S with type elt = Label.t = Labels.Make (Log) (Label)

  (* TODO: working on how to give the rest of them signatures to make maintaining this easier *)
  module Note = Annotation_note.Make (Log) (Base) (State) (Label)
  module Annotation = Annotation.Make (Log) (Label) (Note)
  module Annotations = Annotations.Make (Log) (Note) (Annotation)

  module Transition :
    Transition.S
    with type t = (State.t, Label.t, Base.Tree.t, Annotation.t) Transition.t' =
    Transition.Make (Log) (Base) (State) (Label) (Note) (Annotation)

  module Transitions =
    Transitions.Make (Log) (Base) (State) (Label) (Labels) (Annotation)
      (Transition)

  module Action = Action.Make (Log) (Base) (Label) (Annotation)

  module Actions =
    Actions.Make (Log) (Base) (Label) (Labels) (Annotation) (Action)

  module ActionPair =
    Actionpair.Make (Log) (Base) (State) (States) (Label) (Annotation) (Action)

  module ActionPairs =
    Actionpairs.Make (Log) (Base) (State) (States) (Action) (ActionPair)

  module ActionMap =
    Actionmap.Make (Log) (Base) (State) (States) (Label) (Annotation) (Action)
      (Actions)
      (ActionPair)
      (ActionPairs)

  module Edge = Edge.Make (Log) (State) (Label) (Action)
  module Edges = Edges.Make (Log) (State) (Label) (Action) (Edge)

  module EdgeMap =
    Edgemap.Make (Log) (Base) (State) (States) (Label) (Annotation) (Transition)
      (Transitions)
      (Action)
      (Actions)
      (ActionPair)
      (ActionPairs)
      (ActionMap)
      (Edge)
      (Edges)

  module Partition =
    State_partition.Make (Log) (State) (States) (Label) (Action) (ActionMap)
      (EdgeMap)

  module Info =
    Info.Make (Log) (Base) (Label) (Labels) (Bindings) (ConstructorBindings)

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
    module Saturate =
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
