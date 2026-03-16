module type S = sig
  type enc
  type state
  type states
  type partition
  type label
  type labels
  type note
  type annotation
  type annotations
  type transition
  type transitions
  type action
  type actions
  type actionmap
  type edgemap
  type rocqlts
  type info
  type lts
  type fsm
  type result
  type bisimilarity

  val enc : enc -> EConstr.t
  val handle : enc -> exn -> EConstr.t

  exception CouldNotDecode_State of state

  val state : state -> EConstr.t

  exception CouldNotDecode_Label of label

  val label : label -> EConstr.t

  exception CouldNotDecode_LTS_Constructor of rocqlts

  val lts_constructor : rocqlts -> EConstr.t

  module Base : Json.S with type k = enc
  module State : Json.S with type k = state
  module States : Json.S with type k = states
  module Partition : Json.S with type k = partition
  module Label : Json.S with type k = label
  module Labels : Json.S with type k = labels
  module Note : Json.S with type k = note
  module Annotation : Json.S with type k = annotation
  module Annotations : Json.S with type k = annotations
  module Transition : Json.S with type k = transition
  module Transitions : Json.S with type k = transitions
  module Action : Json.S with type k = action
  module ActionMap : Json.S with type k = actionmap
  module EdgeMap : Json.S with type k = edgemap
  module Info : Json.S with type k = info
  module LTS : Json.S with type k = lts
  module FSM : Json.S with type k = fsm
  module Result : Json.S with type k = result
  module Bisimilarity : Json.S with type k = bisimilarity
end

module Make
    (Log : Logger.S)
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
   and type states = Model.States.t
   and type partition = Model.Partition.t
   and type label = Model.Label.t
   and type labels = Model.Labels.t
   and type note = Model.Note.t
   and type annotation = Model.Annotation.t
   and type annotations = Model.Annotations.t
   and type transition = Model.Transition.t
   and type transitions = Model.Transitions.t
   and type action = Model.Action.t
   and type actions = Model.Actions.t
   and type actionmap = Model.ActionMap.t'
   and type edgemap = Model.EdgeMap.t'
   and type rocqlts = Model.Info.Meta.RocqLTS.t
   and type info = Model.Info.t
   and type lts = Model.LTS.t
   and type fsm = Model.FSM.t
   and type result = Model.Bisimilarity.Result.t
   and type bisimilarity = Model.Bisimilarity.t = struct
  type enc = Enc.t
  type state = Model.State.t
  type states = Model.States.t
  type partition = Model.Partition.t
  type label = Model.Label.t
  type labels = Model.Labels.t
  type note = Model.Note.t
  type annotation = Model.Annotation.t
  type annotations = Model.Annotations.t
  type transition = Model.Transition.t
  type transitions = Model.Transitions.t
  type action = Model.Action.t
  type actions = Model.Actions.t
  type actionmap = Model.ActionMap.t'
  type edgemap = Model.EdgeMap.t'
  type rocqlts = Model.Info.Meta.RocqLTS.t
  type info = Model.Info.t
  type lts = Model.LTS.t
  type fsm = Model.FSM.t
  type result = Model.Bisimilarity.Result.t
  type bisimilarity = Model.Bisimilarity.t

  let enc (x : Enc.t) : EConstr.t = M.decode x

  let handle (x : Enc.t) (e : exn) : EConstr.t =
    try enc x with M.CannotDecode _ -> raise e
  ;;

  exception CouldNotDecode_State of Model.State.t

  let state (x : Model.State.t) : EConstr.t =
    handle x.base (CouldNotDecode_State x)
  ;;

  exception CouldNotDecode_Label of Model.Label.t

  let label (x : Model.Label.t) : EConstr.t =
    handle x.base (CouldNotDecode_Label x)
  ;;

  exception CouldNotDecode_LTS_Constructor of Model.Info.Meta.RocqLTS.t

  let lts_constructor (x : Model.Info.Meta.RocqLTS.t) : EConstr.t =
    handle x.base (CouldNotDecode_LTS_Constructor x)
  ;;

  (** [module Decode.ModelToString] handles the decoding of [module Model] components to their decoded and pretty-printed counterparts.
  *)
  module Base = struct
    include
      Json.Thing.Make
        (Log)
        (struct
          type k = Enc.t

          let name = "Base"

          let json ?(as_elt : bool = false) (x : Enc.t) : Yojson.t =
            `Assoc
              [ "enc", Enc.json ~as_elt:true x
              ; "term", `String (M.decode x |> M.Strfy.econstr)
              ]
          ;;
        end)

    module Tree = struct
      module Node =
        Json.Thing.Make
          (Log)
          (struct
            type k = Enc.Tree.Node.t

            let name = "Node"

            let json ?as_elt (x : Enc.Tree.Node.t) : Yojson.t =
              `Assoc
                [ "base", json ~as_elt:true (fst x); "index", `Int (snd x) ]
            ;;
          end)

      include
        Json.Thing.Make
          (Log)
          (struct
            type k = Enc.Tree.t

            let name = "Tree"

            let rec json ?as_elt (N (x, xl) : Enc.Tree.t) : Yojson.t =
              `Assoc
                [ "node", Node.json ~as_elt:true x
                ; "cons", `List (List.map (json ~as_elt:true) xl)
                ]
            ;;
          end)
    end

    module Trees =
      Json.Set.Make
        (Log)
        (struct
          module Set = Enc.Trees

          let name = "Trees"
          let json = Tree.json
        end)
  end

  module State =
    Json.Thing.Make
      (Log)
      (struct
        type k = Model.State.t

        let name = "State"

        let json ?(as_elt : bool = false) (x : Model.State.t) : Yojson.t =
          Base.json ~as_elt:true x.base
        ;;
      end)

  module States =
    Json.Set.Make
      (Log)
      (struct
        module Set = Model.States

        let name = "States"
        let json = State.json
      end)

  module Partition =
    Json.Set.Make
      (Log)
      (struct
        module Set = Model.Partition

        let name = "Paritition"
        let json = States.json
      end)

  module Label =
    Json.Thing.Make
      (Log)
      (struct
        type k = Model.Label.t

        let name = "Label"

        let json ?(as_elt : bool = false) (x : Model.Label.t) : Yojson.t =
          `Assoc
            [ "EConstr", Base.json ~as_elt:true x.base
            ; ( "is_silent"
              , Json.option ~as_elt:true (fun ?as_elt x -> `Bool x) x.is_silent
              )
            ]
        ;;
      end)

  module Labels =
    Json.Set.Make
      (Log)
      (struct
        module Set = Model.Labels

        let name = "Labels"
        let json = Label.json
      end)

  module Note =
    Json.Thing.Make
      (Log)
      (struct
        type k = Model.Note.t

        let name = "Note"

        let json ?(as_elt : bool = false) (x : Model.Note.t) : Yojson.t =
          `Assoc
            [ "from", State.json ~as_elt:true x.from
            ; "label", Label.json ~as_elt:true x.label
            ; "goto", State.json ~as_elt:true x.goto
            ; "using", Base.Trees.json ~as_elt:true x.using
            ]
        ;;
      end)

  module Annotation =
    Json.Thing.Make
      (Log)
      (struct
        type k = Model.Annotation.t

        let name = "Annotation"

        let rec json ?(as_elt : bool = false) (x : Model.Annotation.t)
          : Yojson.t
          =
          `Assoc
            [ "this", Note.json ~as_elt:true x.this
            ; ( "next"
              , match x.next with
                | None -> `String "None"
                | Some next -> json ~as_elt:true next )
            ]
        ;;
      end)

  module Annotations =
    Json.Set.Make
      (Log)
      (struct
        module Set = Model.Annotations

        let name = "Annotations"
        let json = Annotation.json
      end)

  module Transition =
    Json.Thing.Make
      (Log)
      (struct
        type k = Model.Transition.t

        let name = "Transition"

        let json ?(as_elt : bool = false) (x : Model.Transition.t) : Yojson.t =
          `Assoc
            [ "from", State.json ~as_elt:true x.from
            ; "goto", State.json ~as_elt:true x.goto
            ; "label", Label.json ~as_elt:true x.label
            ; ( "annotation"
              , Json.option ~as_elt:true Annotation.json x.annotation )
            ; "tree", Json.option ~as_elt:true Base.Tree.json x.tree
            ]
        ;;
      end)

  module Transitions =
    Json.Set.Make
      (Log)
      (struct
        module Set = Model.Transitions

        let name = "Transitions"
        let json = Transition.json
      end)

  module Action =
    Json.Thing.Make
      (Log)
      (struct
        type k = Model.Action.t

        let name = "Action"

        let json ?(as_elt : bool = false) (x : Model.Action.t) : Yojson.t =
          `Assoc
            [ "label", Label.json ~as_elt:true x.label
            ; ( "annotation"
              , Json.option ~as_elt:true Annotation.json x.annotation )
            ; "trees", Base.Trees.json ~as_elt:true x.trees
            ]
        ;;
      end)

  module ActionMap =
    Json.Map.Make
      (Log)
      (struct
        module Map = Model.ActionMap

        type value = Model.States.t

        let name = "ActionMap"
      end)
      (Model.Action)
      (struct
        include Model.States

        let name = "Destinations"
      end)

  module EdgeMap =
    Json.Map.Make
      (Log)
      (struct
        module Map = Model.EdgeMap

        type value = Model.ActionMap.t'

        let name = "EdgeMap"
      end)
      (struct
        include Model.State

        let name = "From"
      end)
      (struct
        include ActionMap

        let name = "Actions"
        let compare a b : int = 0
      end)

  module Info = struct
    module Meta = struct
      module RocqLTS =
        Json.Thing.Make
          (Log)
          (struct
            type k = Model.Info.Meta.RocqLTS.t

            let name = "RocqLTS"

            let json ?(as_elt : bool = false) (x : Model.Info.Meta.RocqLTS.t)
              : Yojson.t
              =
              `Assoc
                [ "base", Base.json ~as_elt:true x.base
                ; ( "constructors"
                  , `List
                      (List.map
                         (ConstructorBindings.json ~as_elt:true)
                         x.constructors) )
                ]
            ;;
          end)

      include
        Json.Thing.Make
          (Log)
          (struct
            type k = Model.Info.Meta.t

            let name = "Meta"

            let json ?as_elt (x : Model.Info.Meta.t) : Yojson.t =
              `Assoc
                [ "complete", `Bool x.is_complete
                ; "merged", `Bool x.is_merged
                ; "bounds", Model.Info.Meta.Bounds.json ~as_elt:true x.bounds
                ; "rocq lts", `List (List.map (RocqLTS.json ~as_elt:true) x.lts)
                ]
            ;;
          end)
    end

    include
      Json.Thing.Make
        (Log)
        (struct
          type k = Model.Info.t

          let name = "Info"

          let json ?as_elt (x : Model.Info.t) : Yojson.t =
            `Assoc
              [ "meta", Json.option ~as_elt:true Meta.json x.meta
              ; "weak labels", Labels.json ~as_elt:true x.weak_labels
              ]
          ;;
        end)
  end

  module LTS =
    Json.Thing.Make
      (Log)
      (struct
        type k = Model.LTS.t

        let name = "LTS"

        let json ?as_elt (x : Model.LTS.t) : Yojson.t =
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

  module FSM =
    Json.Thing.Make
      (Log)
      (struct
        type k = Model.FSM.t

        let name = "FSM"

        let json ?(as_elt : bool = false) (x : Model.FSM.t) : Yojson.t =
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

  module Result =
    Json.Thing.Make
      (Log)
      (struct
        type k = Model.Bisimilarity.Result.t

        let name = "Result"

        let json ?as_elt (x : Model.Bisimilarity.Result.t) : Yojson.t =
          `Assoc
            [ "bisimilar states", Partition.json ~as_elt:true x.bisim_states
            ; ( "non-bisimilar states"
              , Partition.json ~as_elt:true x.non_bisim_states )
            ]
        ;;
      end)

  module Bisimilarity = struct
    module FSMPair =
      Json.Thing.Make
        (Log)
        (struct
          type k = Model.Bisimilarity.FSMPair.t

          let name = "FSM Pair"

          let json ?as_elt (x : Model.Bisimilarity.FSMPair.t) : Yojson.t =
            `Assoc
              [ "original", FSM.json ~as_elt:true x.original
              ; "saturated", FSM.json ~as_elt:true x.saturated
              ]
          ;;
        end)

    include
      Json.Thing.Make
        (Log)
        (struct
          type k = Model.Bisimilarity.t

          let name = "Bisimilarity"

          let json ?(as_elt : bool = false) (x : Model.Bisimilarity.t)
            : Yojson.t
            =
            `Assoc
              [ ( "fsms"
                , `Assoc
                    [ "a", FSMPair.json ~as_elt:true x.fsm_a
                    ; "b", FSMPair.json ~as_elt:true x.fsm_b
                    ; "merged", FSM.json ~as_elt:true x.merged
                    ] )
              ; "result", Result.json ~as_elt:true x.result
              ]
          ;;
        end)
  end
end
