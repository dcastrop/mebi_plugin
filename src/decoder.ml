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

  (** Plain-text renderings of results ([MeBi Config Output Format Text],
      the default): the states and labels numbered once, with their terms,
      then the moves as [from -label-> to]. With [detail], each move also
      shows its derivation as constructor names ([via s_l(s_pre)]), and the
      relations are listed with their constructors. *)
  module Plain : sig
    (** [lts ~detail x] is the LTS [x] as text. Raises nothing. *)
    val lts : detail:bool -> lts -> string

    (** [fsm ~detail x] is the FSM [x] as text. Raises nothing. *)
    val fsm : detail:bool -> fsm -> string

    (** [result ~detail x] is the bisimilarity result [x] as text: whether
        the initial states are related, and the classes, each as its
        states' terms. [detail] adds nothing. Raises nothing. *)
    val result : detail:bool -> result -> string
  end
end

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
   and type bisimilarity = Model.Bisimilarity.t = struct
  type enc = Enc.t
  type state = Model.State.t
  type states = Model.State.Set.t
  type partition = Model.Partition.t
  type label = Model.Label.t
  type labels = Model.Label.Set.t
  type note = Model.Note.t
  type annotation = Model.Annotation.t
  type annotations = Model.Annotation.Set.t
  type transition = Model.Transition.t
  type transitions = Model.Transition.Set.t
  type action = Model.Action.t
  type actions = Model.Action.Set.t
  type actionmap = Model.Action.Map.t'
  type edgemap = Model.EdgeMap.t'
  type rocqlts = Model.Info.Meta.RocqLTS.t
  type info = Model.Info.t
  type lts = Model.LTS.t
  type fsm = Model.FSM.t
  type result = Model.Bisimilarity.Result.t
  type bisimilarity = Model.Bisimilarity.t

  (* See the [.mli]. *)
  let enc (x : Enc.t) : EConstr.t = M.decode x

  (* See the [.mli]. *)
  let handle (x : Enc.t) (e : exn) : EConstr.t =
    try enc x with M.CannotDecode _ -> raise e
  ;;

  exception CouldNotDecode_State of Model.State.t

  (* See the [.mli]. *)
  let state (x : Model.State.t) : EConstr.t =
    handle x.base (CouldNotDecode_State x)
  ;;

  exception CouldNotDecode_Label of Model.Label.t

  (* See the [.mli]. *)
  let label (x : Model.Label.t) : EConstr.t =
    handle x.base (CouldNotDecode_Label x)
  ;;

  exception CouldNotDecode_LTS_Constructor of Model.Info.Meta.RocqLTS.t

  (* See the [.mli]. *)
  let lts_constructor (x : Model.Info.Meta.RocqLTS.t) : EConstr.t =
    handle x.base (CouldNotDecode_LTS_Constructor x)
  ;;

  (* See the [.mli]: one JSON printer per model component. *)
  module Base = struct
    include Json.Thing.Make (struct
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
      module Node = Json.Thing.Make (struct
          type k = Enc.Tree.Node.t

          let name = "Node"

          let json ?as_elt (x : Enc.Tree.Node.t) : Yojson.t =
            `Assoc [ "base", json ~as_elt:true (fst x); "index", `Int (snd x) ]
          ;;
        end)

      include Json.Thing.Make (struct
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

    module Trees = Json.Set.Make (struct
        module Set = Enc.Trees

        let name = "Trees"
        let json = Tree.json
      end)
  end

  module State = Json.Thing.Make (struct
      type k = Model.State.t

      let name = "State"

      let json ?(as_elt : bool = false) (x : Model.State.t) : Yojson.t =
        Base.json ~as_elt:true x.base
      ;;
    end)

  module States = Json.Set.Make (struct
      module Set = Model.State.Set

      let name = "States"
      let json = State.json
    end)

  module Partition = Json.Set.Make (struct
      module Set = Model.Partition

      let name = "Paritition"
      let json = States.json
    end)

  module Label = Json.Thing.Make (struct
      type k = Model.Label.t

      let name = "Label"

      let json ?(as_elt : bool = false) (x : Model.Label.t) : Yojson.t =
        `Assoc
          [ "EConstr", Base.json ~as_elt:true x.base
          ; ( "is_silent"
            , Json.option ~as_elt:true (fun ?as_elt x -> `Bool x) x.is_silent )
          ]
      ;;
    end)

  module Labels = Json.Set.Make (struct
      module Set = Model.Label.Set

      let name = "Labels"
      let json = Label.json
    end)

  module Note = Json.Thing.Make (struct
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

  module Annotation = Json.Thing.Make (struct
      type k = Model.Annotation.t

      let name = "Annotation"

      let rec json ?(as_elt : bool = false) (x : Model.Annotation.t) : Yojson.t =
        `Assoc
          [ "this", Note.json ~as_elt:true x.this
          ; ( "next"
            , match x.next with
              | None -> `String "None"
              | Some next -> json ~as_elt:true next )
          ]
      ;;
    end)

  module Annotations = Json.Set.Make (struct
      module Set = Model.Annotation.Set

      let name = "Annotations"
      let json = Annotation.json
    end)

  module Transition = Json.Thing.Make (struct
      type k = Model.Transition.t

      let name = "Transition"

      let json ?(as_elt : bool = false) (x : Model.Transition.t) : Yojson.t =
        `Assoc
          [ "from", Base.json ~as_elt:true x.from.base
          ; "goto", Base.json ~as_elt:true x.goto.base
          ; "label", Base.json ~as_elt:true x.label.base
          ; "annotation", Json.option ~as_elt:true Annotation.json x.annotation
          ; "tree", Json.option ~as_elt:true Base.Tree.json x.tree
          ]
      ;;
    end)

  module Transitions = Json.Set.Make (struct
      module Set = Model.Transition.Set

      let name = "Transitions"
      let json = Transition.json
    end)

  module Action = Json.Thing.Make (struct
      type k = Model.Action.t

      let name = "Action"

      let json ?(as_elt : bool = false) (x : Model.Action.t) : Yojson.t =
        `Assoc
          [ "label", Base.json ~as_elt:true x.label.base
          ; "annotation", Json.option ~as_elt:true Annotation.json x.annotation
          ; "trees", Base.Trees.json ~as_elt:true x.trees
          ]
      ;;
    end)

  module ActionMap =
    Json.Map.Make
      (struct
        module Map = Model.Action.Map

        type value = Model.State.Set.t

        let name = "ActionMap"
      end)
      (Model.Action)
      (struct
        include Model.State.Set

        let name = "Destinations"
      end)

  module EdgeMap =
    Json.Map.Make
      (struct
        module Map = Model.EdgeMap

        type value = Model.Action.Map.t'

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
      module RocqLTS = Json.Thing.Make (struct
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

      include Json.Thing.Make (struct
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

    include Json.Thing.Make (struct
        type k = Model.Info.t

        let name = "Info"

        let json ?as_elt (x : Model.Info.t) : Yojson.t =
          `Assoc
            [ ( "nums"
              , Json.option
                  (fun ?as_elt ({ states; labels; edges } : Model.Info.nums) ->
                    `Assoc
                      [ "states", `Int states
                      ; "labels", `Int labels
                      ; "edges", `Int edges
                      ])
                  x.nums )
            ; "meta", Json.option ~as_elt:true Meta.json x.meta
            ; "weak labels", Labels.json ~as_elt:true x.weak_labels
            ]
        ;;
      end)
  end

  module LTS = Json.Thing.Make (struct
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

  module FSM = Json.Thing.Make (struct
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

  module Result = Json.Thing.Make (struct
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
    module FSMPair = Json.Thing.Make (struct
        type k = Model.Bisimilarity.FSMPair.t

        let name = "FSM Pair"

        let json ?as_elt (x : Model.Bisimilarity.FSMPair.t) : Yojson.t =
          `Assoc
            [ "original", FSM.json ~as_elt:true x.original
            ; "saturated", FSM.json ~as_elt:true x.saturated
            ]
        ;;
      end)

    include Json.Thing.Make (struct
        type k = Model.Bisimilarity.t

        let name = "Bisimilarity"

        let json ?(as_elt : bool = false) (x : Model.Bisimilarity.t) : Yojson.t =
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

  (* See the [.mli]. *)
  module Plain = struct
    module SMap = Map.Make (Model.State)
    module LMap = Map.Make (Model.Label)

    (** [term x] is the Rocq term [x] encodes, printed. *)
    let term (x : Enc.t) : string = M.decode x |> M.Strfy.econstr

    (** [constructor (r, i)] is the name of the [i]th constructor of the
        relation [r], or [r#i] if [r] does not decode to an inductive. *)
    let constructor ((r, i) : Enc.Tree.Node.t) : string =
      let env = Global.env () in
      let sigma = Evd.from_env env in
      match EConstr.kind sigma (M.decode r) with
      | Ind ((mind, j), _) ->
        let names =
          (Environ.lookup_mind mind env).mind_packets.(j).mind_consnames
        in
        if i < Array.length names
        then Names.Id.to_string names.(i)
        else Printf.sprintf "%s#%i" (term r) i
      | _ -> Printf.sprintf "%s#%i" (term r) i
    ;;

    (** [tree t] is the derivation [t] as constructor names, the premises'
        derivations in brackets: [s_l(s_pre)]. *)
    let rec tree (Enc.Tree.N (n, ts) : Enc.Tree.t) : string =
      match ts with
      | [] -> constructor n
      | ts ->
        Printf.sprintf
          "%s(%s)"
          (constructor n)
          (String.concat ", " (List.map tree ts))
    ;;

    (** [relations info] is one line per relation the system was extracted
        with, naming its constructors. *)
    let relations (info : Model.Info.t) : string list =
      match info.meta with
      | None -> []
      | Some meta ->
        "relations:"
        :: List.map
             (fun (r : Model.Info.Meta.RocqLTS.t) ->
               let n = List.length r.constructors in
               Printf.sprintf
                 "  %s: %s"
                 (term r.base)
                 (String.concat
                    ", "
                    (List.init n (fun i -> constructor (r.base, i)))))
             meta.lts
    ;;

    (** [header kind info init nstates nlabels nmoves moves] is the first
        line: the sizes, whether complete, and the initial state. *)
    let header
          (kind : string)
          (info : Model.Info.t)
          (init : string)
          ((ns, nl, nm, moves) : int * int * int * string)
      : string
      =
      let complete =
        match info.meta with
        | Some { is_complete = false; _ } -> ", incomplete"
        | _ -> ""
      in
      Printf.sprintf
        "%s: %i states, %i labels, %i %s%s.%s"
        kind
        ns
        nl
        nm
        moves
        complete
        init
    ;;

    (** [tables states labels] numbers [states] and [labels] in order, and
        is their tables' lines with the two numberings. *)
    let tables
          (states : Model.State.Set.t)
          (terminals : Model.State.Set.t)
          (labels : Model.Label.t list)
      : string list * int SMap.t * int LMap.t
      =
      let sidx, _ =
        Model.State.Set.fold
          (fun s (m, i) -> SMap.add s i m, i + 1)
          states
          (SMap.empty, 0)
      in
      let lidx, _ =
        List.fold_left
          (fun (m, i) l -> if LMap.mem l m then m, i else LMap.add l i m, i + 1)
          (LMap.empty, 0)
          labels
      in
      let state_lines =
        SMap.bindings sidx
        |> List.sort (fun (_, a) (_, b) -> Int.compare a b)
        |> List.map (fun ((s : Model.State.t), i) ->
          Printf.sprintf
            "  %i  %s%s"
            i
            (term s.base)
            (if Model.State.Set.mem s terminals then "  (terminal)" else ""))
      in
      let label_lines =
        LMap.bindings lidx
        |> List.sort (fun (_, a) (_, b) -> Int.compare a b)
        |> List.map (fun ((l : Model.Label.t), i) ->
          Printf.sprintf
            "  %i  %s%s"
            i
            (term l.base)
            (if l.is_silent = Some true then "  (silent)" else ""))
      in
      ("states:" :: state_lines) @ ("labels:" :: label_lines), sidx, lidx
    ;;

    (** [moves word lines] is the moves section, sorted. *)
    let moves (word : string) (lines : ((int * int * int) * string) list)
      : string list
      =
      (word ^ ":")
      :: (List.sort_uniq Stdlib.compare lines
          |> List.map (fun ((f, l, t), via) ->
            Printf.sprintf "  %i -%i-> %i%s" f l t via))
    ;;

    let init_line sidx = function
      | Some s ->
        (match SMap.find_opt s sidx with
         | Some i -> Printf.sprintf " Initial state: %i." i
         | None -> "")
      | None -> ""
    ;;

    (* See the [.mli]. *)
    let lts ~(detail : bool) (x : Model.LTS.t) : string =
      let labels =
        Model.Label.Set.elements x.alphabet
        @ List.map
            (fun (t : Model.Transition.t) -> t.label)
            (Model.Transition.Set.elements x.transitions)
      in
      let tabs, sidx, lidx = tables x.states x.terminals labels in
      let edges =
        Model.Transition.Set.elements x.transitions
        |> List.map (fun (t : Model.Transition.t) ->
          ( ( SMap.find t.from sidx
            , LMap.find t.label lidx
            , SMap.find t.goto sidx )
          , match detail, t.tree with
            | true, Some d -> "  via " ^ tree d
            | _ -> "" ))
      in
      String.concat
        "\n"
        ((header
            "LTS"
            x.info
            (init_line sidx x.init)
            ( Model.State.Set.cardinal x.states
            , LMap.cardinal lidx
            , List.length edges
            , "transitions" )
          :: tabs)
         @ moves "transitions" edges
         @ if detail then relations x.info else [])
    ;;

    (* See the [.mli]. *)
    let fsm ~(detail : bool) (x : Model.FSM.t) : string =
      let triples =
        Model.EdgeMap.fold
          (fun from am acc ->
            Model.Action.Map.fold
              (fun (a : Model.Action.t) dests acc ->
                Model.State.Set.fold
                  (fun d acc -> (from, a, d) :: acc)
                  dests
                  acc)
              am
              acc)
          x.edges
          []
      in
      let labels =
        Model.Label.Set.elements x.alphabet
        @ List.map (fun (_, (a : Model.Action.t), _) -> a.label) triples
      in
      let tabs, sidx, lidx = tables x.states x.terminals labels in
      let edges =
        List.map
          (fun ((f : Model.State.t), (a : Model.Action.t), (d : Model.State.t)) ->
            ( (SMap.find f sidx, LMap.find a.label lidx, SMap.find d sidx)
            , if detail && Bool.not (Enc.Trees.is_empty a.trees)
              then
                "  via "
                ^ String.concat
                    " | "
                    (List.map tree (Enc.Trees.elements a.trees))
              else "" ))
          triples
      in
      String.concat
        "\n"
        ((header
            "FSM"
            x.info
            (init_line sidx x.init)
            ( Model.State.Set.cardinal x.states
            , LMap.cardinal lidx
            , List.length (List.sort_uniq Stdlib.compare (List.map fst edges))
            , "edges" )
          :: tabs)
         @ moves "edges" edges
         @ if detail then relations x.info else [])
    ;;

    (* See the [.mli]. *)
    let result ~detail:(_ : bool) (x : Model.Bisimilarity.Result.t) : string =
      let classes (p : Model.Partition.t) : string list =
        Model.Partition.elements p
        |> List.mapi (fun i (c : Model.State.Set.t) ->
          Printf.sprintf
            "  %i  %s"
            i
            (Model.State.Set.elements c
             |> List.map (fun (s : Model.State.t) -> term s.base)
             |> String.concat " | "))
      in
      let related =
        match x.roots_related with
        | Some true -> "yes"
        | Some false -> "no"
        | None -> "unknown"
      in
      String.concat
        "\n"
        ((Printf.sprintf
            "Bisimilarity: initial states related: %s. %i classes with states \
             of both systems, %i with states of one."
            related
            (Model.Partition.cardinal x.bisim_states)
            (Model.Partition.cardinal x.non_bisim_states)
          :: "classes with states of both systems:"
          :: classes x.bisim_states)
         @ ("classes with states of one system:" :: classes x.non_bisim_states)
        )
    ;;
  end
end
