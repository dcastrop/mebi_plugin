module type S = sig
  module Enc : Encoding.SEncoding

  (* *)
  module type SState = sig
    type t =
      { term : Enc.t
      ; pp : string option
      }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val to_string : t -> string
  end

  module State : SState

  module type SStates = sig
    module S : Set.S with type elt = State.t

    type t = S.t

    val to_string : t -> string
  end

  module States : SStates

  (* *)
  module type SLabel = sig
    type t =
      { term : Enc.t
      ; is_silent : bool option
      }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val to_string : t -> string
    val is_silent : t -> bool
  end

  module Label : SLabel

  module type SLabels = sig
    module S : Set.S with type elt = Label.t

    type t = S.t

    val to_string : t -> string
  end

  module Labels : SLabels

  (* *)
  module type SNote = sig
    type t =
      { from : State.t
      ; label : Label.t
      ; using : CTrees.t
      ; goto : State.t
      }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val to_string : t -> string
    val is_silent : t -> bool
  end

  module Note : SNote

  (* *)
  module type SAnnotation = sig
    type t =
      { this : Note.t
      ; next : t option
      }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val to_string : t -> string
    val is_empty : t -> bool
    val length : t -> int
    val shorter : t -> t -> t
    val exists : Note.t -> t -> bool
    val exists_label : Label.t -> t -> bool
    val append : Note.t -> t -> t
    val last : t -> Note.t

    exception CannotDropLastOfSingleton of t

    val drop_last : t -> t
  end

  module Annotation : SAnnotation

  (* *)
  module type STransition = sig
    type t =
      { from : State.t
      ; goto : State.t
      ; label : Label.t
      ; annotation : Annotation.t option
      ; constructor_trees : CTrees.t
      }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val is_silent : t -> bool
    val annotation_is_empty : t -> bool
    val to_string : t -> string
  end

  module Transition : STransition

  module type STransitions = sig
    module S : Set.S with type elt = Transition.t

    type t = S.t

    val to_string : t -> string
  end

  module Transitions : STransitions

  (* *)
  module type SAction = sig
    type t =
      { label : Label.t
      ; annotation : Annotation.t option
      ; constructor_trees : CTrees.t
      }

    val wk_equal : t -> t -> bool
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val is_silent : t -> bool
    val annotation_is_empty : t -> bool
    val to_string : t -> string
  end

  module Action : SAction

  module type SActions = sig
    module S : Set.S with type elt = Action.t

    type t = S.t

    val labelled : t -> Label.t -> t
    val to_string : t -> string
  end

  module Actions : SActions

  module type SActionMap = sig
    module M : Hashtbl.S with type key = Action.t

    type t = States.t M.t

    val update : t -> Action.t -> States.t -> unit
    val get_destinations : t -> States.t
    val to_string : t -> string
  end

  module ActionMap : SActionMap

  (* *)
  module type SEdge = sig
    type t =
      { from : State.t
      ; goto : State.t
      ; action : Action.t
      }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val is_silent : t -> bool
    val to_string : t -> string
  end

  module Edge : SEdge

  module type SEdges = sig
    module S : Set.S with type elt = Edge.t

    type t = S.t

    val labelled : t -> Label.t -> t
    val to_string : t -> string
  end

  module Edges : SEdges

  module type SEdgeMap = sig
    module M : Hashtbl.S with type key = State.t

    type t = ActionMap.t M.t

    val update : t -> State.t -> Action.t -> States.t -> unit
    val get_destinations : t -> State.t -> States.t
    val get_actions : t -> State.t -> Actions.t
    val get_edges : t -> State.t -> Edges.t
    val to_string : t -> string
  end

  module EdgeMap : SEdgeMap

  (* *)
  module type SParitition = sig
    module S : Set.S with type elt = States.t

    type t = S.t

    val get_reachable : t -> State.t -> EdgeMap.t -> t
    val to_string : t -> string
  end

  module Paritition : SParitition

  (* *)
  module type SInfo = sig
    type t =
      { meta : meta option
      ; weak_labels : Labels.t
      }

    and meta =
      { is_complete : bool
      ; is_merged : bool
      ; bounds : bounds
      ; lts : lts list
      }

    and bounds =
      | States of int
      | Transitions of int

    and lts =
      { enc : Enc.t
      ; constructors : Rocq_bindings.constructor list
      }

    val to_string : t -> string
  end

  module Info : SInfo

  (* *)
  module type SLTS = sig
    type t =
      { init : State.t option
      ; terminals : States.t
      ; alphabet : Labels.t
      ; states : States.t
      ; transitions : Transitions.t
      ; info : Info.t
      }

    val to_string : t -> string
  end

  module LTS : SLTS
end

module Make (T : Term.TYPE) : TYPE = struct
  module T : Term.TYPE = T

  (* *)
  module type SState = sig
    type t =
      { term : Enc.t
      ; pp : string option
      }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val to_string : t -> string
  end

  module State : SState = struct
    type t =
      { term : Enc.t
      ; pp : string option
      }

    let equal (a : t) (b : t) : bool = Enc.equal a.term b.term
    let compare (a : t) (b : t) : int = Enc.compare a.term b.term
    let hash (x : t) : int = Enc.hash x.term

    (* *)
    let to_string (x : t) : string =
      Utils.Strfy.record
        [ "term", Enc.to_string x.term
        ; "pp", Utils.Strfy.option (Args Utils.Strfy.string) x.pp
        ]
    ;;
  end

  module type SStates = sig
    module S : Set.S with type elt = State.t

    type t = S.t

    val to_string : t -> string
  end

  module States : SStates = struct
    module S : Set.S with type elt = State.t = Set.Make (State)

    type t = S.t

    let to_string (xs : t) : string =
      S.to_list xs
      |> Utils.Strfy.list
           ~args:{ (Utils.Strfy.style_args ()) with name = Some "States" }
           (Of State.to_string)
    ;;
  end

  (* *)
  module type SLabel = sig
    type t =
      { term : Enc.t
      ; is_silent : bool option
      }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val to_string : t -> string
    val is_silent : t -> bool
  end

  module Label : SLabel = struct
    type t =
      { term : Enc.t
      ; is_silent : bool option
      }

    let equal (a : t) (b : t) : bool = Enc.equal a.term b.term
    let compare (a : t) (b : t) : int = Enc.compare a.term b.term
    let hash (x : t) : int = Enc.hash x.term

    (* *)
    let to_string (x : t) : string =
      Utils.Strfy.record
        [ "term", Enc.to_string x.term
        ; "is_silent", Utils.Strfy.option (Args Utils.Strfy.bool) x.is_silent
        ]
    ;;

    (* *)
    let is_silent (x : t) : bool = Option.default false x.is_silent
  end

  module type SLabels = sig
    module S : Set.S with type elt = Label.t

    type t = S.t

    val to_string : t -> string
  end

  module Labels : SLabels = struct
    module S : Set.S with type elt = Label.t = Set.Make (Label)

    type t = S.t

    let to_string (xs : t) : string =
      S.to_list xs
      |> Utils.Strfy.list
           ~args:{ (Utils.Strfy.style_args ()) with name = Some "Labels" }
           (Of Label.to_string)
    ;;
  end

  (* *)
  module CTree : Constructor.Tree.TYPE = Constructor.Tree.Make (T)
  module CTrees : Set.S with type elt = CTree.t = Set.Make (CTree)

  (* *)
  module type SNote = sig
    type t =
      { from : State.t
      ; label : Label.t
      ; using : CTrees.t
      ; goto : State.t
      }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val to_string : t -> string
    val is_silent : t -> bool
  end

  module Note : SNote = struct
    type t =
      { from : State.t
      ; label : Label.t
      ; using : CTrees.t
      ; goto : State.t
      }

    let equal (a : t) (b : t) : bool =
      State.equal a.from b.from
      && State.equal a.goto b.goto
      && Label.equal a.label b.label
      && CTrees.equal a.using b.using
    ;;

    let compare (a : t) (b : t) : int =
      Utils.compare_chain
        [ State.compare a.from b.from
        ; State.compare a.goto b.goto
        ; Label.compare a.label b.label
        ; CTrees.compare a.using b.using
        ]
    ;;

    (* *)
    let to_string (x : t) : string =
      (* Utils.Strfy.record
            [ "from", S.to_string x.from
            ; "goto", S.to_string x.goto
            ; "via", L.to_string x.via
            ; "using", Utils.Strfy.list (Of C.to_string) x.using
           
            ] *)
      Printf.sprintf
        "<State (%s) Goto (%s) Via (%s)>"
        (State.to_string x.from)
        (State.to_string x.goto)
        (Label.to_string x.label)
    ;;

    (* *)
    let is_silent (x : t) : bool = Label.is_silent x.label
  end

  (* *)
  module type SAnnotation = sig
    type t =
      { this : Note.t
      ; next : t option
      }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val to_string : t -> string
    val is_empty : t -> bool
    val length : t -> int
    val shorter : t -> t -> t
    val exists : Note.t -> t -> bool
    val exists_label : Label.t -> t -> bool
    val append : Note.t -> t -> t
    val last : t -> Note.t

    exception CannotDropLastOfSingleton of t

    val drop_last : t -> t
  end

  module Annotation : SAnnotation = struct
    type t =
      { this : Note.t
      ; next : t option
      }

    let rec equal (a : t) (b : t) : bool =
      Note.equal a.this b.this && Option.equal equal a.next b.next
    ;;

    let rec compare (a : t) (b : t) : int =
      Utils.compare_chain
        [ Note.compare a.this b.this; Option.compare compare a.next b.next ]
    ;;

    let is_empty : t -> bool = function
      | { this; next = None } -> true
      | _ -> false
    ;;

    let rec length : t -> int = function
      | { next = None; _ } -> 1
      | { next = Some next; _ } -> 1 + length next
    ;;

    let shorter (a : t) (b : t) : t =
      match Int.compare (length a) (length b) with -1 -> a | _ -> b
    ;;

    let rec exists (x : Note.t) : t -> bool = function
      | { this; next = None } -> Note.equal x this
      | { this; next = Some next } ->
        if Note.equal x this then true else exists x next
    ;;

    let rec exists_label (x : Label.t) : t -> bool = function
      | { this; next = None } -> Label.equal x this.label
      | { this; next = Some next } ->
        if Label.equal x this.label then true else exists_label x next
    ;;

    let rec append (x : Note.t) : t -> t = function
      | { this; next = None } -> { this; next = Some { this = x; next = None } }
      | { this; next = Some next } -> { this; next = Some (append x next) }
    ;;

    let rec last : t -> Note.t = function
      | { this; next = None } -> this
      | { next = Some next; _ } -> last next
    ;;

    exception CannotDropLastOfSingleton of t

    let rec drop_last : t -> t = function
      | { this; next = None } ->
        raise (CannotDropLastOfSingleton { this; next = None })
      | { this; next = Some { next = None; _ }; _ } -> { this; next = None }
      | { this; next = Some next } -> { this; next = Some (drop_last next) }
    ;;

    let rec to_string : t -> string = function
      | { this; next = None } -> Note.to_string this
      | { this; next = Some next } ->
        Printf.sprintf "%s; %s" (Note.to_string this) (to_string next)
    ;;
  end

  module type SAnnotations = sig
    module S : Set.S with type elt = Annotation.t

    type t = S.t

    val to_string : t -> string
  end

  module Annotations : SAnnotations = struct
    module S : Set.S with type elt = Annotation.t = Set.Make (Annotation)

    type t = S.t

    let to_string (xs : t) : string =
      S.to_list xs
      |> Utils.Strfy.list
           ~args:{ (Utils.Strfy.style_args ()) with name = Some "Annotations" }
           (Of Annotation.to_string)
    ;;
  end

  (* *)
  module type STransition = sig
    type t =
      { from : State.t
      ; goto : State.t
      ; label : Label.t
      ; annotation : Annotation.t option
      ; constructor_trees : CTrees.t
      }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val is_silent : t -> bool
    val annotation_is_empty : t -> bool
    val to_string : t -> string
  end

  module Transition : STransition = struct
    type t =
      { from : State.t
      ; goto : State.t
      ; label : Label.t
      ; annotation : Annotation.t option
      ; constructor_trees : CTrees.t
      }

    let equal (a : t) (b : t) : bool =
      State.equal a.from b.from
      && State.equal a.goto b.goto
      && Label.equal a.label b.label
      && Option.equal Annotation.equal a.annotation b.annotation
      && CTrees.equal a.constructor_trees b.constructor_trees
    ;;

    let compare (a : t) (b : t) : int =
      Utils.compare_chain
        [ State.compare a.from b.from
        ; State.compare a.goto b.goto
        ; Label.compare a.label b.label
        ; Option.compare Annotation.compare a.annotation b.annotation
        ; CTrees.compare a.constructor_trees b.constructor_trees
        ]
    ;;

    let is_silent (x : t) : bool = Label.is_silent x.label

    let annotation_is_empty : t -> bool = function
      | { annotation = None; _ } -> true
      | { annotation = Some annotation; _ } -> Annotation.is_empty annotation
    ;;

    let to_string (x : t) : string =
      Utils.Strfy.record
        [ "from", State.to_string x.from
        ; "goto", State.to_string x.from
        ; "label", Label.to_string x.label
        ; ( "annotation"
          , Utils.Strfy.option (Of Annotation.to_string) x.annotation )
        ; ( "constructor_trees"
          , Utils.Strfy.list
              (Of CTree.to_string)
              (CTrees.to_list x.constructor_trees) )
        ]
    ;;
  end

  module type STransitions = sig
    module S : Set.S with type elt = Transition.t

    type t = S.t

    val to_string : t -> string
  end

  module Transitions : STransitions = struct
    module S : Set.S with type elt = Transition.t = Set.Make (Transition)

    type t = S.t

    let to_string (xs : t) : string =
      S.to_list xs
      |> Utils.Strfy.list
           ~args:{ (Utils.Strfy.style_args ()) with name = Some "Transitions" }
           (Of Transition.to_string)
    ;;
  end

  (* *)
  module type SAction = sig
    type t =
      { label : Label.t
      ; annotation : Annotation.t option
      ; constructor_trees : CTrees.t
      }

    val wk_equal : t -> t -> bool
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val is_silent : t -> bool
    val annotation_is_empty : t -> bool
    val to_string : t -> string
  end

  module Action : SAction = struct
    type t =
      { label : Label.t
      ; annotation : Annotation.t option
      ; constructor_trees : CTrees.t
      }

    let wk_equal (a : t) (b : t) : bool = Label.equal a.label b.label

    let equal (a : t) (b : t) : bool =
      Label.equal a.label b.label
      && Option.equal Annotation.equal a.annotation b.annotation
      && CTrees.equal a.constructor_trees b.constructor_trees
    ;;

    let compare (a : t) (b : t) : int =
      Utils.compare_chain
        [ Label.compare a.label b.label
        ; Option.compare Annotation.compare a.annotation b.annotation
        ; CTrees.compare a.constructor_trees b.constructor_trees
        ]
    ;;

    let hash (x : t) : int = Label.hash x.label
    let is_silent (x : t) : bool = Label.is_silent x.label

    let annotation_is_empty : t -> bool = function
      | { annotation = None; _ } -> true
      | { annotation = Some annotation; _ } -> Annotation.is_empty annotation
    ;;

    let to_string (x : t) : string =
      Utils.Strfy.record
        [ "label", Label.to_string x.label
        ; ( "annotation"
          , Utils.Strfy.option (Of Annotation.to_string) x.annotation )
        ; ( "constructor_trees"
          , Utils.Strfy.list
              (Of CTree.to_string)
              (CTrees.to_list x.constructor_trees) )
        ]
    ;;
  end

  module type SActions = sig
    module S : Set.S with type elt = Action.t

    type t = S.t

    val labelled : t -> Label.t -> t
    val to_string : t -> string
  end

  module Actions : SActions = struct
    module S : Set.S with type elt = Action.t = Set.Make (Action)

    type t = S.t

    let labelled (xs : t) (y : Label.t) : t =
      S.filter (fun ({ label; _ } : Action.t) -> Label.equal label y) xs
    ;;

    let to_string (xs : t) : string =
      S.to_list xs
      |> Utils.Strfy.list
           ~args:{ (Utils.Strfy.style_args ()) with name = Some "Actions" }
           (Of Action.to_string)
    ;;
  end

  module type SActionMap = sig
    module M : Hashtbl.S with type key = Action.t

    type t = States.t M.t

    val update : t -> Action.t -> States.t -> unit
    val get_destinations : t -> States.t
    val to_string : t -> string
  end

  module ActionMap : SActionMap = struct
    module M : Hashtbl.S with type key = Action.t = Hashtbl.Make (Action)

    type t = States.t M.t

    let update (x : t) (action : Action.t) (states : States.t) : unit =
      if States.S.is_empty states
      then ()
      else (
        match M.find_opt x action with
        | None -> M.add x action states
        | Some old_states ->
          M.replace x action (States.S.union old_states states))
    ;;

    (** [get_destinations x f e] merges the values of [x] using [f], where [e] is some initial (i.e., "empty") collection of ['a].
    *)
    let get_destinations (x : t) : States.t =
      M.to_seq_values x |> Seq.fold_left States.S.union States.S.empty
    ;;

    let to_string (xs : t) : string =
      M.to_seq xs
      |> List.of_seq
      |> Utils.Strfy.list
           ~args:{ (Utils.Strfy.style_args ()) with name = Some "Actions" }
           (Of
              (fun (k, v) ->
                Utils.Strfy.record
                  [ "action", Action.to_string k; "->", States.to_string v ]))
    ;;
  end

  (* *)
  module type SEdge = sig
    type t =
      { from : State.t
      ; goto : State.t
      ; action : Action.t
      }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val is_silent : t -> bool
    val to_string : t -> string
  end

  module Edge : SEdge = struct
    type t =
      { from : State.t
      ; goto : State.t
      ; action : Action.t
      }

    let equal (a : t) (b : t) : bool =
      State.equal a.from b.from
      && State.equal a.goto b.goto
      && Action.equal a.action b.action
    ;;

    let compare (a : t) (b : t) : int =
      Utils.compare_chain
        [ State.compare a.from b.from
        ; State.compare a.goto b.goto
        ; Action.compare a.action b.action
        ]
    ;;

    let is_silent (x : t) : bool = Action.is_silent x.action

    let to_string (x : t) : string =
      Utils.Strfy.record
        [ "from", State.to_string x.from
        ; "goto", State.to_string x.from
        ; "action", Action.to_string x.action
        ]
    ;;
  end

  module type SEdges = sig
    module S : Set.S with type elt = Edge.t

    type t = S.t

    val labelled : t -> Label.t -> t
    val to_string : t -> string
  end

  module Edges : SEdges = struct
    module S : Set.S with type elt = Edge.t = Set.Make (Edge)

    type t = S.t

    let labelled (xs : t) (y : Label.t) : t =
      S.filter (fun ({ action; _ } : Edge.t) -> Label.equal action.label y) xs
    ;;

    let to_string (xs : t) : string =
      S.to_list xs
      |> Utils.Strfy.list
           ~args:{ (Utils.Strfy.style_args ()) with name = Some "Edges" }
           (Of Edge.to_string)
    ;;
  end

  module type SEdgeMap = sig
    module M : Hashtbl.S with type key = State.t

    type t = ActionMap.t M.t

    val update : t -> State.t -> Action.t -> States.t -> unit
    val get_destinations : t -> State.t -> States.t
    val get_actions : t -> State.t -> Actions.t
    val get_edges : t -> State.t -> Edges.t
    val to_string : t -> string
  end

  module EdgeMap : SEdgeMap = struct
    module M : Hashtbl.S with type key = State.t = Hashtbl.Make (State)

    type t = ActionMap.t M.t

    let update
          (x : t)
          (from : State.t)
          (action : Action.t)
          (destinations : States.t)
      : unit
      =
      match M.find_opt x from with
      | None ->
        [ action, destinations ]
        |> List.to_seq
        |> ActionMap.M.of_seq
        |> M.add x from
      | Some actions -> ActionMap.update actions action destinations
    ;;

    let get_destinations (x : t) (from : State.t) : States.t =
      ActionMap.get_destinations (M.find x from)
    ;;

    let get_actions (x : t) (from : State.t) : Actions.t =
      M.find x from |> ActionMap.M.to_seq_keys |> Actions.S.of_seq
    ;;

    let get_edges (x : t) (from : State.t) : Edges.t =
      ActionMap.M.fold
        (fun (action : Action.t) (v : States.t) (acc : Edges.t) : Edges.t ->
          States.S.fold
            (fun (goto : State.t) (acc : Edges.t) : Edges.t ->
              Edges.S.add { from; action; goto } acc)
            v
            acc)
        (M.find x from)
        Edges.S.empty
    ;;

    let to_string (xs : t) : string =
      M.to_seq xs
      |> List.of_seq
      |> Utils.Strfy.list
           ~args:{ (Utils.Strfy.style_args ()) with name = Some "Edges" }
           (Of
              (fun (k, v) ->
                Utils.Strfy.record
                  [ "from", State.to_string k; "->", ActionMap.to_string v ]))
    ;;
  end

  (* *)
  module type SParitition = sig
    module S : Set.S with type elt = States.t

    type t = S.t

    val get_reachable : t -> State.t -> EdgeMap.t -> t
    val to_string : t -> string
  end

  module Paritition : SParitition = struct
    module S : Set.S with type elt = States.t = Set.Make (States.S)

    type t = S.t

    let get_reachable (x : t) (from : State.t) (edges : EdgeMap.t) : t =
      let destinations : States.t = EdgeMap.get_destinations edges from in
      S.filter
        (fun (y : States.t) ->
          Bool.not (States.S.is_empty (States.S.inter y destinations)))
        x
    ;;

    let to_string (xs : t) : string =
      S.to_list xs
      |> Utils.Strfy.list
           ~args:{ (Utils.Strfy.style_args ()) with name = Some "Paritition" }
           (Of States.to_string)
    ;;
  end

  (* *)
  module type SInfo = sig
    type t =
      { meta : meta option
      ; weak_labels : Labels.t
      }

    and meta =
      { is_complete : bool
      ; is_merged : bool
      ; bounds : bounds
      ; lts : lts list
      }

    and bounds =
      | States of int
      | Transitions of int

    and lts =
      { enc : Enc.t
      ; constructors : Rocq_bindings.constructor list
      }

    val to_string : t -> string
  end

  module Info : SInfo = struct
    type t =
      { meta : meta option
      ; weak_labels : Labels.t
      }

    and meta =
      { is_complete : bool
      ; is_merged : bool
      ; bounds : bounds
      ; lts : lts list
      }

    and bounds =
      | States of int
      | Transitions of int

    and lts =
      { enc : Enc.t
      ; constructors : Rocq_bindings.constructor list
      }

    let to_string (x : t) : string =
      let f
            ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
            (y : meta)
        : string
        =
        Utils.Strfy.record
          ~args
          [ "is complete", Utils.Strfy.bool y.is_complete
          ; "is merged", Utils.Strfy.bool y.is_merged
          ; ( "bounds"
            , match y.bounds with
              | States i -> Printf.sprintf "States (%i)" i
              | Transitions i -> Printf.sprintf "Transitions (%i)" i )
          ; ( "lts"
            , Utils.Strfy.list
                (Of
                   (fun ({ enc; constructors } : lts) ->
                     Utils.Strfy.record
                       [ "enc", Enc.to_string enc
                       ; ( "constructors"
                         , Utils.Strfy.list
                             (Of Rocq_bindings.constructor_to_string)
                             constructors )
                       ]))
                y.lts )
          ]
      in
      Utils.Strfy.record
        [ "meta", Utils.Strfy.option (Args f) x.meta
        ; "weak labels", Labels.to_string x.weak_labels
        ]
    ;;
  end

  (* *)
  module type SLTS = sig
    type t =
      { init : State.t option
      ; terminals : States.t
      ; alphabet : Labels.t
      ; states : States.t
      ; transitions : Transitions.t
      ; info : Info.t
      }

    val to_string : t -> string
  end

  module LTS : SLTS = struct
    type t =
      { init : State.t option
      ; terminals : States.t
      ; alphabet : Labels.t
      ; states : States.t
      ; transitions : Transitions.t
      ; info : Info.t
      }

    let to_string (x : t) : string =
      Utils.Strfy.record
        [ "init", Utils.Strfy.option (Of State.to_string) x.init
        ; "terminals", States.to_string x.terminals
        ; "alphabet", Labels.to_string x.alphabet
        ; "states", States.to_string x.states
        ; "transitions", Transitions.to_string x.transitions
        ; "info", Info.to_string x.info
        ]
    ;;
  end

  (* *)
  module type SFSM = sig
    type t =
      { init : State.t option
      ; terminals : States.t
      ; alphabet : Labels.t
      ; states : States.t
      ; edges : EdgeMap.t
      ; info : Info.t
      }
  end

  module FSM : SFSM = struct
    type t =
      { init : State.t option
      ; terminals : States.t
      ; alphabet : Labels.t
      ; states : States.t
      ; edges : EdgeMap.t
      ; info : Info.t
      }

    let to_string (x : t) : string =
      Utils.Strfy.record
        [ "init", Utils.Strfy.option (Of State.to_string) x.init
        ; "terminals", States.to_string x.terminals
        ; "alphabet", Labels.to_string x.alphabet
        ; "states", States.to_string x.states
        ; "edges", EdgeMap.to_string x.edges
        ; "info", Info.to_string x.info
        ]
    ;;
  end

  (* *)
  module type SConvert = sig
    val transitions_to_edgemap : Transitions.t -> EdgeMap.t
    val lts_to_fsm : LTS.t -> FSM.t
  end

  module Convert : SConvert = struct
    (* let transitions_to_edges (xs:Transitions.t) : Edges.t = () *)

    let transitions_to_edgemap (xs : Transitions.t) : EdgeMap.t =
      let edges : EdgeMap.t = EdgeMap.M.create 0 in
      Transitions.S.iter
        (fun ({ from; goto; label; annotation; constructor_trees } :
               Transition.t) ->
          EdgeMap.update
            edges
            from
            { label; annotation; constructor_trees }
            (States.S.singleton goto))
        xs;
      edges
    ;;

    let lts_to_fsm (x : LTS.t) : FSM.t =
      { init = x.init
      ; terminals = x.terminals
      ; alphabet = x.alphabet
      ; states = x.states
      ; edges = transitions_to_edgemap x.transitions
      ; info = x.info
      }
    ;;
  end
end
