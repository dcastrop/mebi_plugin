module Make (Enc : Encoding.SEncoding) = struct
  module Tree = Enc_tree.Make (Enc)
  module Trees : Set.S with type elt = Tree.t = Set.Make (Tree)

  module State = struct
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

  module States = struct
    module S : Set.S with type elt = State.t = Set.Make (State)
    include S

    let add_to_opt (x : State.t) (ys : t option) : t =
      add x (Option.default empty ys)
    ;;

    exception StateHasNoOrigin of (State.t * t * t)

    (** [origin_of_state x a b] returns [-1] if [x] is only in [a], [1] if [x] is only in [b], [0] if [x] is in both [a] and [b], and raises [StateHasNoOrigin] otherwise.
    *)
    let origin_of_state (x : State.t) (a : t) (b : t) : int =
      match mem x a, mem x b with
      | true, true -> 0
      | true, false -> -1
      | false, true -> 1
      | false, false -> raise (StateHasNoOrigin (x, a, b))
    ;;

    (** [has_shared_origin a b c] returns [true] if any of the states in [a] are present in both [b] and [c], otherwise [false].
    *)
    let has_shared_origin (a : t) (b : t) (c : t) : bool =
      exists (fun (x : State.t) : bool -> Int.equal 0 (origin_of_state x b c)) a
    ;;

    let to_string (xs : t) : string =
      S.to_list xs
      |> Utils.Strfy.list
           ~args:{ (Utils.Strfy.style_args ()) with name = Some "States" }
           (Of State.to_string)
    ;;
  end

  module Label = struct
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

  module Labels = struct
    module S : Set.S with type elt = Label.t = Set.Make (Label)
    include S

    let to_string (xs : t) : string =
      S.to_list xs
      |> Utils.Strfy.list
           ~args:{ (Utils.Strfy.style_args ()) with name = Some "Labels" }
           (Of Label.to_string)
    ;;
  end

  module Note = struct
    type t =
      { from : State.t
      ; label : Label.t
      ; using : Trees.t
      ; goto : State.t
      }

    let equal (a : t) (b : t) : bool =
      State.equal a.from b.from
      && State.equal a.goto b.goto
      && Label.equal a.label b.label
      && Trees.equal a.using b.using
    ;;

    let compare (a : t) (b : t) : int =
      Utils.compare_chain
        [ State.compare a.from b.from
        ; State.compare a.goto b.goto
        ; Label.compare a.label b.label
        ; Trees.compare a.using b.using
        ]
    ;;

    (* *)
    let to_string (x : t) : string =
      Printf.sprintf
        "<State (%s) Goto (%s) Via (%s)>"
        (State.to_string x.from)
        (State.to_string x.goto)
        (Label.to_string x.label)
    ;;

    (* *)
    let is_silent (x : t) : bool = Label.is_silent x.label
  end

  module Annotation = struct
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

  module Annotations = struct
    module S : Set.S with type elt = Annotation.t = Set.Make (Annotation)
    include S

    let to_string (xs : t) : string =
      S.to_list xs
      |> Utils.Strfy.list
           ~args:{ (Utils.Strfy.style_args ()) with name = Some "Annotations" }
           (Of Annotation.to_string)
    ;;
  end

  module Transition = struct
    type t =
      { from : State.t
      ; goto : State.t
      ; label : Label.t
      ; annotation : Annotation.t option
      ; constructor_trees : Trees.t
      }

    let equal (a : t) (b : t) : bool =
      State.equal a.from b.from
      && State.equal a.goto b.goto
      && Label.equal a.label b.label
      && Option.equal Annotation.equal a.annotation b.annotation
      && Trees.equal a.constructor_trees b.constructor_trees
    ;;

    let compare (a : t) (b : t) : int =
      Utils.compare_chain
        [ State.compare a.from b.from
        ; State.compare a.goto b.goto
        ; Label.compare a.label b.label
        ; Option.compare Annotation.compare a.annotation b.annotation
        ; Trees.compare a.constructor_trees b.constructor_trees
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
              (Of Tree.to_string)
              (Trees.to_list x.constructor_trees) )
        ]
    ;;
  end

  module Transitions = struct
    module S : Set.S with type elt = Transition.t = Set.Make (Transition)
    include S

    let to_string (xs : t) : string =
      S.to_list xs
      |> Utils.Strfy.list
           ~args:{ (Utils.Strfy.style_args ()) with name = Some "Transitions" }
           (Of Transition.to_string)
    ;;
  end

  module Action = struct
    type t =
      { label : Label.t
      ; annotation : Annotation.t option
      ; constructor_trees : Trees.t
      }

    let wk_equal (a : t) (b : t) : bool = Label.equal a.label b.label

    let equal (a : t) (b : t) : bool =
      Label.equal a.label b.label
      && Option.equal Annotation.equal a.annotation b.annotation
      && Trees.equal a.constructor_trees b.constructor_trees
    ;;

    let compare (a : t) (b : t) : int =
      Utils.compare_chain
        [ Label.compare a.label b.label
        ; Option.compare Annotation.compare a.annotation b.annotation
        ; Trees.compare a.constructor_trees b.constructor_trees
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
              (Of Tree.to_string)
              (Trees.to_list x.constructor_trees) )
        ]
    ;;
  end

  module ActionPair = struct
    type t = Action.t * States.t

    let to_string ((a, b) : t) =
      Utils.Strfy.record
        [ "action", Action.to_string a; "destinations", States.to_string b ]
    ;;

    let compare ((a, x) : t) ((b, y) : t) : int =
      Utils.compare_chain [ Action.compare a b; States.compare x y ]
    ;;
  end

  module ActionPairs = struct
    module S = Set.Make (ActionPair)
    include S

    (** [merge_saturated_tuples a b] merges elements of [b] into [a], either by updating an element in [a] with additional annotation for a saturation tuple that describes the same action-destination, or in the case that the saturation tuple is not described within [a] by inserting it within [a].
    *)
    let rec merge_saturated_tuples (a : ActionPair.t list)
      : ActionPair.t list -> ActionPair.t list
      = function
      | [] -> a
      | h :: tl ->
        let (a : (Action.t * States.t) list) =
          match try_update_saturated_tuple h a with
          | None, a -> h :: a
          | Some updated, a -> updated :: a
        in
        merge_saturated_tuples a tl

    (** [try_update_saturated_tuple x a] returns [None, a] when [x] cannot be used to update a pre-existing tuple in [a], and [Some z, a'] where [z] is the updated tuple in [a] which has been removed in [a'].
    *)
    and try_update_saturated_tuple
          ((xaction, xdestinations) : ActionPair.t)
          (a : ActionPair.t list)
      : ActionPair.t option * ActionPair.t list
      =
      let f : Annotation.t option * Annotation.t option -> Annotation.t option =
        function
        | None, None -> None
        | None, y -> y
        | x, None -> x
        | Some x, Some y -> Some (Annotation.shorter x y)
      in
      List.fold_left
        (fun ((updated_opt, acc) :
               (Action.t * States.t) option * (Action.t * States.t) list)
          ((yaction, ydestinations) : Action.t * States.t) ->
          match updated_opt with
          | Some opt -> Some opt, (yaction, ydestinations) :: acc
          | None ->
            if
              Action.wk_equal xaction yaction
              && States.equal xdestinations ydestinations
            then (
              let annotation : Annotation.t option =
                f (yaction.annotation, xaction.annotation)
              in
              let zaction : Action.t =
                { label = yaction.label
                ; annotation
                ; constructor_trees =
                    Trees.union
                      yaction.constructor_trees
                      xaction.constructor_trees
                }
              in
              Some (zaction, ydestinations), acc)
            else None, (yaction, ydestinations) :: acc)
        (None, [])
        a
    ;;
  end

  module Actions = struct
    module S : Set.S with type elt = Action.t = Set.Make (Action)
    include S

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

  module ActionMap = struct
    module M : Hashtbl.S with type key = Action.t = Hashtbl.Make (Action)
    include M

    type t' = States.t t

    let update (x : t') (action : Action.t) (states : States.t) : unit =
      if States.is_empty states
      then ()
      else (
        match find_opt x action with
        | None -> add x action states
        | Some old_states -> replace x action (States.union old_states states))
    ;;

    (** [get_destinations x f e] merges the values of [x] using [f], where [e] is some initial (i.e., "empty") collection of ['a].
    *)
    let get_destinations (x : t') : States.t =
      to_seq_values x |> Seq.fold_left States.union States.empty
    ;;

    let reduce_by_label (x : t') (label : Label.t) : t' =
      let y : t' = copy x in
      filter_map_inplace
        (fun (k : Action.t) (vs : States.t) ->
          if Label.equal k.label label then Some vs else None)
        y;
      y
    ;;

    let to_actions (x : t') : Actions.t = to_seq_keys x |> Actions.of_seq

    let to_actionpairs (x : t') : ActionPairs.t =
      fold
        (fun (k : Action.t)
          (vs : States.t)
          : (ActionPairs.t -> ActionPairs.t) -> ActionPairs.add (k, vs))
        x
        ActionPairs.empty
    ;;

    let of_actionpairs (xs : ActionPairs.t) : t' =
      let y : t' = create 0 in
      ActionPairs.iter (fun ((k, vs) : ActionPair.t) -> update y k vs) xs;
      y
    ;;

    let merge (a : t') (b : t') : t' =
      ActionPairs.union (to_actionpairs a) (to_actionpairs b) |> of_actionpairs
    ;;

    let to_string (xs : t') : string =
      to_seq xs
      |> List.of_seq
      |> Utils.Strfy.list
           ~args:{ (Utils.Strfy.style_args ()) with name = Some "Actions" }
           (Of
              (fun (k, v) ->
                Utils.Strfy.record
                  [ "action", Action.to_string k; "->", States.to_string v ]))
    ;;
  end

  module Edge = struct
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

  module Edges = struct
    module S : Set.S with type elt = Edge.t = Set.Make (Edge)
    include S

    let labelled (xs : t) (y : Label.t) : t =
      filter (fun ({ action; _ } : Edge.t) -> Label.equal action.label y) xs
    ;;

    let to_string (xs : t) : string =
      to_list xs
      |> Utils.Strfy.list
           ~args:{ (Utils.Strfy.style_args ()) with name = Some "Edges" }
           (Of Edge.to_string)
    ;;
  end

  module EdgeMap = struct
    module M : Hashtbl.S with type key = State.t = Hashtbl.Make (State)
    include M

    type t' = ActionMap.t' t

    let update
          (x : t')
          (from : State.t)
          (action : Action.t)
          (destinations : States.t)
      : unit
      =
      match find_opt x from with
      | None ->
        [ action, destinations ]
        |> List.to_seq
        |> ActionMap.of_seq
        |> add x from
      | Some actions -> ActionMap.update actions action destinations
    ;;

    let get_destinations (x : t') (from : State.t) : States.t =
      ActionMap.get_destinations (find x from)
    ;;

    let get_actions (x : t') (from : State.t) : Actions.t =
      find x from |> ActionMap.to_seq_keys |> Actions.of_seq
    ;;

    let reduce_by_label (x : t') (label : Label.t) : t' =
      let y : t' = copy x in
      filter_map_inplace
        (fun (k : State.t) (vs : ActionMap.t') ->
          let vs' : ActionMap.t' = ActionMap.reduce_by_label vs label in
          if ActionMap.length vs' > 0 then Some vs' else None)
        y;
      y
    ;;

    let get_edges (x : t') (from : State.t) : Edges.t =
      ActionMap.fold
        (fun (action : Action.t) (v : States.t) (acc : Edges.t) : Edges.t ->
          States.fold
            (fun (goto : State.t) (acc : Edges.t) : Edges.t ->
              Edges.add { from; action; goto } acc)
            v
            acc)
        (find x from)
        Edges.empty
    ;;

    let to_edges (x : t') : Edges.t =
      fold
        (fun (from : State.t) (vs : ActionMap.t') : (Edges.t -> Edges.t) ->
          ActionMap.to_actionpairs vs
          |> ActionPairs.fold
               (fun
                   ((action, destinations) : ActionPair.t)
                    : (Edges.t -> Edges.t)
                  ->
               States.fold
                 (fun (goto : State.t) : (Edges.t -> Edges.t) ->
                   Edges.add { from; goto; action })
                 destinations))
        x
        Edges.empty
    ;;

    let of_edges (xs : Edges.t) : t' =
      let ys : t' = create 0 in
      Edges.iter
        (fun ({ from; goto; action } : Edge.t) ->
          update ys from action (States.singleton goto))
        xs;
      ys
    ;;

    let merge (a : t') (b : t') : t' =
      let c : t' = copy a in
      iter
        (fun (k : State.t) (vs : ActionMap.t') ->
          match find_opt c k with
          | Some actions -> ActionMap.merge actions vs |> replace c k
          | None -> add c k vs)
        b;
      c
    ;;

    let to_string (xs : t') : string =
      to_seq xs
      |> List.of_seq
      |> Utils.Strfy.list
           ~args:{ (Utils.Strfy.style_args ()) with name = Some "Edges" }
           (Of
              (fun (k, v) ->
                Utils.Strfy.record
                  [ "from", State.to_string k; "->", ActionMap.to_string v ]))
    ;;
  end

  module Partition = struct
    module S : Set.S with type elt = States.t = Set.Make (States)
    include S

    let get_reachable (x : t) (from : State.t) (edges : EdgeMap.t') : t =
      let destinations : States.t = EdgeMap.get_destinations edges from in
      filter
        (fun (y : States.t) ->
          Bool.not (States.is_empty (States.inter y destinations)))
        x
    ;;

    let to_string (xs : t) : string =
      to_list xs
      |> Utils.Strfy.list
           ~args:{ (Utils.Strfy.style_args ()) with name = Some "Partition" }
           (Of States.to_string)
    ;;
  end

  module Info = struct
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

    (** [merge a b] updates [a] with additional information from [b] *)
    let merge (a : t) (b : t) : t =
      let meta : meta option =
        Option.cata
          (fun (a : meta) : meta option ->
            Some
              { a with
                lts =
                  Option.cata
                    (fun (b : meta) : lts list ->
                      List.fold_left
                        (fun (acc : lts list) (x : lts) ->
                          if
                            List.exists
                              (fun (y : lts) -> Enc.equal x.enc y.enc)
                              acc
                          then acc
                          else x :: acc)
                        a.lts
                        b.lts)
                    a.lts
                    b.meta
              })
          b.meta
          a.meta
      in
      { meta; weak_labels = Labels.union a.weak_labels b.weak_labels }
    ;;

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

  module LTS = struct
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

  module FSM = struct
    type t =
      { init : State.t option
      ; terminals : States.t
      ; alphabet : Labels.t
      ; states : States.t
      ; edges : EdgeMap.t'
      ; info : Info.t
      }

    let merge (a : t) (b : t) : t =
      let init : State.t option = None in
      let terminals : States.t = States.union a.terminals b.terminals in
      let alphabet : Labels.t = Labels.union a.alphabet b.alphabet in
      let states : States.t = States.union a.states b.states in
      let edges : EdgeMap.t' = EdgeMap.merge a.edges b.edges in
      let info : Info.t = Info.merge a.info b.info in
      { init; terminals; alphabet; states; edges; info }
    ;;

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

  module Convert = struct
    let transitions_to_edgemap (xs : Transitions.t) : EdgeMap.t' =
      let edges : EdgeMap.t' = EdgeMap.create 0 in
      Transitions.iter
        (fun ({ from; goto; label; annotation; constructor_trees } :
               Transition.t) ->
          EdgeMap.update
            edges
            from
            { label; annotation; constructor_trees }
            (States.singleton goto))
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

  module Saturate = struct
    type data =
      { named : Action.t option
      ; notes : wip list
      ; visited : States.t
      ; old_edges : EdgeMap.t'
      }

    and wip =
      { from : State.t
      ; via : Label.t
      ; trees : Trees.t
      }

    let wip (from : State.t) (action : Action.t) : wip =
      { from; via = action.label; trees = action.constructor_trees }
    ;;

    let initial_data (old_edges : EdgeMap.t') : data =
      { named = None; notes = []; visited = States.empty; old_edges }
    ;;

    (****************************************************************************)

    (** returns a copy of [d] with the updated name *)
    let update_named (x : Action.t) (d : data) : data =
      let f (x : Action.t) (d : data) : Action.t option =
        Option.cata
          (fun y -> Some y)
          (if Action.is_silent x then None else Some x)
          d.named
      in
      { d with named = f x d }
    ;;

    (** returns a copy of [d] with the updated notes *)
    let update_notes (from : State.t) (action : Action.t) (d : data) : data =
      let f (from : State.t) (action : Action.t) (d : data) : wip list =
        wip from action :: d.notes
      in
      { d with notes = f from action d }
    ;;

    (** returns a copy of [d] with the updated visited *)
    let update_visited (x : State.t) (d : data) : data =
      let f (x : State.t) (d : data) : States.t = States.add x d.visited in
      { d with visited = f x d }
    ;;

    (****************************************************************************)

    let already_visited (x : State.t) (d : data) : bool = States.mem x d.visited

    let skip_action (x : Action.t) (d : data) : bool =
      if Action.is_silent x then false else Option.has_some d.named
    ;;

    let get_old_actions (from : State.t) (d : data) : ActionMap.t' option =
      EdgeMap.find_opt d.old_edges from
    ;;

    (****************************************************************************)

    exception Model_Saturate_WIP_IsEmptyList of unit

    let wip_to_annotation (goto : State.t) (xs : wip list) : Annotation.t =
      let rec f : wip list -> Annotation.t = function
        | [] -> raise (Model_Saturate_WIP_IsEmptyList ())
        | { from; via; trees } :: [] ->
          { this = { from; label = via; using = trees; goto }; next = None }
        | { from; via; trees } :: h :: tl ->
          let { from = goto; via = via2; trees = tree2 } = h in
          { this = { from; label = via; using = trees; goto }
          ; next = Some (f (h :: tl))
          }
      in
      f (List.rev xs)
    ;;

    (****************************************************************************)

    exception Model_Saturate_WIP_HadNoNamedActions of wip list
    exception Model_Saturate_WIP_HadMultipleNamedActions of wip list

    let validate_wips (xs : wip list) : unit =
      match
        List.filter
          (fun ({ via; _ } : wip) -> Label.is_silent via |> Bool.not)
          xs
      with
      | [] -> raise (Model_Saturate_WIP_HadNoNamedActions xs)
      | _ :: [] -> ()
      | _ :: _ -> raise (Model_Saturate_WIP_HadMultipleNamedActions xs)
    ;;

    (** returns all of the possible actions after the named action *)
    let extrapolate_annotations (x : Annotation.t) : Annotations.t =
      (* NOTE: skip pre-named action *)
      let rec skip ({ this; next } : Annotation.t) : Annotations.t =
        let xs =
          Option.cata
            (if Note.is_silent this then skip else get)
            Annotations.empty
            next
          |> Annotations.map (fun (y : Annotation.t) -> { this; next = Some y })
        in
        (* NOTE: don't forget to add this action if named *)
        if Note.is_silent this
        then xs
        else Annotations.add { this; next = None } xs
      (* NOTE: get every annotation from named action onwards *)
      and get : Annotation.t -> Annotations.t = function
        | { this; next = None } -> Annotations.singleton { this; next = None }
        | { this; next = Some next } ->
          get next
          |> Annotations.map (fun (y : Annotation.t) -> { this; next = Some y })
          |> Annotations.add { this; next = None }
      in
      Annotations.add x (skip x)
    ;;

    (****************************************************************************)

    (** [stop] *)
    let stop (d : data) (goto : State.t) (acc : ActionPairs.t) : ActionPairs.t =
      match d.named with
      | None -> acc
      | Some named ->
        let () = validate_wips d.notes in
        wip_to_annotation goto d.notes
        |> extrapolate_annotations
        |> Annotations.to_list
        |> List.map (fun (x : Annotation.t) : ActionPair.t ->
          let y : Action.t =
            { label = named.label
            ; annotation = Some x
            ; constructor_trees = Trees.empty
            }
          in
          y, States.singleton (Annotation.last x).goto)
        |> List.fold_left
             (fun (acc : ActionPairs.t) ((a, s) : ActionPair.t) ->
               (* NOTE: merge destinations of equal actions *)
               let matching =
                 ActionPairs.filter
                   (fun ((b, t) : ActionPair.t) -> Action.equal a b)
                   acc
               in
               if ActionPairs.is_empty matching
               then ActionPairs.add (a, s) acc
               else (
                 (* NOTE: update states of each matching *)
                 let acc = ActionPairs.diff acc matching in
                 matching
                 |> ActionPairs.to_list
                 |> List.map (fun (_, t) -> a, States.union s t)
                 |> ActionPairs.of_list
                 |> ActionPairs.union acc))
             acc
    ;;

    (****************************************************************************)

    (** [check_from] explores the outgoing actions of state [from], which is some destination of another action.
    *)
    let rec check_from (d : data) (from : State.t) (acc : ActionPairs.t)
      : ActionPairs.t
      =
      if already_visited from d
      then stop d from acc
      else (
        let d : data = update_visited from d in
        match get_old_actions from d with
        | None -> stop d from acc
        | Some old_actions -> check_actions d from old_actions acc)

    and check_actions (d : data) (from : State.t) (xs : ActionMap.t')
      : ActionPairs.t -> ActionPairs.t
      =
      ActionMap.fold
        (fun (x : Action.t) (ys : States.t) (acc : ActionPairs.t) ->
          if skip_action x d
          then stop d from acc
          else (
            let d : data (* NOTE: copy [d] *) = update_notes from x d in
            let d : data = update_named x d in
            check_destinations d from ys acc))
        xs

    and check_destinations (d : data) (from : State.t) (xs : States.t)
      : ActionPairs.t -> ActionPairs.t
      =
      States.fold (check_from d) xs
    ;;

    (****************************************************************************)

    (** [edge_action_destinations] returns a list of saturated actions tupled with their respective destinations, which is the reflexive-transitive closure of visible actions that may weakly be performed from each of [the_destinations].
        edge -> edge_actions -> edge_action_destinations -> ( ... )
        @param ys
          is the set of destination [States.t] reachable from state [from] via actions that have already been recorded in [d.notes] as a [wip].
    *)
    let edge_action_destinations (d : data) (from : State.t) (ys : States.t)
      : ActionPairs.t
      =
      States.fold
        (fun (y : State.t) (acc : ActionPairs.t) ->
          check_from d y ActionPairs.empty)
        ys
        ActionPairs.empty
    ;;

    (** [edge_actions] returns a list of saturated actions tupled with their respective destinations, obtained from [edge_action_destinations] which explores the reflexive-transitive closure
        edge -> edge_actions -> edge_action_destinations -> ( ... ) *)
    let edge_actions
          (from : State.t)
          (old_actions : ActionMap.t')
          (old_edges : EdgeMap.t')
      : ActionPairs.t
      =
      ActionMap.fold
        (fun (x : Action.t) (ys : States.t) (acc : ActionPair.t list) ->
          let d : data =
            initial_data old_edges |> update_named x |> update_notes from x
          in
          edge_action_destinations d from ys
          |> ActionPairs.to_list
          |> ActionPairs.merge_saturated_tuples acc)
        old_actions
        []
      |> ActionPairs.of_list
    ;;

    (** [edge] updates [new_actions] with actions saturated by [edge_actions]
        edge -> edge_actions -> edge_action_destinations -> ( ... ) *)
    let edge
          (new_actions : ActionMap.t')
          (from : State.t)
          (old_actions : ActionMap.t')
          (old_edges : EdgeMap.t')
      : unit
      =
      edge_actions from old_actions old_edges
      |> ActionPairs.iter
           (fun ((saturated_action, destinations) : Action.t * States.t) ->
           ActionMap.update new_actions saturated_action destinations)
    ;;

    (** [] *)
    let edges (labels : Labels.t) (states : States.t) (old_edges : EdgeMap.t')
      : EdgeMap.t'
      =
      let new_edges : EdgeMap.t' = EdgeMap.create 0 in
      EdgeMap.iter
        (fun (from : State.t) (old_actions : ActionMap.t') ->
          (* NOTE: populate [new_actions] with saturated [old_actions] *)
          let new_actions : ActionMap.t' = ActionMap.create 0 in
          let () = edge new_actions from old_actions old_edges in
          EdgeMap.replace new_edges from new_actions)
        old_edges;
      new_edges
    ;;

    let fsm ?(only_if_weak : bool option = None) (x : FSM.t) : FSM.t =
      match only_if_weak with
      | Some false -> x
      | _ -> { x with edges = edges x.alphabet x.states (EdgeMap.copy x.edges) }
    ;;
  end

  module Minimize = struct
    type t =
      { fsm : FSM.t
      ; pi : Partition.t
      }

    exception Split_OnlyReturnedOneBlock_ButNeqBlock of (States.t * States.t)

    let ensure_equal (a : States.t) (b : States.t) : unit =
      try assert (States.equal a b) with
      | Assert_failure _ ->
        raise (Split_OnlyReturnedOneBlock_ButNeqBlock (a, b))
    ;;

    exception CannotSplitEmptyBlock of unit

    let ensure_nonempty (a : States.t) : unit =
      try assert (States.is_empty a |> Bool.not) with
      | Assert_failure _ -> raise (CannotSplitEmptyBlock ())
    ;;

    let split_block
          (pi : Partition.t)
          (s : State.t)
          (edges : EdgeMap.t')
          (block : States.t)
      : States.t * States.t option
      =
      ensure_nonempty block;
      let reachable_from_s : Partition.t = Partition.get_reachable pi s edges in
      States.fold
        (fun (t : State.t) ((b1, b2) : States.t * States.t option) ->
          if State.equal s t
          then States.add s b1, b2
          else (
            let reachable_from_t : Partition.t =
              Partition.get_reachable pi t edges
            in
            (* NOTE: split if [s] and [t] can reach different blocks *)
            if Partition.equal reachable_from_s reachable_from_t
            then States.add t b1, b2
            else b1, Some (States.add_to_opt t b2)))
        block
        (States.empty, None)
    ;;

    let for_each_label
          (pi : Partition.t ref)
          (changed : bool ref)
          (edges : EdgeMap.t')
          (block : States.t ref)
          (label : Label.t)
      : unit
      =
      let edges : EdgeMap.t' = EdgeMap.reduce_by_label edges label in
      (* NOTE: select some state [s] from [block] *)
      let s : State.t = States.min_elt !block in
      match split_block !pi s edges !block with
      | a, None -> ensure_equal a !block
      | a, Some b ->
        pi := Partition.remove !block !pi |> Partition.add a |> Partition.add b;
        block := a;
        changed := true
    ;;

    let for_each_block
          (pi : Partition.t ref)
          (changed : bool ref)
          (alphabet : Labels.t)
          (edges : EdgeMap.t')
          (block : States.t)
      : unit
      =
      Labels.iter (for_each_label pi changed edges (ref block)) alphabet
    ;;

    let partition_states (fsm : FSM.t) : Partition.t =
      let pi : Partition.t ref = ref (Partition.singleton fsm.states) in
      let changed : bool ref = ref true in
      while !changed do
        changed := false;
        Partition.iter (for_each_block pi changed fsm.alphabet fsm.edges) !pi
      done;
      !pi
    ;;

    let fsm ?(weak : bool = false) (fsm : FSM.t) : t =
      { fsm
      ; pi = Saturate.fsm ~only_if_weak:(Some weak) fsm |> partition_states
      }
    ;;
  end

  module Bisimilar = struct
    type t =
      { fsm_a : fsm_pair
      ; fsm_b : fsm_pair
      ; merged : FSM.t
      ; result : result
      }

    and result =
      { bisim_states : Partition.t
      ; non_bisim_states : Partition.t
      }

    and fsm_pair =
      { original : FSM.t
      ; saturated : FSM.t
      }

    let fsm_pair ?(weak : bool = false) (original : FSM.t) : fsm_pair =
      { original; saturated = Saturate.fsm ~only_if_weak:(Some weak) original }
    ;;

    let the_cached_result : t option ref = ref None
    let set_the_result (x : t) : unit = the_cached_result := Some x

    exception NoCachedResult of unit

    let get_the_result () : t =
      match !the_cached_result with
      | None -> raise (NoCachedResult ())
      | Some x -> x
    ;;

    let split (pi : Partition.t) (a : States.t) (b : States.t) : result =
      let bisim_states, non_bisim_states =
        Partition.fold
          (fun (x : States.t) (bisim_states, non_bisim_states) ->
            if States.has_shared_origin x a b
            then Partition.add x bisim_states, non_bisim_states
            else bisim_states, Partition.add x non_bisim_states)
          pi
          (Partition.empty, Partition.empty)
      in
      { bisim_states; non_bisim_states }
    ;;

    let fsm ?(weak : bool = false) (a : FSM.t) (b : FSM.t) : t =
      let fsm_a : fsm_pair = fsm_pair ~weak a in
      let fsm_b : fsm_pair = fsm_pair ~weak b in
      let merged : FSM.t = FSM.merge fsm_a.saturated fsm_b.saturated in
      let pi : Partition.t = (Minimize.fsm merged).pi in
      let result = split pi fsm_a.original.states fsm_b.original.states in
      { fsm_a; fsm_b; merged; result }
    ;;
  end
end
