(* No rocq-runtime and no rocq_tools: the model is plain OCaml over an
   abstract element type, and must stay linkable from test/ without a Rocq
   runtime. Info.Make takes its constructor-bindings parameter as a Json.S
   rather than a Constructor_bindings.S for exactly this reason.

   All of the model's mutually-referential components live here as nested
   modules inside one functor body, rather than as ~17 separate files each
   its own functor. Nested modules see each other directly, so none of the
   type-sharing constraints that used to relate one component's functor
   parameters to another's output are needed here -- see
   notes/3-collapse-model-component-cluster.md for the motivation. *)

(* --- per-component signatures, one per component, each named after its
   original file. Each is a near-verbatim copy of that file's [module type S], renamed to avoid clashing with its neighbours in this single
   namespace. *)

module type State_sig = sig
  type base
  type t = { base : base }

  include Json.S with type k = t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end

module type States_sig = sig
  include Set.S
  include Json.S with type k = t

  val add_to_opt : elt -> t option -> t

  exception StateHasNoOrigin of (elt * t * t)

  val origin_of_state : elt -> t -> t -> int
  val has_shared_origin : t -> t -> t -> bool
end

module type Label_sig = sig
  type base

  type t =
    { base : base
    ; is_silent : bool option
    }

  include Json.S with type k = t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val is_silent : t -> bool
end

module type Labels_sig = sig
  include Set.S
  include Json.S with type k = t

  val non_silent : t -> t
end

module type Annotation_note_sig = sig
  type state
  type label
  type trees

  type t =
    { from : state
    ; label : label
    ; using : trees
    ; goto : state
    }

  include Json.S with type k = t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_silent : t -> bool
  val has_label : label -> t -> bool
end

module type Annotation_sig = sig
  type label
  type note

  type t =
    { this : note
    ; next : t option
    }

  include Json.S with type k = t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_empty : t -> bool

  exception AnnotationIsNone

  val opt_is_empty : ?fail_if_none:bool -> t option -> bool
  val length : t -> int
  val opt_length : ?fail_if_none:bool -> t option -> int
  val shorter : t -> t -> t
  val exists : note -> t -> bool
  val exists_label : label -> t -> bool
  val append : note -> t -> t
  val last : t -> note

  exception CannotDropLastOfSingleton of t

  val drop_last : t -> t
end

module type Annotations_sig = sig
  include Set.S
  include Json.S with type k = t

  val extrapolate : elt -> t
end

module type Transition_sig = sig
  type state
  type label
  type tree
  type annotation

  type t =
    { from : state
    ; goto : state
    ; label : label
    ; tree : tree option
    ; annotation : annotation option
    }

  include Json.S with type k = t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_silent : t -> bool
end

module type Transitions_sig = sig
  type labels

  include Set.S
  include Json.S with type k = t

  val labels : t -> labels
end

module type Action_sig = sig
  type label
  type annotation
  type trees

  type t =
    { label : label
    ; annotation : annotation option
    ; trees : trees
    }

  include Json.S with type k = t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val wk_equal : t -> t -> bool
  val is_silent : t -> bool
  val is_labelled : label -> t -> bool
  val shorter_annotation : t -> t -> t
end

module type Actions_sig = sig
  type label
  type labels

  include Set.S
  include Json.S with type k = t

  val labelled : t -> label -> t
  val labels : t -> labels
end

module type Actionpair_sig = sig
  type action
  type states
  type t = action * states

  include Json.S with type k = t

  val compare : t -> t -> int
  val shorter_annotation : t -> t -> t
  val try_update : t -> t list -> t option * t list
  val merge_lists : t list -> t list -> t list
end

module type Actionpairs_sig = sig
  type states

  include Set.S
  include Json.S with type k = t

  val destinations : t -> states

  exception IsEmpty

  val shortest_annotation : t -> elt
  val merge_list : t -> elt list -> t
end

module type Actionmap_sig = sig
  type label
  type action
  type actions
  type states
  type actionpairs

  include Hashtbl.S with type key = action

  type t' = states t

  include Json.S with type k = t'

  val size : t' -> int
  val update : t' -> action -> states -> unit
  val destinations : t' -> states
  val reduce_by_label : t' -> label -> t'
  val to_actions : t' -> actions
  val to_actionpairs : t' -> actionpairs
  val of_actionpairs : actionpairs -> t'
  val merge : t' -> t' -> t'
end

module type Edge_sig = sig
  type state
  type label
  type action

  type t =
    { from : state
    ; goto : state
    ; action : action
    }

  include Json.S with type k = t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_silent : t -> bool
  val is_labelled : label -> t -> bool
end

module type Edges_sig = sig
  type label

  include Set.S
  include Json.S with type k = t

  val labelled : t -> label -> t
end

module type Edgemap_sig = sig
  type state
  type states
  type label
  type transitions
  type action
  type actions
  type actionmap
  type edges

  include Hashtbl.S with type key = state

  type t' = actionmap t

  include Json.S with type k = t'

  val size : t' -> int
  val update : t' -> state -> action -> states -> unit
  val destinations : t' -> state -> states
  val get_actions : t' -> state -> actions
  val reduce_by_label : t' -> label -> t'
  val get_edges : t' -> state -> edges
  val to_edges : t' -> edges
  val of_edges : edges -> t'
  val of_transitions : transitions -> t'
  val merge : t' -> t' -> t'
end

module type State_partition_sig = sig
  type state
  type label
  type edgemap

  include Set.S
  include Json.S with type k = t

  val get_bisimilar : state -> t -> elt
  val filter_reachable : elt -> t -> t
  val reachable : state -> edgemap -> t -> t
  val reachable_by_label : state -> label -> edgemap -> t -> t
end

module type Info_sig = sig
  type base
  type constructorbindings
  type labels

  module Meta : sig
    module Bounds : sig
      type t =
        | States of int
        | Transitions of int
        | Merged of t * t

      include Json.S with type k = t
    end

    module RocqLTS : sig
      type t =
        { base : base
        ; constructors : constructorbindings list
        }

      include Json.S with type k = t
    end

    type t =
      { is_complete : bool
      ; is_merged : bool
      ; bounds : Bounds.t
      ; lts : RocqLTS.t list
      }

    include Json.S with type k = t

    val merge : t -> t -> t
    val merge_opt : t option -> t option -> t option
  end

  type t =
    { meta : Meta.t option
    ; weak_labels : labels
    ; nums : nums option
    }

  and nums =
    { states : int
    ; labels : int
    ; edges : int
    }

  include Json.S with type k = t

  val merge : ?nums:nums option -> t -> t -> t
end

(* --- the components cluster itself. *)

module type S = sig
  type base
  type tree
  type trees
  type constructorbindings

  (* Each element type is grouped with its own Set/Map/Pair, e.g. [State.Set]
     where the old flat design had a separate [States] module. [EdgeMap] and
     [Partition] stay standalone: [EdgeMap] and [Action.Map] are mutually
     dependent (each stores the other's [t]/[t'] as a value), which cannot be
     expressed if either is nested inside its own key type's module -- doing
     so would require [State] to be declared both before [Note]/[Transition]/
     [Edge] (which need its bare type) and after [Action]/[Transition]/[Edge]
     (which [State.Map] would need), an ordering cycle ordinary (non-[rec])
     module signatures cannot express. *)

  module State : sig
    include State_sig with type base = base
    module Set : States_sig with type elt = t
  end

  module Label : sig
    include Label_sig with type base = base
    module Set : Labels_sig with type elt = t
  end

  module Note :
    Annotation_note_sig
    with type state = State.t
     and type label = Label.t
     and type trees = trees

  module Annotation : sig
    include Annotation_sig with type label = Label.t and type note = Note.t
    module Set : Annotations_sig with type elt = t
  end

  module Transition : sig
    include
      Transition_sig
      with type state = State.t
       and type label = Label.t
       and type tree = tree
       and type annotation = Annotation.t

    module Set : Transitions_sig with type elt = t and type labels = Label.Set.t
  end

  module Action : sig
    include
      Action_sig
      with type label = Label.t
       and type annotation = Annotation.t
       and type trees = trees

    module Set :
      Actions_sig
      with type elt = t
       and type label = Label.t
       and type labels = Label.Set.t

    module Pair : sig
      include Actionpair_sig with type action = t and type states = State.Set.t

      module Set :
        Actionpairs_sig with type states = State.Set.t and type elt = t
    end

    module Map :
      Actionmap_sig
      with type label = Label.t
       and type action = t
       and type actions = Set.t
       and type states = State.Set.t
       and type actionpairs = Pair.Set.t
  end

  module Edge : sig
    include
      Edge_sig
      with type state = State.t
       and type label = Label.t
       and type action = Action.t

    module Set : Edges_sig with type elt = t and type label = Label.t
  end

  module EdgeMap :
    Edgemap_sig
    with type state = State.t
     and type states = State.Set.t
     and type label = Label.t
     and type transitions = Transition.Set.t
     and type action = Action.t
     and type actions = Action.Set.t
     and type actionmap = Action.Map.t'
     and type edges = Edge.Set.t

  module Partition :
    State_partition_sig
    with type elt = State.Set.t
     and type state = State.t
     and type label = Label.t
     and type edgemap = EdgeMap.t'

  module Info :
    Info_sig
    with type base = base
     and type constructorbindings = constructorbindings
     and type labels = Label.Set.t
end

module Make (Base : Base_term.S) (ConstructorBindings : Json.S) :
  S
  with type base = Base.t
   and type tree = Base.Tree.t
   and type trees = Base.Trees.t
   and type constructorbindings = ConstructorBindings.k = struct
  type base = Base.t
  type tree = Base.Tree.t
  type trees = Base.Trees.t
  type constructorbindings = ConstructorBindings.k

  (* Wrapped in its own module so the flat, mutually-referential component
     bodies below (unchanged from the pre-nesting design) can be regrouped
     under State/Label/Annotation/Transition/Action/Edge afterwards without
     a "multiple definition of module X" clash -- accessed here via
     [Impl.State], not the bare name [State]. *)
  module Impl = struct
    module State = struct
      type base = Base.t
      type t = { base : base }

      module X = struct
        type nonrec t = t

        let name = "State"
        let json ?as_elt (x : t) : Yojson.t = Base.json ~as_elt:true x.base
        let equal a b = Base.equal a.base b.base
        let compare a b = Base.compare a.base b.base
      end

      include Thing.Make (X)

      let hash x = Base.hash x.base
    end

    module States = struct
      include
        Thing.Set
          (State)
          (struct
            let name = "States"
          end)

      let add_to_opt (x : State.t) (ys : t option) : t =
        add x (Stdlib.Option.value ys ~default:empty)
      ;;

      exception StateHasNoOrigin of (State.t * t * t)

      let origin_of_state (x : State.t) (a : t) (b : t) : int =
        match mem x a, mem x b with
        | true, true -> 0
        | true, false -> -1
        | false, true -> 1
        | false, false -> raise (StateHasNoOrigin (x, a, b))
      ;;

      let has_shared_origin (a : t) (b : t) (c : t) : bool =
        let f (i : int) (x : State.t) : bool =
          match origin_of_state x b c with 0 -> true | j -> Int.equal i j
        in
        exists (f (-1)) a && exists (f 1) a
      ;;
    end

    module Label = struct
      type base = Base.t

      type t =
        { base : base
        ; is_silent : bool option
        }

      module X = struct
        type nonrec t = t

        let name = "Label"

        let json ?as_elt (x : t) : Yojson.t =
          `Assoc
            [ "base", Base.json ~as_elt:true x.base
            ; ( "is_silent"
              , Json.option ~as_elt:true (fun ?as_elt x -> `Bool x) x.is_silent
              )
            ]
        ;;

        let equal (a : t) (b : t) : bool = Base.equal a.base b.base

        let compare (a : t) (b : t) : int =
          Utils.compare_chain
            [ Base.compare a.base b.base
            ; Stdlib.Option.fold
                ~none:0
                ~some:(fun (a : bool) ->
                  Stdlib.Option.fold ~none:0 ~some:(Bool.compare a) b.is_silent)
                a.is_silent
            ]
        ;;
      end

      include Thing.Make (X)

      let hash (x : t) : int = Base.hash x.base

      let is_silent (x : t) : bool =
        Stdlib.Option.value x.is_silent ~default:false
      ;;
    end

    module Labels = struct
      include
        Thing.Set
          (Label)
          (struct
            let name = "Labels"
          end)

      let non_silent (xs : t) : t =
        filter (fun (x : Label.t) -> Bool.not (Label.is_silent x)) xs
      ;;
    end

    module Note = struct
      type state = State.t
      type label = Label.t
      type trees = Base.Trees.t

      type t =
        { from : state
        ; label : label
        ; using : trees
        ; goto : state
        }

      module X = struct
        type nonrec t = t

        let name = "Note"

        let json ?(as_elt : bool = false) (x : t) : Yojson.t =
          `Assoc
            [ "from", State.json ~as_elt:true x.from
            ; "label", Label.json ~as_elt:true x.label
            ; "goto", State.json ~as_elt:true x.goto
            ; "using", Base.Trees.json ~as_elt:true x.using
            ]
        ;;

        let equal (a : t) (b : t) : bool =
          State.equal a.from b.from
          && State.equal a.goto b.goto
          && Label.equal a.label b.label
          && Base.Trees.equal a.using b.using
        ;;

        let compare (a : t) (b : t) : int =
          Utils.compare_chain
            [ State.compare a.from b.from
            ; State.compare a.goto b.goto
            ; Label.compare a.label b.label
            ; Base.Trees.compare a.using b.using
            ]
        ;;
      end

      include Thing.Make (X)

      let is_silent (x : t) : bool = Label.is_silent x.label
      let has_label (x : Label.t) (y : t) : bool = Label.equal x y.label
    end

    module Annotation = struct
      type label = Label.t
      type note = Note.t

      type t =
        { this : note
        ; next : t option
        }

      module X = struct
        type nonrec t = t

        let name = "Annotation"

        let rec json ?(as_elt : bool = false) (x : t) : Yojson.t =
          `Assoc
            [ "this", Note.json ~as_elt:true x.this
            ; ( "next"
              , match x.next with
                | None -> `String "None"
                | Some next -> json ~as_elt:true next )
            ]
        ;;

        let rec equal (a : t) (b : t) : bool =
          Note.equal a.this b.this && Option.equal equal a.next b.next
        ;;

        let rec compare (a : t) (b : t) : int =
          Utils.compare_chain
            [ Note.compare a.this b.this; Option.compare compare a.next b.next ]
        ;;
      end

      include Thing.Make (X)

      let is_empty : t -> bool = function
        | { this; next = None } -> true
        | _ -> false
      ;;

      exception AnnotationIsNone

      let opt_is_empty ?(fail_if_none : bool = false) : t option -> bool
        = function
        | None -> if fail_if_none then raise AnnotationIsNone else true
        | Some x -> is_empty x
      ;;

      let rec length : t -> int = function
        | { next = None; _ } -> 1
        | { next = Some next; _ } -> 1 + length next
      ;;

      let opt_length ?(fail_if_none : bool = false) : t option -> int = function
        | None -> if fail_if_none then raise AnnotationIsNone else 0
        | Some x -> length x
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
        | { this; next = None } -> Note.has_label x this
        | { this; next = Some next } ->
          if Note.has_label x this then true else exists_label x next
      ;;

      let rec append (x : Note.t) : t -> t = function
        | { this; next = None } ->
          { this; next = Some { this = x; next = None } }
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
    end

    module Annotations = struct
      include
        Thing.Set
          (Annotation)
          (struct
            let name = "Annotations"
          end)

      (** returns all of the possible actions after the named action *)
      let extrapolate (x : Annotation.t) : t =
        Logger.trace __FUNCTION__;
        let rec skip ({ this; next } : Annotation.t) : t =
          let xs =
            Stdlib.Option.fold
              ~none:empty
              ~some:(if Note.is_silent this then skip else get)
              next
            |> map (fun (y : Annotation.t) -> { this; next = Some y })
          in
          if Note.is_silent this then xs else add { this; next = None } xs
        and get : Annotation.t -> t = function
          | { this; next = None } -> singleton { this; next = None }
          | { this; next = Some next } ->
            get next
            |> map (fun (y : Annotation.t) -> { this; next = Some y })
            |> add { this; next = None }
        in
        add x (skip x)
      ;;
    end

    module Transition = struct
      type state = State.t
      type label = Label.t
      type tree = Base.Tree.t
      type annotation = Annotation.t

      type t =
        { from : state
        ; goto : state
        ; label : label
        ; tree : tree option
        ; annotation : annotation option
        }

      module X = struct
        type nonrec t = t

        let name = "Transition"

        let json ?(as_elt : bool = false) (x : t) : Yojson.t =
          `Assoc
            [ "from", State.json ~as_elt:true x.from
            ; "goto", State.json ~as_elt:true x.goto
            ; "label", Label.json ~as_elt:true x.label
            ; ( "annotation"
              , Json.option ~as_elt:true Annotation.json x.annotation )
            ; "tree", Json.option ~as_elt:true Base.Tree.json x.tree
            ]
        ;;

        let equal (a : t) (b : t) : bool =
          State.equal a.from b.from
          && State.equal a.goto b.goto
          && Label.equal a.label b.label
          && Option.equal Annotation.equal a.annotation b.annotation
          && Option.equal Base.Tree.equal a.tree b.tree
        ;;

        let compare (a : t) (b : t) : int =
          Utils.compare_chain
            [ State.compare a.from b.from
            ; State.compare a.goto b.goto
            ; Label.compare a.label b.label
            ; Option.compare Annotation.compare a.annotation b.annotation
            ; Option.compare Base.Tree.compare a.tree b.tree
            ]
        ;;
      end

      include Thing.Make (X)

      let is_silent (x : t) : bool = Label.is_silent x.label
    end

    module Transitions = struct
      type labels = Labels.t

      include
        Thing.Set
          (Transition)
          (struct
            let name = "Transitions"
          end)

      let labels (xs : t) : Labels.t =
        Logger.trace __FUNCTION__;
        fold
          (fun ({ label; _ } : Transition.t) : (Labels.t -> Labels.t) ->
            Labels.add label)
          xs
          Labels.empty
      ;;
    end

    module Action = struct
      type label = Label.t
      type annotation = Annotation.t
      type trees = Base.Trees.t

      type t =
        { label : label
        ; annotation : annotation option
        ; trees : trees
        }

      module X = struct
        type nonrec t = t

        let name = "Action"

        let json ?(as_elt : bool = false) (x : t) : Yojson.t =
          `Assoc
            [ "label", Label.json ~as_elt:true x.label
            ; ( "annotation"
              , Json.option ~as_elt:true Annotation.json x.annotation )
            ; "trees", Base.Trees.json ~as_elt:true x.trees
            ]
        ;;

        let equal (a : t) (b : t) : bool =
          Label.equal a.label b.label
          && Option.equal Annotation.equal a.annotation b.annotation
          && Base.Trees.equal a.trees b.trees
        ;;

        let compare (a : t) (b : t) : int =
          Utils.compare_chain
            [ Label.compare a.label b.label
            ; Option.compare Annotation.compare a.annotation b.annotation
            ; Base.Trees.compare a.trees b.trees
            ]
        ;;
      end

      include Thing.Make (X)

      let hash (x : t) : int = Label.hash x.label
      let wk_equal (a : t) (b : t) : bool = Label.equal a.label b.label
      let is_silent (x : t) : bool = Label.is_silent x.label
      let is_labelled (x : Label.t) (y : t) : bool = Label.equal x y.label

      let shorter_annotation (a : t) (b : t) : t =
        match
          Int.compare
            (Annotation.opt_length a.annotation)
            (Annotation.opt_length b.annotation)
        with
        | 1 -> b
        | _ -> a
      ;;
    end

    module Actions = struct
      type label = Label.t
      type labels = Labels.t

      include
        Thing.Set
          (Action)
          (struct
            let name = "Actions"
          end)

      let labelled (xs : t) (y : label) : t =
        Logger.trace __FUNCTION__;
        filter (fun ({ label; _ } : Action.t) -> Label.equal label y) xs
      ;;

      let labels (xs : t) : Labels.t =
        Logger.trace __FUNCTION__;
        fold
          (fun ({ label; _ } : Action.t) : (Labels.t -> Labels.t) ->
            Labels.add label)
          xs
          Labels.empty
      ;;
    end

    module ActionPair = struct
      type action = Action.t
      type states = States.t
      type t = action * states

      module X = struct
        type nonrec t = t

        let name = "ActionPair"

        let json ?as_elt (x : t) : Yojson.t =
          `Assoc
            [ "action", Action.json (fst x)
            ; "destinations", States.json (snd x)
            ]
        ;;

        let compare ((a, x) : t) ((b, y) : t) : int =
          Utils.compare_chain [ Action.compare a b; States.compare x y ]
        ;;

        (* Not part of the original Actionpair.S -- ActionPair had no [equal]
           before. Added only because Thing.Make needs one; defined so it
           agrees with [compare], which is the only property anything can
           actually rely on. *)
        let equal (a : t) (b : t) : bool = compare a b = 0
      end

      include Thing.Make (X)

      let shorter_annotation ((a, xs) : t) ((b, ys) : t) : t =
        match
          Int.compare
            (Annotation.opt_length a.annotation)
            (Annotation.opt_length b.annotation)
        with
        | 1 -> b, ys
        | _ -> a, xs
      ;;

      (** [try_update x a] returns [None, a] when [x] cannot be used to update a pre-existing tuple in [a], and [Some z, a'] where [z] is the updated tuple in [a] which has been removed in [a'].
          (* TODO:REFACTOR -- this is the reason so many functor params *) *)
      let try_update ((xaction, xdestinations) : t) (a : t list)
        : t option * t list
        =
        Logger.trace __FUNCTION__;
        let f : Annotation.t option * Annotation.t option -> Annotation.t option
          = function
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
                  ; trees = Base.Trees.union yaction.trees xaction.trees
                  }
                in
                Some (zaction, ydestinations), acc)
              else None, (yaction, ydestinations) :: acc)
          (None, [])
          a
      ;;

      (** [merge_lists a b] merges elements of [b] into [a], either by updating an element in [a] with additional annotation for a saturation tuple that describes the same action-destination, or in the case that the saturation tuple is not described within [a] by inserting it within [a].
      *)
      let rec merge_lists (a : t list) : t list -> t list =
        Logger.trace __FUNCTION__;
        function
        | [] -> a
        | h :: tl ->
          let (a : (Action.t * States.t) list) =
            match try_update h a with
            | None, a -> h :: a
            | Some updated, a -> updated :: a
          in
          merge_lists a tl
      ;;
    end

    module ActionPairs = struct
      type states = States.t

      include
        Thing.Set
          (ActionPair)
          (struct
            let name = "ActionPairs"
          end)

      let destinations (x : t) : States.t =
        to_list x
        |> List.fold_left
             (fun (acc : States.t) ((a, b) : ActionPair.t) ->
               States.union acc b)
             States.empty
      ;;

      exception IsEmpty

      (** returns the action in [x] that has the {e shortest} annotation (where [None] is treated as 0).
      *)
      let shortest_annotation (x : t) : ActionPair.t =
        match to_list x with
        | [] -> raise IsEmpty
        | h :: tl -> List.fold_left ActionPair.shorter_annotation h tl
      ;;

      let merge_list : t -> ActionPair.t list -> t =
        List.fold_left (fun (acc : t) ((a, s) : ActionPair.t) ->
          let matching =
            filter (fun ((b, t) : ActionPair.t) -> Action.equal a b) acc
          in
          if is_empty matching
          then add (a, s) acc
          else (
            let acc = diff acc matching in
            matching
            |> to_list
            |> List.map (fun (_, t) -> a, States.union s t)
            |> of_list
            |> union acc))
      ;;
    end

    module ActionMap = struct
      type label = Label.t
      type action = Action.t
      type actions = Actions.t
      type states = States.t
      type actionpairs = ActionPairs.t

      module Map_ : Hashtbl.S with type key = Action.t = Hashtbl.Make (Action)
      include Map_

      type t' = States.t t

      include
        Json.Map.Make
          (struct
            module Map = Map_

            type value = States.t

            let name = "ActionMap"
          end)
          (Action)
          (struct
            include States

            let name = "Destinations"
          end)

      let size (x : t') : int =
        fold (fun _ (ys : States.t) (z : int) -> z + States.cardinal ys) x 0
      ;;

      (** [update] ... if the action is already present, then along with merging the destination states, we also merge the constructor trees.
      *)
      let update (x : t') (action : Action.t) (states : States.t) : unit =
        Logger.trace __FUNCTION__;
        if States.is_empty states
        then ()
        else (
          match find_opt x action with
          | None -> add x action states
          | Some old_states ->
            let action : Action.t =
              to_seq_keys x
              |> Seq.filter (Action.equal action)
              |> Seq.fold_left
                   (fun (action : Action.t) (y : Action.t) ->
                     { action with
                       trees = Base.Trees.union action.trees y.trees
                     })
                   action
            in
            replace x action (States.union old_states states))
      ;;

      (** [destinations x f e] merges the values of [x] using [f], where [e] is some initial (i.e., "empty") collection of ['a].
      *)
      let destinations (x : t') : States.t =
        Logger.trace __FUNCTION__;
        to_seq_values x
        |> List.of_seq
        |> List.fold_left States.union States.empty
      ;;

      let reduce_by_label (x : t') (label : Label.t) : t' =
        Logger.trace __FUNCTION__;
        let y : t' = copy x in
        filter_map_inplace
          (fun (k : Action.t) (vs : States.t) ->
            if Label.equal k.label label then Some vs else None)
          y;
        y
      ;;

      let to_actions (x : t') : Actions.t = to_seq_keys x |> Actions.of_seq

      let to_actionpairs (x : t') : ActionPairs.t =
        Logger.trace __FUNCTION__;
        fold
          (fun (k : Action.t)
            (vs : States.t)
            : (ActionPairs.t -> ActionPairs.t) -> ActionPairs.add (k, vs))
          x
          ActionPairs.empty
      ;;

      let of_actionpairs (xs : ActionPairs.t) : t' =
        Logger.trace __FUNCTION__;
        let y : t' = create 0 in
        ActionPairs.iter (fun ((k, vs) : ActionPairs.elt) -> update y k vs) xs;
        y
      ;;

      let merge (a : t') (b : t') : t' =
        Logger.trace __FUNCTION__;
        ActionPairs.union (to_actionpairs a) (to_actionpairs b)
        |> of_actionpairs
      ;;
    end

    module Edge = struct
      type state = State.t
      type label = Label.t
      type action = Action.t

      type t =
        { from : state
        ; goto : state
        ; action : action
        }

      module X = struct
        type nonrec t = t

        let name = "Edge"

        let json ?(as_elt : bool = false) (x : t) : Yojson.t =
          `Assoc
            [ "from", State.json x.from
            ; "goto", State.json x.goto
            ; "action", Action.json x.action
            ]
        ;;

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
      end

      include Thing.Make (X)

      let is_silent (x : t) : bool = Action.is_silent x.action

      let is_labelled (x : Label.t) (y : t) : bool =
        Action.is_labelled x y.action
      ;;
    end

    module Edges = struct
      type label = Edge.label

      (* Name kept as "Edge" (singular), matching the JSON dump name the
         original edges.ml used -- not renamed to "Edges" here, to keep the
         dump format byte-for-byte unchanged. *)
      include
        Thing.Set
          (Edge)
          (struct
            let name = "Edge"
          end)

      let labelled (xs : t) (y : label) : t =
        Logger.trace __FUNCTION__;
        filter (Edge.is_labelled y) xs
      ;;
    end

    module EdgeMap = struct
      type state = State.t
      type states = States.t
      type label = Action.label
      type transitions = Transitions.t
      type action = Action.t
      type actions = Actions.t
      type actionmap = ActionMap.t'
      type edges = Edges.t

      module Map_ : Hashtbl.S with type key = State.t = Hashtbl.Make (State)
      include Map_

      type t' = ActionMap.t' t

      include
        Json.Map.Make
          (struct
            module Map = Map_

            type value = ActionMap.t'

            let name = "EdgeMap"
          end)
          (struct
            include State

            let name = "From"
          end)
          (struct
            include ActionMap

            let name = "Actions"
            let compare a b : int = 0
          end)

      let size (x : t') : int =
        fold (fun _ (ys : ActionMap.t') (z : int) -> z + ActionMap.size ys) x 0
      ;;

      let update
            (x : t')
            (from : State.t)
            (action : Action.t)
            (destinations : States.t)
        : unit
        =
        Logger.trace __FUNCTION__;
        match find_opt x from with
        | None ->
          ActionPairs.singleton (action, destinations)
          |> ActionMap.of_actionpairs
          |> add x from
        | Some actions -> ActionMap.update actions action destinations
      ;;

      let destinations (x : t') (from : State.t) : States.t =
        Logger.trace __FUNCTION__;
        match find_opt x from with
        | None -> States.empty
        | Some ys -> ActionMap.destinations ys
      ;;

      let get_actions (x : t') (from : State.t) : Actions.t =
        Logger.trace __FUNCTION__;
        find x from |> ActionMap.to_seq_keys |> Actions.of_seq
      ;;

      let reduce_by_label (x : t') (label : label) : t' =
        Logger.trace __FUNCTION__;
        let y : t' = copy x in
        filter_map_inplace
          (fun (k : State.t) (vs : ActionMap.t') ->
            let vs' : ActionMap.t' = ActionMap.reduce_by_label vs label in
            if ActionMap.length vs' > 0 then Some vs' else None)
          y;
        y
      ;;

      let get_edges (x : t') (from : State.t) : Edges.t =
        Logger.trace __FUNCTION__;
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
        Logger.trace __FUNCTION__;
        fold
          (fun (from : State.t) (vs : ActionMap.t') : (Edges.t -> Edges.t) ->
            ActionMap.to_actionpairs vs
            |> ActionPairs.fold
                 (fun
                     ((action, destinations) : ActionPairs.elt)
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
        Logger.trace __FUNCTION__;
        let ys : t' = create 0 in
        Edges.iter
          (fun ({ from; goto; action } : Edge.t) ->
            update ys from action (States.singleton goto))
          xs;
        ys
      ;;

      let of_transitions (xs : Transitions.t) : t' =
        Logger.trace __FUNCTION__;
        let edges : t' = create 0 in
        Transitions.iter
          (fun ({ from; goto; label; annotation; tree } : Transition.t) ->
            update
              edges
              from
              { label
              ; annotation
              ; trees =
                  Stdlib.Option.fold
                    ~none:Base.Trees.empty
                    ~some:Base.Trees.singleton
                    tree
              }
              (States.singleton goto))
          xs;
        edges
      ;;

      let merge (a : t') (b : t') : t' =
        Logger.trace __FUNCTION__;
        let c : t' = copy a in
        iter
          (fun (k : State.t) (vs : ActionMap.t') ->
            match find_opt c k with
            | Some actions -> ActionMap.merge actions vs |> replace c k
            | None -> add c k vs)
          b;
        c
      ;;
    end

    module Partition = struct
      type state = State.t
      type label = ActionMap.label
      type edgemap = EdgeMap.t'

      include
        Thing.Set
          (States)
          (struct
            let name = "Partitions"
          end)

      let get_bisimilar (x : State.t) : t -> States.t =
        find_first (fun (ys : States.t) -> States.mem x ys)
      ;;

      let filter_reachable (xs : States.t) : t -> t =
        filter (fun (y : States.t) ->
          Bool.not (States.is_empty (States.inter y xs)))
      ;;

      let reachable (from : State.t) (edges : EdgeMap.t') : t -> t =
        Logger.trace __FUNCTION__;
        filter_reachable (EdgeMap.destinations edges from)
      ;;

      let reachable_by_label
            (from : State.t)
            (label : label)
            (edges : EdgeMap.t')
        : t -> t
        =
        Logger.trace __FUNCTION__;
        let actions =
          ActionMap.reduce_by_label (EdgeMap.find edges from) label
        in
        filter_reachable (ActionMap.destinations actions)
      ;;
    end

    module Info = struct
      type base = Base.t
      type constructorbindings = ConstructorBindings.k
      type labels = Labels.t

      module Meta = struct
        module Bounds = struct
          type t =
            | States of int
            | Transitions of int
            | Merged of t * t

          include Json.Thing.Make (struct
              type k = t

              let name = "Bounds"

              let json ?(as_elt : bool = false) (x : t) : Yojson.t =
                let rec f : t -> Yojson.t = function
                  | States i -> `Assoc [ "by", `String "states"; "num", `Int i ]
                  | Transitions i ->
                    `Assoc [ "by", `String "transitions"; "num", `Int i ]
                  | Merged (a, b) ->
                    `Assoc [ "Merged", `Assoc [ "a", f a; "b", f b ] ]
                in
                f x
              ;;
            end)
        end

        module RocqLTS = struct
          type t =
            { base : base
            ; constructors : constructorbindings list
            }

          include Json.Thing.Make (struct
              type k = t

              let name = "RocqLTS"

              let json ?(as_elt : bool = false) (x : t) : Yojson.t =
                `Assoc
                  [ "base", Base.json x.base
                  ; ( "constructors"
                    , `List
                        (List.map
                           (ConstructorBindings.json ~as_elt:true)
                           x.constructors) )
                  ]
              ;;
            end)
        end

        type t =
          { is_complete : bool
          ; is_merged : bool
          ; bounds : Bounds.t
          ; lts : RocqLTS.t list
          }

        include Json.Thing.Make (struct
            type k = t

            let name = "Meta"

            let json ?as_elt (x : t) : Yojson.t =
              `Assoc
                [ "complete", `Bool x.is_complete
                ; "merged", `Bool x.is_merged
                ; "bounds", Bounds.json ~as_elt:true x.bounds
                ; "rocq lts", `List (List.map (RocqLTS.json ~as_elt:true) x.lts)
                ]
            ;;
          end)

        let merge (a : t) (b : t) : t =
          { is_complete = a.is_complete && b.is_complete
          ; is_merged = true
          ; bounds = Merged (a.bounds, b.bounds)
          ; lts =
              List.merge
                (fun (a : RocqLTS.t) (b : RocqLTS.t) ->
                  Base.compare a.base b.base)
                a.lts
                b.lts
          }
        ;;

        let merge_opt (a : t option) (b : t option) : t option =
          match a, b with
          | None, None -> None
          | Some a, Some b -> Some (merge a b)
          | Some a, None -> Some { a with is_merged = true }
          | None, Some b -> Some { b with is_merged = true }
        ;;
      end

      type t =
        { meta : Meta.t option
        ; weak_labels : labels
        ; nums : nums option
        }

      and nums =
        { states : int
        ; labels : int
        ; edges : int
        }

      include Json.Thing.Make (struct
          type k = t

          let name = "Info"

          let json ?as_elt (x : t) : Yojson.t =
            `Assoc
              [ ( "nums"
                , Json.option
                    (fun ?as_elt ({ states; labels; edges } : nums) ->
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

      let merge ?(nums : nums option = None) (a : t) (b : t) : t =
        { meta = Meta.merge_opt a.meta b.meta
        ; weak_labels = Labels.union a.weak_labels b.weak_labels
        ; nums
        }
      ;;
    end
  end

  module State = struct
    include Impl.State
    module Set = Impl.States
  end

  module Label = struct
    include Impl.Label
    module Set = Impl.Labels
  end

  module Note = Impl.Note

  module Annotation = struct
    include Impl.Annotation
    module Set = Impl.Annotations
  end

  module Transition = struct
    include Impl.Transition
    module Set = Impl.Transitions
  end

  module Action = struct
    include Impl.Action
    module Set = Impl.Actions

    module Pair = struct
      include Impl.ActionPair
      module Set = Impl.ActionPairs
    end

    module Map = Impl.ActionMap
  end

  module Edge = struct
    include Impl.Edge
    module Set = Impl.Edges
  end

  module EdgeMap = Impl.EdgeMap
  module Partition = Impl.Partition
  module Info = Impl.Info
end
