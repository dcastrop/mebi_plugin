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

(** A state: a base term. *)
module type State_sig = sig
  type base
  type t = { base : base }

  include Json.S with type k = t

  (** [equal a b] is whether the states [a] and [b] have equal base terms.
      Raises nothing. *)
  val equal : t -> t -> bool

  (** [compare a b] orders states by their base terms. Raises nothing. *)
  val compare : t -> t -> int

  (** [hash x] is the hash of [x]'s base term, consistent with {!equal}.
      Raises nothing. *)
  val hash : t -> int
end

(** A set of states. *)
module type States_sig = sig
  include Set.S
  include Json.S with type k = t

  (** [add_to_opt x ys] is [ys] with [x] added, or the set of [x] alone if
      [ys] is [None]. Raises nothing. *)
  val add_to_opt : elt -> t option -> t

  (** Raised by {!origin_of_state}: the state is in neither set. *)
  exception StateHasNoOrigin of (elt * t * t)

  (** [origin_of_state x a b] is which of [a] and [b] the state [x] is in:
      [-1] for [a] only, [1] for [b] only, [0] for both.

      @raise StateHasNoOrigin if [x] is in neither (raised here). *)
  val origin_of_state : elt -> t -> t -> int

  (** [has_shared_origin a b c] is whether [a] has a state of [b] and a
      state of [c] (a state in both counts for either): whether [a] mixes
      the two systems.

      @raise StateHasNoOrigin
        if a state of [a] met before the answer is known is in neither [b]
        nor [c] (propagated from {!origin_of_state}). *)
  val has_shared_origin : t -> t -> t -> bool
end

(** A label: a base term, and whether it is silent, when known. *)
module type Label_sig = sig
  type base

  type t =
    { base : base
    ; is_silent : bool option
    }

  include Json.S with type k = t

  (** [equal a b] is whether the labels [a] and [b] have equal base terms;
      {!field-is_silent} is not compared. Raises nothing. *)
  val equal : t -> t -> bool

  (** [compare a b] orders labels by base term, then by {!field-is_silent} when
      both know it. Not a total order: a label whose {!field-is_silent} is [None]
      compares equal to the same base with [Some true] and with [Some false],
      which differ from each other ([TODO.md]). Raises nothing. *)
  val compare : t -> t -> int

  (** [hash x] is the hash of [x]'s base term, consistent with {!equal}.
      Raises nothing. *)
  val hash : t -> int

  (** [is_silent x] is whether [x] is known to be silent ([false] if
      unknown). Raises nothing. *)
  val is_silent : t -> bool
end

(** A set of labels. *)
module type Labels_sig = sig
  include Set.S
  include Json.S with type k = t

  (** [non_silent xs] is the labels of [xs] not known to be silent. Raises
      nothing. *)
  val non_silent : t -> t
end

(** One step of a weak transition's witness: [from -label-> goto], with
    the derivation trees [using] it was built from. *)
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

  (** [equal a b] is whether the notes [a] and [b] have equal source, target,
      label and derivation trees. Raises nothing. *)
  val equal : t -> t -> bool

  (** [compare a b] orders notes by source, target, label, then trees.
      Raises nothing. *)
  val compare : t -> t -> int

  (** [is_silent n] is whether [n]'s label is silent ({!Components.Label_sig.val-is_silent}).
      Raises nothing. *)
  val is_silent : t -> bool
end

(** A weak transition's witness: the non-empty sequence of its steps, the
    first in [this], the rest in [next]. *)
module type Annotation_sig = sig
  type label
  type note

  type t =
    { this : note
    ; next : t option
    }

  include Json.S with type k = t

  (** [equal a b] is whether the annotations [a] and [b] have equal notes, in
      order. Raises nothing. *)
  val equal : t -> t -> bool

  (** [compare a b] orders annotations note by note (a shorter one first if
      it is a prefix). Raises nothing. *)
  val compare : t -> t -> int

  (** Raised by {!opt_length} on [None] when asked to. *)
  exception AnnotationIsNone

  (** [length a] is the number of notes in [a] (at least 1). Raises nothing. *)
  val length : t -> int

  (** [opt_length ?fail_if_none a] is {!length} of the annotation [a], or [0]
      if it is [None].

      @raise AnnotationIsNone
        if [a] is [None] and [fail_if_none] is set
        (raised here). *)
  val opt_length : ?fail_if_none:bool -> t option -> int
end

(** A set of witnesses. *)
module type Annotations_sig = sig
  include Set.S
  include Json.S with type k = t
end

(** A transition of an LTS, [from -label-> goto], with its derivation tree
    (when extracted from Rocq) and its witness (when weak). *)
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

  (** [equal a b] is whether the transitions [a] and [b] agree on every field.
      Raises nothing. *)
  val equal : t -> t -> bool

  (** [compare a b] orders transitions by source, target, label, annotation,
      then derivation tree. Raises nothing. *)
  val compare : t -> t -> int

  (** [is_silent t] is whether [t]'s label is silent. Raises nothing. *)
  val is_silent : t -> bool
end

(** A set of transitions. *)
module type Transitions_sig = sig
  type labels

  include Set.S
  include Json.S with type k = t

  (** [labels ts] is the labels of the transitions [ts]. Raises nothing. *)
  val labels : t -> labels
end

(** An action of an FSM: a label, its witness when weak, and the
    derivation trees of the strong steps it stands for. An FSM maps each
    state to its actions, and each action to its destinations. *)
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

  (** [equal a b] is whether the actions [a] and [b] have equal label,
      witness and derivation trees. Raises nothing. *)
  val equal : t -> t -> bool

  (** [compare a b] orders actions by label, witness, then trees. Raises
      nothing. *)
  val compare : t -> t -> int

  (** [hash a] is a hash consistent with {!equal}: of the label alone for an
      action with no witness; for a weak action, also of the witness's
      length and its first and last states. Raises nothing. *)
  val hash : t -> int

  (** [is_silent a] is whether [a]'s label is silent. Raises nothing. *)
  val is_silent : t -> bool
end

(** A set of actions. *)
module type Actions_sig = sig
  type label
  type labels

  include Set.S
  include Json.S with type k = t
end

(** An action and its destinations. *)
module type Actionpair_sig = sig
  type action
  type states
  type t = action * states

  include Json.S with type k = t

  (** [compare a b] orders pairs by action, then destinations. Raises
      nothing. *)
  val compare : t -> t -> int

  (** [shorter_annotation p q] is the one of the pairs [p] and [q] whose
      action has the shorter witness (by {!Components.Annotation_sig.opt_length}); [p]
      if they are as long. Raises nothing. *)
  val shorter_annotation : t -> t -> t
end

(** A set of (action, destinations) pairs. *)
module type Actionpairs_sig = sig
  type states

  include Set.S
  include Json.S with type k = t

  (** [destinations ps] is the union of the destinations of [ps]. Raises
      nothing. *)
  val destinations : t -> states

  (** Raised by {!shortest_annotation} on an empty set. *)
  exception IsEmpty

  (** [shortest_annotation ps] is the pair of [ps] whose action has the
      shortest witness (none counts as 0); the first in set order on a tie.

      @raise IsEmpty if [ps] is empty (raised here). *)
  val shortest_annotation : t -> elt
end

(** The actions from one state: a table from each action to its
    destinations. *)
module type Actionmap_sig = sig
  type label
  type action
  type actions
  type states
  type actionpairs

  include Hashtbl.S with type key = action

  type t' = states t

  include Json.S with type k = t'

  (** [size m] is the number of (action, destination) pairs in [m]. Raises
      nothing. *)
  val size : t' -> int

  (** [update m a d] adds the destinations [d] to the action [a] in [m]
      (nothing if [d] is empty). Meant also to merge the derivation trees of
      equal actions, but since {!Components.Action_sig.equal} compares the trees, the
      actions it finds already have them ([TODO.md]). Raises nothing. *)
  val update : t' -> action -> states -> unit

  (** [destinations m] is the union of [m]'s destinations. Raises nothing. *)
  val destinations : t' -> states

  (** [reduce_by_label m l] is a copy of [m] with only its actions labelled
      [l]. Raises nothing. *)
  val reduce_by_label : t' -> label -> t'

  (** [to_actionpairs m] is [m] as a set of (action, destinations) pairs.
      Raises nothing. *)
  val to_actionpairs : t' -> actionpairs

  (** [of_actionpairs ps] is a new table of the pairs [ps] ({!update} for
      each). Raises nothing. *)
  val of_actionpairs : actionpairs -> t'

  (** [merge a b] is a new table with the pairs of both [a] and [b]. Raises
      nothing. *)
  val merge : t' -> t' -> t'
end

(** An FSM's steps: a table from each state to its actions
    ({!Actionmap_sig}). *)
module type Edgemap_sig = sig
  type state
  type states
  type label
  type transitions
  type action
  type actions
  type actionmap

  include Hashtbl.S with type key = state

  type t' = actionmap t

  include Json.S with type k = t'

  (** [size m] is the number of (source, action, destination) triples in
      [m]. Raises nothing. *)
  val size : t' -> int

  (** [update m s a d] adds the destinations [d] to the action [a] from
      [s] in [m], giving [s] an entry if it has none. Raises nothing. *)
  val update : t' -> state -> action -> states -> unit

  (** [destinations m s] is every state one action from [s] in [m] (none if
      [s] has no entry). Raises nothing. *)
  val destinations : t' -> state -> states

  (** [reduce_by_label m l] is a copy of [m] with only its actions labelled
      [l], dropping states left with none. Raises nothing. *)
  val reduce_by_label : t' -> label -> t'

  (** [of_transitions ts] is a new table of the transitions [ts], each an
      action with its label, annotation and (if it has one) derivation tree.
      Raises nothing. *)
  val of_transitions : transitions -> t'

  (** [merge a b] is a copy of [a] with [b]'s entries added; a state in both
      gets its action tables merged ({!Components.Actionmap_sig.merge}). Raises nothing.
  *)
  val merge : t' -> t' -> t'
end

(** A partition of states into blocks (sets of states), as a
    minimisation or bisimilarity check leaves it. *)
module type State_partition_sig = sig
  type state
  type label
  type edgemap

  include Set.S
  include Json.S with type k = t

  (** [get_bisimilar x p] is the block of the partition [p] containing [x].

      @raise Not_found if no block does (raised here). *)
  val get_bisimilar : state -> t -> elt

  (** [filter_reachable xs p] is the blocks of [p] with a state of [xs].
      Raises nothing. *)
  val filter_reachable : elt -> t -> t

  (** [reachable s m p] is the blocks of [p] with a state one action from
      [s] in [m]. Raises nothing. *)
  val reachable : state -> edgemap -> t -> t
end

(** What a model records about itself: its metadata (whether exploration
    completed, its bounds, the Rocq LTSs it came from), its weak labels,
    and its counts. *)
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

    (** [merge a b] is the metadata of a model made of the two models [a] and
        [b] describe: complete if both are, marked merged, both bounds kept,
        and both lists of Rocq LTSs merged by base term (sorted lists stay
        sorted). Raises nothing. *)
    val merge : t -> t -> t

    (** [merge_opt a b] is {!merge} of [a] and [b], or whichever is present,
        marked merged, or [None]. Raises nothing. *)
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

  (** [merge ?nums a b] is the information of a model made of the two that
      [a] and [b] describe: metadata merged ({!Meta.merge_opt}), weak labels
      united, and the counts [nums] (default [None]: not computed here).
      Raises nothing. *)
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
     so would require [State] to be declared both before [Note]/[Transition]
     (which need its bare type) and after [Action]/[Transition] (which
     [State.Map] would need), an ordering cycle ordinary (non-[rec])
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

  module EdgeMap :
    Edgemap_sig
    with type state = State.t
     and type states = State.Set.t
     and type label = Label.t
     and type transitions = Transition.Set.t
     and type action = Action.t
     and type actions = Action.Set.t
     and type actionmap = Action.Map.t'

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
     under State/Label/Annotation/Transition/Action afterwards without
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

      (* See {!module-type:State_sig}. *)
      let hash x = Base.hash x.base
    end

    module States = struct
      include
        Thing.Set
          (State)
          (struct
            let name = "States"
          end)

      (* See {!module-type:States_sig}. *)
      let add_to_opt (x : State.t) (ys : t option) : t =
        add x (Stdlib.Option.value ys ~default:empty)
      ;;

      exception StateHasNoOrigin of (State.t * t * t)

      (* See {!module-type:States_sig}. *)
      let origin_of_state (x : State.t) (a : t) (b : t) : int =
        match mem x a, mem x b with
        | true, true -> 0
        | true, false -> -1
        | false, true -> 1
        | false, false -> raise (StateHasNoOrigin (x, a, b))
      ;;

      (* See {!module-type:States_sig}. *)
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

      (* See {!module-type:Label_sig}. *)
      let hash (x : t) : int = Base.hash x.base

      (* See {!module-type:Label_sig}. *)
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

      (* See {!module-type:Labels_sig}. *)
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

      (* See {!module-type:Annotation_note_sig}. *)
      let is_silent (x : t) : bool = Label.is_silent x.label
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

      exception AnnotationIsNone

      (* See {!module-type:Annotation_sig}. *)
      let rec length : t -> int = function
        | { next = None; _ } -> 1
        | { next = Some next; _ } -> 1 + length next
      ;;

      (* See {!module-type:Annotation_sig}. *)
      let opt_length ?(fail_if_none : bool = false) : t option -> int = function
        | None -> if fail_if_none then raise AnnotationIsNone else 0
        | Some x -> length x
      ;;
    end

    module Annotations = struct
      include
        Thing.Set
          (Annotation)
          (struct
            let name = "Annotations"
          end)
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

      (* See {!module-type:Transition_sig}. *)
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

      (* See {!module-type:Transitions_sig}. *)
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

      (* Consistent with [equal] (equal actions have equal labels and
         witnesses). An action without a witness hashes by its label alone,
         as before, so unsaturated FSMs keep their table order (which
         [ReModel.transition] breaks ties by). A saturated action also hashes
         its witness: its length and its first and last states. Hashing the
         label alone put all of a state's weak actions under one label into
         one bucket, so filling its action map scanned that bucket per
         insertion with a deep equality -- ~30M comparisons per state on
         [Proc/Test4], seconds per state (2026-10-03). *)
      let hash (x : t) : int =
        match x.annotation with
        | None -> Label.hash x.label
        | Some (a : Annotation.t) ->
          let rec last (a : Annotation.t) (n : int) : Note.t * int =
            match a.next with None -> a.this, n | Some b -> last b (n + 1)
          in
          let (z : Note.t), (n : int) = last a 1 in
          Hashtbl.hash
            (Label.hash x.label, n, State.hash a.this.from, State.hash z.goto)
      ;;

      (* See {!module-type:Action_sig}. *)
      let is_silent (x : t) : bool = Label.is_silent x.label
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

      (* See {!module-type:Actionpair_sig}. *)
      let shorter_annotation ((a, xs) : t) ((b, ys) : t) : t =
        match
          Int.compare
            (Annotation.opt_length a.annotation)
            (Annotation.opt_length b.annotation)
        with
        | 1 -> b, ys
        | _ -> a, xs
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

      (* See {!module-type:Actionpairs_sig}. *)
      let destinations (x : t) : States.t =
        to_list x
        |> List.fold_left
             (fun (acc : States.t) ((a, b) : ActionPair.t) ->
               States.union acc b)
             States.empty
      ;;

      exception IsEmpty

      (* See {!module-type:Actionpairs_sig}. *)
      let shortest_annotation (x : t) : ActionPair.t =
        match to_list x with
        | [] -> raise IsEmpty
        | h :: tl -> List.fold_left ActionPair.shorter_annotation h tl
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

      (* See {!module-type:Actionmap_sig}. *)
      let size (x : t') : int =
        fold (fun _ (ys : States.t) (z : int) -> z + States.cardinal ys) x 0
      ;;

      (** [with_trees_of a b] is the action [a] with [b]'s derivation trees
          added to its own. Raises nothing. *)
      let with_trees_of (a : Action.t) (b : Action.t) : Action.t =
        { a with trees = Base.Trees.union a.trees b.trees }
      ;;

      (* See {!module-type:Actionmap_sig}. The scan over every key, for the trees of equal
         actions, makes each update linear in the table's size. *)
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
              |> Seq.fold_left with_trees_of action
            in
            replace x action (States.union old_states states))
      ;;

      (* See {!module-type:Actionmap_sig}. *)
      let destinations (x : t') : States.t =
        Logger.trace __FUNCTION__;
        to_seq_values x
        |> List.of_seq
        |> List.fold_left States.union States.empty
      ;;

      (* See {!module-type:Actionmap_sig}. *)
      let reduce_by_label (x : t') (label : Label.t) : t' =
        Logger.trace __FUNCTION__;
        let y : t' = copy x in
        filter_map_inplace
          (fun (k : Action.t) (vs : States.t) ->
            if Label.equal k.label label then Some vs else None)
          y;
        y
      ;;

      (* See {!module-type:Actionmap_sig}. *)
      let to_actionpairs (x : t') : ActionPairs.t =
        Logger.trace __FUNCTION__;
        fold
          (fun (k : Action.t)
            (vs : States.t)
            : (ActionPairs.t -> ActionPairs.t) -> ActionPairs.add (k, vs))
          x
          ActionPairs.empty
      ;;

      (* See {!module-type:Actionmap_sig}. *)
      let of_actionpairs (xs : ActionPairs.t) : t' =
        Logger.trace __FUNCTION__;
        let y : t' = create 0 in
        ActionPairs.iter (fun ((k, vs) : ActionPairs.elt) -> update y k vs) xs;
        y
      ;;

      (* See {!module-type:Actionmap_sig}. *)
      let merge (a : t') (b : t') : t' =
        Logger.trace __FUNCTION__;
        ActionPairs.union (to_actionpairs a) (to_actionpairs b)
        |> of_actionpairs
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

      (* See {!module-type:Edgemap_sig}. *)
      let size (x : t') : int =
        fold (fun _ (ys : ActionMap.t') (z : int) -> z + ActionMap.size ys) x 0
      ;;

      (* See {!module-type:Edgemap_sig}. *)
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

      (* See {!module-type:Edgemap_sig}. *)
      let destinations (x : t') (from : State.t) : States.t =
        Logger.trace __FUNCTION__;
        match find_opt x from with
        | None -> States.empty
        | Some ys -> ActionMap.destinations ys
      ;;

      (* See {!module-type:Edgemap_sig}. *)
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

      (** [action_of_transition t] is the action [t] takes: its label and
          witness, and its derivation tree, if it has one. Raises nothing. *)
      let action_of_transition ({ label; annotation; tree; _ } : Transition.t)
        : Action.t
        =
        { label
        ; annotation
        ; trees =
            Stdlib.Option.fold
              ~none:Base.Trees.empty
              ~some:Base.Trees.singleton
              tree
        }
      ;;

      (* See {!module-type:Edgemap_sig}. *)
      let of_transitions (xs : Transitions.t) : t' =
        Logger.trace __FUNCTION__;
        let edges : t' = create 0 in
        Transitions.iter
          (fun (t : Transition.t) ->
            update
              edges
              t.from
              (action_of_transition t)
              (States.singleton t.goto))
          xs;
        edges
      ;;

      (* See {!module-type:Edgemap_sig}. *)
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

      (* See {!module-type:State_partition_sig}. Not [find_first]: that returns the least
         element satisfying a predicate and requires the predicate to be
         monotonically increasing over the set's ordering, which "this block
         contains [x]" is not. With a non-monotonic predicate its binary search
         is unspecified, and it really does miss -- on a ten-block partition of
         twenty states it failed to find the block holding the second state.
         Both callers ({!Mebi_plugin.Results.get_bisimilar_states} and
         {!Product.S.successors}) turn [Not_found] into the empty set, so the
         miss surfaced not as an error but as a state with nothing bisimilar
         to it. Found 2026-09-29 while writing a {!Product.S.estimate} test. *)
      let get_bisimilar (x : State.t) (p : t) : States.t =
        let matching : t = filter (fun (ys : States.t) -> States.mem x ys) p in
        if is_empty matching then raise Not_found else choose matching
      ;;

      (* See {!module-type:State_partition_sig}. *)
      let filter_reachable (xs : States.t) : t -> t =
        filter (fun (y : States.t) ->
          Bool.not (States.is_empty (States.inter y xs)))
      ;;

      (* See {!module-type:State_partition_sig}. *)
      let reachable (from : State.t) (edges : EdgeMap.t') : t -> t =
        Logger.trace __FUNCTION__;
        filter_reachable (EdgeMap.destinations edges from)
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

        (* See {!module-type:Info_sig}. *)
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

        (* See {!module-type:Info_sig}. *)
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

      (* See {!module-type:Info_sig}. *)
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

  module EdgeMap = Impl.EdgeMap
  module Partition = Impl.Partition
  module Info = Impl.Info
end
