module type S = sig
  type state
  type label
  type trees
  type annotation
  type action

  type t =
    { from : state
    ; via : label
    ; trees : trees
    }

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
  val is_silent : t -> bool
  val is_named : t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val create : state -> action -> t

  exception IsEmptyList

  val list_to_annotation : state -> t list -> annotation
end

(** [module WIP] is a lightweight counterpart of [Note.t] that forms some "work-in-progress" [Annotation.t]. Once we stop saturating an action, we check if we are able to yield a new saturated action and convert the [wip list] to an [Annotation.t].
*)
module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type base = Base.t)
    (Label : Label.S with type base = Base.t)
    (Note :
       Annotation_note.S
       with type state = State.t
        and type label = Label.t
        and type trees = Base.Trees.t)
    (Annotation : Annotation.S with type label = Label.t and type note = Note.t)
    (Action :
       Action.S
       with type label = Label.t
        and type annotation = Annotation.t
        and type trees = Base.Trees.t) :
  S
  with type state = State.t
   and type label = Label.t
   and type annotation = Annotation.t
   and type trees = Base.Trees.t
   and type action = Action.t = struct
  type state = State.t
  type label = Label.t
  type trees = Base.Trees.t
  type annotation = Annotation.t
  type action = Action.t

  type t =
    { from : state
    ; via : label
    ; trees : trees
    }

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "WorkInProgress"

        let json ?as_elt (x : t) : Yojson.t =
          `Assoc
            [ "from", State.json ~as_elt:true x.from
            ; "via", Label.json ~as_elt:true x.via
            ; "trees", Base.Trees.json ~as_elt:true x.trees
            ]
        ;;
      end)

  let is_silent (x : t) : bool = Label.is_silent x.via
  let is_named (x : t) : bool = is_silent x |> Bool.not

  let equal (a : t) (b : t) : bool =
    State.equal a.from b.from
    && Label.equal a.via b.via
    && Base.Trees.equal a.trees b.trees
  ;;

  let compare (a : t) (b : t) : int =
    Utils.compare_chain
      [ State.compare a.from b.from
      ; Label.compare a.via b.via
      ; Base.Trees.compare a.trees b.trees
      ]
  ;;

  let create (from : State.t) (action : Action.t) : t =
    { from; via = action.label; trees = action.trees }
  ;;

  exception IsEmptyList

  let list_to_annotation (goto : State.t) (xs : t list) : Annotation.t =
    Log.trace __FUNCTION__;
    let rec f : t list -> Annotation.t = function
      | [] -> raise IsEmptyList
      | { from; via; trees } :: [] ->
        { this = { from; label = via; using = trees; goto }; next = None }
      | { from; via; trees } :: h :: tl ->
        let { from = goto; via = via2; trees = tree2 } : t = h in
        { this = { from; label = via; using = trees; goto }
        ; next = Some (f (h :: tl))
        }
    in
    f (List.rev xs)
  ;;
end
