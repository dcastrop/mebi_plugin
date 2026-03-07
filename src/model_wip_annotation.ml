(** [module WIP] is a lightweight counterpart of [Note.t] that forms some "work-in-progress" [Annotation.t]. Once we stop saturating an action, we check if we are able to yield a new saturated action and convert the [wip list] to an [Annotation.t].
*)
module Make
    (Log : Logger.S)
    (State : sig
       type t

       val json : ?as_elt:bool -> t -> Yojson.t

       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       val equal : t -> t -> bool
       val compare : t -> t -> int
       (* val hash : t -> int *)
     end)
    (Label : sig
       type t

       val json : ?as_elt:bool -> t -> Yojson.t

       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       val equal : t -> t -> bool
       val compare : t -> t -> int

       (* val hash : t -> int *)
       val is_silent : t -> bool
     end)
    (Tree : sig
       module Node : sig
         type t

         (* val json : ?as_elt:bool -> t -> Yojson.t *)
         (* val to_string : ?pretty:bool -> t -> string *)
         (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
         (* val compare : t -> t -> int *)
         (* val equal : t -> t -> bool *)
       end

       type 'a tree = N of 'a * 'a tree list
       type t = Node.t tree

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val add : t -> t -> t *)
       (* val add_list : t -> t list -> t list *)
       (* val equal : t -> t -> bool *)
       (* val compare : t -> t -> int *)
       (* val minimize : t -> Node.t list *)

       exception CannotMinimizeEmptyList of unit

       (* val min : t list -> Node.t list *)
     end)
    (Trees : sig
       include Set.S with type elt = Tree.t

       val json : ?as_elt:bool -> t -> Yojson.t

       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       exception EmptyHasNoMin

       (* val min : t -> Tree.t *)
       (* val min_opt : t -> Tree.t option *)
     end)
    (Note : sig
       type t =
         { from : State.t
         ; label : Label.t
         ; using : Trees.t
         ; goto : State.t
         }

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val equal : t -> t -> bool *)
       (* val compare : t -> t -> int *)
       (* val is_silent : t -> bool *)
       (* val has_label : Label.t -> t -> bool *)
     end)
    (Annotation : sig
       type t =
         { this : Note.t
         ; next : t option
         }

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val equal : t -> t -> bool *)
       (* val compare : t -> t -> int *)
       (* val is_empty : t -> bool *)
       (* val opt_is_empty : ?fail_if_none:bool -> t option -> bool *)
       (* val length : t -> int *)
       (* val opt_length : ?fail_if_none:bool -> t option -> int *)
       (* val shorter : t -> t -> t *)
       (* val exists : Note.t -> t -> bool *)
       (* val exists_label : Label.t -> t -> bool *)
       (* val append : Note.t -> t -> t *)
       (* val last : t -> Note.t *)

       exception CannotDropLastOfSingleton of t

       (* val drop_last : t -> t *)
     end)
    (Action : sig
       type t =
         { label : Label.t
         ; annotation : Annotation.t option
         ; constructor_trees : Trees.t
         }

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val equal : t -> t -> bool *)
       (* val compare : t -> t -> int *)
       (* val hash : t -> int *)
       (* val wk_equal : t -> t -> bool *)
       (* val is_silent : t -> bool *)
       (* val is_labelled : Label.t -> t -> bool *)
       (* val shorter_annotation : t -> t -> t *)
     end) : sig
  type t =
    { from : State.t
    ; via : Label.t
    ; trees : Trees.t
    }

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val is_silent : t -> bool
  val is_named : t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val create : State.t -> Action.t -> t

  exception IsEmptyList

  val list_to_annotation : State.t -> t list -> Annotation.t
end = struct
  type t =
    { from : State.t
    ; via : Label.t
    ; trees : Trees.t
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
            ; "trees", Trees.json ~as_elt:true x.trees
            ]
        ;;
      end)

  let is_silent (x : t) : bool = Label.is_silent x.via
  let is_named (x : t) : bool = is_silent x |> Bool.not

  let equal (a : t) (b : t) : bool =
    State.equal a.from b.from
    && Label.equal a.via b.via
    && Trees.equal a.trees b.trees
  ;;

  let compare (a : t) (b : t) : int =
    Utils.compare_chain
      [ State.compare a.from b.from
      ; Label.compare a.via b.via
      ; Trees.compare a.trees b.trees
      ]
  ;;

  let create (from : State.t) (action : Action.t) : t =
    { from; via = action.label; trees = action.constructor_trees }
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
