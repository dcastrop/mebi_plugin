module Make
    (Log : Logger.S)
    (State : sig
       type t

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val equal : t -> t -> bool *)
       (* val compare : t -> t -> int *)
       (* val hash : t -> int *)
     end)
    (States : sig
       include Set.S with type elt = State.t

       val json : ?as_elt:bool -> t -> Yojson.t
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val add_to_opt : State.t -> t option -> t *)

       exception StateHasNoOrigin of (State.t * t * t)

       (* val origin_of_state : State.t -> t -> t -> int *)
       (* val has_shared_origin : t -> t -> t -> bool *)
     end)
    (Label : sig
       type t

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       val equal : t -> t -> bool
       (* val compare : t -> t -> int *)
       (* val hash : t -> int *)
       (* val is_silent : t -> bool *)
     end)
    (* (Labels : sig
       include Set.S with type elt = Label.t

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val non_silent : t -> t *)
       end) *)
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

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)

       exception EmptyHasNoMin

       (* val min : t -> Tree.t *)
       (* val min_opt : t -> Tree.t option *)
     end)
    (* (Note : sig
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
     end) *)
     (Annotation : sig
       type t
       (* =
         { this : Note.t
         ; next : t option
         } *)

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

       val json : ?as_elt:bool -> t -> Yojson.t

       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       val equal : t -> t -> bool

       (* val compare : t -> t -> int *)
       val hash : t -> int
       (* val wk_equal : t -> t -> bool *)
       (* val is_silent : t -> bool *)
       (* val shorter_annotation : t -> t -> t *)
     end)
    (Actions : sig
       include Set.S with type elt = Action.t

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val labelled : t -> Label.t -> t *)
       (* val labels : t -> Labels.t *)
     end)
    (ActionPair : sig
       type t = Action.t * States.t

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val compare : t -> t -> int *)
       (* val try_update : t -> t list -> t option * t list *)
       (* val merge_lists : t list -> t list -> t list *)
     end)
    (ActionPairs : sig
       include Set.S with type elt = ActionPair.t

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val destinations : t -> States.t *)

       exception IsEmpty

       (* val shortest_annotation : t -> ActionPair.t *)
       (* val merge_list : t -> ActionPair.t list -> t *)
     end) : sig
  include Hashtbl.S with type key = Action.t

  type t' = States.t t

  val json : ?as_elt:bool -> t' -> Yojson.t
  val to_string : ?pretty:bool -> t' -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t' -> unit
  val update : t' -> Action.t -> States.t -> unit
  val destinations : t' -> States.t
  val reduce_by_label : t' -> Label.t -> t'
  val to_actions : t' -> Actions.t
  val to_actionpairs : t' -> ActionPairs.t
  val of_actionpairs : ActionPairs.t -> t'
  val merge : t' -> t' -> t'
end = struct
  module Map_ : Hashtbl.S with type key = Action.t = Hashtbl.Make (Action)
  include Map_

  type t' = States.t t

  include
    Json.Map.Make
      (Log)
      (struct
        module Map = Map_

        type value = States.t

        let name = "ActionMap"
        let kname = "Action"
        let vname = "Destinations"
        let kjson = Action.json
        let vjson = States.json
      end)

  (** [update] ... if the action is already present, then along with merging the destination states, we also merge the constructor trees.
  *)
  let update (x : t') (action : Action.t) (states : States.t) : unit =
    Log.trace __FUNCTION__;
    if States.is_empty states
    then ()
    else (
      match find_opt x action with
      | None -> add x action states
      | Some old_states ->
        (* NOTE: also find the existing key to merge the [constructor_trees] *)
        let action : Action.t =
          to_seq_keys x
          |> Seq.filter (Action.equal action)
          |> Seq.fold_left
               (fun (action : Action.t) (y : Action.t) ->
                 { action with
                   constructor_trees =
                     Trees.union action.constructor_trees y.constructor_trees
                 })
               action
        in
        replace x action (States.union old_states states))
  ;;

  (** [destinations x f e] merges the values of [x] using [f], where [e] is some initial (i.e., "empty") collection of ['a].
  *)
  let destinations (x : t') : States.t =
    Log.trace __FUNCTION__;
    to_seq_values x |> List.of_seq |> List.fold_left States.union States.empty
  ;;

  let reduce_by_label (x : t') (label : Label.t) : t' =
    Log.trace __FUNCTION__;
    let y : t' = copy x in
    filter_map_inplace
      (fun (k : Action.t) (vs : States.t) ->
        if Label.equal k.label label then Some vs else None)
      y;
    y
  ;;

  let to_actions (x : t') : Actions.t = to_seq_keys x |> Actions.of_seq

  let to_actionpairs (x : t') : ActionPairs.t =
    Log.trace __FUNCTION__;
    fold
      (fun (k : Action.t) (vs : States.t) : (ActionPairs.t -> ActionPairs.t) ->
        ActionPairs.add (k, vs))
      x
      ActionPairs.empty
  ;;

  let of_actionpairs (xs : ActionPairs.t) : t' =
    Log.trace __FUNCTION__;
    let y : t' = create 0 in
    ActionPairs.iter (fun ((k, vs) : ActionPair.t) -> update y k vs) xs;
    y
  ;;

  let merge (a : t') (b : t') : t' =
    Log.trace __FUNCTION__;
    ActionPairs.union (to_actionpairs a) (to_actionpairs b) |> of_actionpairs
  ;;
end
