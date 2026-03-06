module Make
    (Log : Logger.S)
    (State : sig
       type t

       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
       val equal : t -> t -> bool
       val compare : t -> t -> int
       val hash : t -> int
     end)
    (States : sig
       include Set.S with type elt = State.t

       val add_to_opt : State.t -> t option -> t

       exception StateHasNoOrigin of (State.t * t * t)

       val origin_of_state : State.t -> t -> t -> int
       val has_shared_origin : t -> t -> t -> bool
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (Label : sig
       type t

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val hash : t -> int
       val is_silent : t -> bool
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (Labels : sig
       include Set.S with type elt = Label.t

       val non_silent : t -> t
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (Action : sig
       type t

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val hash : t -> int
       val wk_equal : t -> t -> bool
       val is_silent : t -> bool
       val is_labelled : Label.t -> t -> bool
       val shorter_annotation : t -> t -> t
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (Actions : sig
       include Set.S with type elt = Action.t

       val labelled : t -> Label.t -> t
       val labels : t -> Labels.t
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (ActionPair : sig
       type t = Action.t * States.t

       val compare : t -> t -> int
       val try_update : t -> t list -> t option * t list
       val merge_lists : t list -> t list -> t list
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (ActionPairs : sig
       include Set.S with type elt = ActionPair.t

       val destinations : t -> States.t

       exception IsEmpty

       val shortest_annotation : t -> ActionPair.t
       val merge_list : t -> ActionPair.t list -> t
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (ActionMap : sig
       include Hashtbl.S with type key = Action.t

       type t' = States.t t

       val update : t' -> Action.t -> States.t -> unit
       val destinations : t' -> States.t
       val reduce_by_label : t' -> Label.t -> t'
       val to_actions : t' -> Actions.t
       val to_actionpairs : t' -> ActionPairs.t
       val of_actionpairs : ActionPairs.t -> t'
       val merge : t' -> t' -> t'
       val json : ?as_elt:bool -> t' -> Yojson.t
       val to_string : ?pretty:bool -> t' -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t' -> unit
     end)
    (Edge : sig
       type t =
         { from : State.t
         ; goto : State.t
         ; action : Action.t
         }

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val is_silent : t -> bool
       val is_labelled : Label.t -> t -> bool
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (Edges : sig
       include Set.S with type elt = Edge.t

       val labelled : t -> Label.t -> t
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (EdgeMap : sig
       include Hashtbl.S with type key = State.t

       type t' = ActionMap.t' t

       val update : t' -> State.t -> Action.t -> States.t -> unit
       val destinations : t' -> State.t -> States.t
       val get_actions : t' -> State.t -> Actions.t
       val reduce_by_label : t' -> Label.t -> t'
       val get_edges : t' -> State.t -> Edges.t
       val to_edges : t' -> Edges.t
       val of_edges : Edges.t -> t'
       val merge : t' -> t' -> t'
       val json : ?as_elt:bool -> t' -> Yojson.t
       val to_string : ?pretty:bool -> t' -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t' -> unit
     end) : sig
  include Set.S with type elt = States.t

  val get_bisimilar : State.t -> t -> States.t
  val filter_reachable : States.t -> t -> t
  val reachable : State.t -> EdgeMap.t' -> t -> t
  val reachable_by_label : State.t -> Label.t -> EdgeMap.t' -> t -> t
  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
end = struct
  module Set_ : Set.S with type elt = States.t = Set.Make (States)
  include Set_

  let get_bisimilar (x : State.t) : t -> States.t =
    find_first (fun (ys : States.t) -> States.mem x ys)
  ;;

  let filter_reachable (xs : States.t) : t -> t =
    filter (fun (y : States.t) ->
      Bool.not (States.is_empty (States.inter y xs)))
  ;;

  let reachable (from : State.t) (edges : EdgeMap.t') : t -> t =
    Log.trace __FUNCTION__;
    filter_reachable (EdgeMap.destinations edges from)
  ;;

  let reachable_by_label (from : State.t) (label : Label.t) (edges : EdgeMap.t')
    : t -> t
    =
    Log.trace __FUNCTION__;
    let actions = ActionMap.reduce_by_label (EdgeMap.find edges from) label in
    filter_reachable (ActionMap.destinations actions)
  ;;

  include
    Json.Set.Make
      (Log)
      (struct
        module Set = Set_

        let name = "Partitions"
        let json = States.json
      end)
end
