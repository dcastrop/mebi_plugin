module Make : (Log : Logger.S)
    (State : sig
       type t

       val json : ?as_elt:bool -> t -> Yojson.t

       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       val equal : t -> t -> bool
       val compare : t -> t -> int
       (* val hash : t -> int *)
     end)
    (States : sig
       include Set.S with type elt = State.t

       (* val json : ?as_elt:bool -> t -> Yojson.t
          val to_string : ?pretty:bool -> t -> string
          val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
          val add_to_opt : State.t -> t option -> t *)

       exception StateHasNoOrigin of (State.t * t * t)

       (* val origin_of_state : State.t -> t -> t -> int *)
       (* val has_shared_origin : t -> t -> t -> bool *)
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
    (Labels : sig
       include Set.S with type elt = Label.t

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val non_silent : t -> t *)
     end)
    (Tree : sig
       module Node : sig
         type t

         (* val compare : t -> t -> int *)
         (* val equal : t -> t -> bool *)
         (* val json : ?as_elt:bool -> t -> Yojson.t *)
         (* val to_string : ?pretty:bool -> t -> string *)
         (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       end

       type 'a tree = N of 'a * 'a tree list
       type t = Node.t tree

       (* val add : t -> t -> t *)
       (* val add_list : t -> t list -> t list *)
       (* val equal : t -> t -> bool *)
       (* val compare : t -> t -> int *)
       (* val minimize : t -> Node.t list *)

       exception CannotMinimizeEmptyList of unit

       (* val min : t list -> Node.t list *)
       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
     end)
    (Trees : sig
       include Set.S with type elt = Tree.t

       exception EmptyHasNoMin

       (* val min : t -> Tree.t *)
       (* val min_opt : t -> Tree.t option *)
       val json : ?as_elt:bool -> t -> Yojson.t
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
     end)
    (Note : sig
       type t =
         { from : State.t
         ; label : Label.t
         ; using : Trees.t
         ; goto : State.t
         }

       (* val equal : t -> t -> bool *)
       (* val compare : t -> t -> int *)
       (* val is_silent : t -> bool *)
       (* val has_label : Label.t -> t -> bool *)
       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
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
       val last : t -> Note.t

       exception CannotDropLastOfSingleton of t

       (* val drop_last : t -> t *)
     end)
    (Annotations : sig
       include Set.S with type elt = Annotation.t

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       val extrapolate : elt -> t
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
       val is_silent : t -> bool
       (* val is_labelled : Label.t -> t -> bool *)
       (* val shorter_annotation : t -> t -> t *)
     end)
    (* (Actions : sig
       include Set.S with type elt = Action.t

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
       val labelled : t -> Label.t -> t
       val labels : t -> Labels.t *)
       end) *)
    (ActionPair : sig
       type t = Action.t * States.t

       (* val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val compare : t -> t -> int *)
       (* val shorter_annotation : t -> t -> t *)
       (* val try_update : t -> t list -> t option * t list *)
       val merge_lists : t list -> t list -> t list
     end)
    (ActionPairs : sig
       include Set.S with type elt = ActionPair.t

       (* val json : ?as_elt:bool -> t -> Yojson.t
          val to_string : ?pretty:bool -> t -> string
          val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
          val destinations : t -> States.t *)

       exception IsEmpty

       (* val shortest_annotation : t -> ActionPair.t *)
       val merge_list : t -> ActionPair.t list -> t
     end)
    (ActionMap : sig
       include Hashtbl.S with type key = Action.t

       type t' = States.t t

       (* val json : ?as_elt:bool -> t' -> Yojson.t
          val to_string : ?pretty:bool -> t' -> string
          val log : ?__FUNCTION__:string -> ?s:string -> t' -> unit *)
       val update : t' -> Action.t -> States.t -> unit
       (* val destinations : t' -> States.t
          val reduce_by_label : t' -> Label.t -> t'
          val to_actions : t' -> Actions.t
          val to_actionpairs : t' -> ActionPairs.t
          val of_actionpairs : ActionPairs.t -> t'
          val merge : t' -> t' -> t' *)
     end)
    (* (Edge : sig
       type t =
         { from : State.t
         ; goto : State.t
         ; action : Action.t
         }

       (* val json : ?as_elt:bool -> t -> Yojson.t
          val to_string : ?pretty:bool -> t -> string
          val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
          val equal : t -> t -> bool
          val compare : t -> t -> int
          val is_silent : t -> bool
          val is_labelled : Label.t -> t -> bool *)
     end) *)
    (* (Edges : sig
       include Set.S with type elt = Edge.t

       (* val json : ?as_elt:bool -> t -> Yojson.t
          val to_string : ?pretty:bool -> t -> string
          val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
          val labelled : t -> Label.t -> t *)
     end) *)
    (EdgeMap : sig
       include Hashtbl.S with type key = State.t

       type t' = ActionMap.t' t

       (* val json : ?as_elt:bool -> t' -> Yojson.t
          val to_string : ?pretty:bool -> t' -> string
          val log : ?__FUNCTION__:string -> ?s:string -> t' -> unit
          val update : t' -> State.t -> Action.t -> States.t -> unit
          val destinations : t' -> State.t -> States.t
          val get_actions : t' -> State.t -> Actions.t
          val reduce_by_label : t' -> Label.t -> t'
          val get_edges : t' -> State.t -> Edges.t
          val to_edges : t' -> Edges.t
          val of_edges : Edges.t -> t'
          val merge : t' -> t' -> t' *)
     end)
    -> sig
  module WIP :
      module type of
        Model_wip_annotation.Make (Log) (State) (Label) (Tree) (Trees) (Note)
          (Annotation)
          (Action)

  module Trace :
      module type of
        Model_wip_trace.Make (Log) (State) (Label) (Tree) (Trees) (Note)
          (Annotation)
          (WIP)

  module Traces :
      module type of Model_wip_traces.Make (Log) (State) (WIP) (Trace)

  type data =
    { named : Label.t option
    ; current : Traces.elt option
    ; visited : States.t
    ; traces : Traces.t ref
    ; can_collect_traces : bool ref
    ; old_edges : EdgeMap.t'
    }

  val initial_data : Traces.t ref -> EdgeMap.t' -> data
  val has_named : data -> bool
  val update_traces : data -> Trace.t -> unit
  val update_named : Action.t -> data -> data
  val update_current : WIP.t -> data -> data
  val update_visited : State.t -> data -> data
  val already_visited : State.t -> data -> bool
  val skip_action : Action.t -> data -> bool
  val get_old_actions : State.t -> data -> ActionMap.t' option
  val update_acc : Trace.t -> Label.t -> ActionPairs.t -> ActionPairs.t
  val stop : data -> State.t -> ActionPairs.t -> ActionPairs.t

  val finish_with_trace
    :  Trace.t
    -> data
    -> Label.t
    -> ActionPairs.t
    -> ActionPairs.t

  val finish_with_trace_upto
    :  Trace.t
    -> data
    -> Label.t
    -> ActionPairs.t
    -> ActionPairs.t

  val check_from : data -> State.t -> ActionPairs.t -> ActionPairs.t

  val check_actions
    :  data
    -> State.t
    -> ActionMap.t'
    -> ActionPairs.t
    -> ActionPairs.t

  val collect_from_traces
    :  data
    -> State.t
    -> Action.t
    -> States.t
    -> ActionPairs.t
    -> ActionPairs.t

  val continue_check_destinations
    :  data
    -> State.t
    -> Action.t
    -> States.t
    -> ActionPairs.t
    -> ActionPairs.t

  val check_destinations
    :  data
    -> State.t
    -> States.t
    -> ActionPairs.t
    -> ActionPairs.t

  val edge_action_destinations : data -> State.t -> States.t -> ActionPairs.t

  val edge_actions
    :  State.t
    -> ActionMap.t'
    -> EdgeMap.t'
    -> Traces.t ref
    -> ActionPairs.t

  val edge
    :  ActionMap.t'
    -> State.t
    -> ActionMap.t'
    -> EdgeMap.t'
    -> Traces.t ref
    -> unit

  val edges : Labels.t -> States.t -> EdgeMap.t' -> EdgeMap.t'
end
