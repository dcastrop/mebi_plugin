module Make : (Log : Logger.S)
    (State : sig
       type t

       (* val json : ?as_elt:bool -> t -> Yojson.t *)

       (* val to_string : ?pretty:bool -> t -> string *)
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
       val equal : t -> t -> bool
       (* val compare : t -> t -> int *)
       (* val hash : t -> int *)
     end)
    (States : sig
       include Set.S with type elt = State.t

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       val add_to_opt : State.t -> t option -> t

       exception StateHasNoOrigin of (State.t * t * t)

       (* val origin_of_state : State.t -> t -> t -> int *)
       (* val has_shared_origin : t -> t -> t -> bool *)
     end)
    (Label : sig
       type t

       (* val json : ?as_elt:bool -> t -> Yojson.t *)

       (* val to_string : ?pretty:bool -> t -> string *)
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
       (* val equal : t -> t -> bool *)
       (* val compare : t -> t -> int *)

       (* val hash : t -> int *)
       (* val is_silent : t -> bool *)
     end)
    (Labels : sig
       include Set.S with type elt = Label.t

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       val non_silent : t -> t
     end)
    (Action : sig
       type t
       (* =
         { label : Label.t
         ; annotation : Annotation.t option
         ; constructor_trees : Trees.t
         } *)

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
     end)
    (ActionMap : sig
       include Hashtbl.S with type key = Action.t

       type t' = States.t t

       (* val json : ?as_elt:bool -> t' -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t' -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t' -> unit *)
       (* val update : t' -> Action.t -> States.t -> unit *)
       (* val destinations : t' -> States.t *)
       (* val reduce_by_label : t' -> Label.t -> t' *)
       (* val to_actions : t' -> Actions.t *)
       (* val to_actionpairs : t' -> ActionPairs.t *)
       (* val of_actionpairs : ActionPairs.t -> t' *)
       (* val merge : t' -> t' -> t' *)
     end)
    (EdgeMap : sig
       include Hashtbl.S with type key = State.t

       type t' = ActionMap.t' t

       (* val json : ?as_elt:bool -> t' -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t' -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t' -> unit *)
       (* val update : t' -> State.t -> Action.t -> States.t -> unit *)
       (* val destinations : t' -> State.t -> States.t *)
       (* val get_actions : t' -> State.t -> Actions.t *)
       val reduce_by_label : t' -> Label.t -> t'
       (* val get_edges : t' -> State.t -> Edges.t *)
       (* val to_edges : t' -> Edges.t *)
       (* val of_edges : Edges.t -> t' *)
       (* val merge : t' -> t' -> t' *)
     end)
    (Partition : sig
       include Set.S with type elt = States.t

       val json : ?as_elt:bool -> t -> Yojson.t

       (* val to_string : ?pretty:bool -> t -> string *)
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit

       (* val get_bisimilar : State.t -> t -> States.t *)
       (* val filter_reachable : States.t -> t -> t *)
       val reachable : State.t -> EdgeMap.t' -> t -> t
       (* val reachable_by_label : State.t -> Label.t -> EdgeMap.t' -> t -> t *)
     end)
    (Info : sig
       module Meta : sig
         module Bounds : sig
           type t =
             | States of int
             | Transitions of int
             | Merged of t * t

           (* val json : ?as_elt:bool -> t -> Yojson.t
              val to_string : ?pretty:bool -> t -> string
              val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
         end

         module RocqLTS : sig
           type t

           (* val json : ?as_elt:bool -> t -> Yojson.t
              val to_string : ?pretty:bool -> t -> string
              val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
         end

         type t =
           { is_complete : bool
           ; is_merged : bool
           ; bounds : Bounds.t
           ; lts : RocqLTS.t list
           }

         (* val json : ?as_elt:bool -> t -> Yojson.t
            val to_string : ?pretty:bool -> t -> string
            val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       end

       type t =
         { meta : Meta.t option
         ; weak_labels : Labels.t
         }

       (* val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val merge : t -> t -> t *)
     end)
    (FSM : sig
       type t =
         { init : EdgeMap.key option
         ; terminals : Partition.elt
         ; alphabet : Labels.t
         ; states : Partition.elt
         ; edges : EdgeMap.t'
         ; info : Info.t
         }

       val json : ?as_elt:bool -> t -> Yojson.t
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val merge : t -> t -> t *)
       (* val is_weak_mode : t -> bool *)

       (* module Saturate :
          module type of
          Algorithm_saturate.Make (Log) (State) (States) (Label) (Labels)
          (Tree)
          (Trees)
          (Note)
          (Annotation)
          (Annotations)
          (Action)
          (ActionPair)
          (ActionPairs)
          (ActionMap)
          (EdgeMap) *)

       val saturate : ?only_if_weak:bool -> t -> t
     end)
    -> sig
  type t =
    { fsm : FSM.t
    ; pi : Partition.t
    }

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit

  exception CannotSplitEmptyBlock of unit

  val ensure_nonempty : States.t -> unit

  val split_block
    :  Partition.t
    -> State.t
    -> EdgeMap.t'
    -> States.t
    -> States.t * States.t option

  exception Split_OnlyReturnedOneBlock_ButNeqBlock of (States.t * States.t)

  val ensure_equal : States.t -> States.t -> unit

  val for_each_label
    :  Partition.t ref
    -> bool ref
    -> EdgeMap.t'
    -> States.t ref
    -> Label.t
    -> unit

  val for_each_block
    :  Partition.t ref
    -> bool ref
    -> Labels.t
    -> EdgeMap.t'
    -> States.t
    -> unit

  val partition_states : FSM.t -> Partition.t
  val fsm : FSM.t -> t
end
