module Make
    (Log : Logger.S)
    (State : sig
       type t

       (* val json : ?as_elt:bool -> t -> Yojson.t
          val to_string : ?pretty:bool -> t -> string
          val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
          val equal : t -> t -> bool
          val compare : t -> t -> int
          val hash : t -> int *)
     end)
    (States : sig
       include Set.S with type elt = State.t

       (* val json : ?as_elt:bool -> t -> Yojson.t
          val to_string : ?pretty:bool -> t -> string
          val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
          val add_to_opt : State.t -> t option -> t *)

       exception StateHasNoOrigin of (State.t * t * t)

       (* val origin_of_state : State.t -> t -> t -> int *)
       val has_shared_origin : t -> t -> t -> bool
     end)
    (Label : sig
       type t

       (* val json : ?as_elt:bool -> t -> Yojson.t
          val to_string : ?pretty:bool -> t -> string
          val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
          val equal : t -> t -> bool
          val compare : t -> t -> int
          val hash : t -> int
          val is_silent : t -> bool *)
     end)
    (Labels : sig
       include Set.S with type elt = Label.t

       (* val json : ?as_elt:bool -> t -> Yojson.t
          val to_string : ?pretty:bool -> t -> string
          val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
          val non_silent : t -> t *)
     end)
    (Action : sig
       type t
       (* =
         { label : Label.t
         ; annotation : Annotation.t option
         ; constructor_trees : Trees.t
         } *)

       (* val json : ?as_elt:bool -> t -> Yojson.t
          val to_string : ?pretty:bool -> t -> string
          val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
          val equal : t -> t -> bool
          val compare : t -> t -> int
          val hash : t -> int
          val wk_equal : t -> t -> bool
          val is_silent : t -> bool
          val is_labelled : Label.t -> t -> bool
          val shorter_annotation : t -> t -> t *)
     end)
    (ActionMap : sig
       include Hashtbl.S with type key = Action.t

       type t' = States.t t

       (* val json : ?as_elt:bool -> t' -> Yojson.t
          val to_string : ?pretty:bool -> t' -> string
          val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t' -> unit
          val update : t' -> Action.t -> States.t -> unit
          val destinations : t' -> States.t
          val reduce_by_label : t' -> Label.t -> t'
          val to_actions : t' -> Actions.t
          val to_actionpairs : t' -> ActionPairs.t
          val of_actionpairs : ActionPairs.t -> t'
          val merge : t' -> t' -> t' *)
     end)
    (EdgeMap : sig
       include Hashtbl.S with type key = State.t

       type t' = ActionMap.t' t

       (* val json : ?as_elt:bool -> t' -> Yojson.t
          val to_string : ?pretty:bool -> t' -> string
          val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t' -> unit
          val update : t' -> State.t -> Action.t -> States.t -> unit
          val destinations : t' -> State.t -> States.t
          val get_actions : t' -> State.t -> Actions.t
          val reduce_by_label : t' -> Label.t -> t'
          val get_edges : t' -> State.t -> Edges.t
          val to_edges : t' -> Edges.t
          val of_edges : Edges.t -> t'
          val merge : t' -> t' -> t' *)
     end)
    (Partition : sig
       include Set.S with type elt = States.t

       val json : ?as_elt:bool -> t -> Yojson.t
       (* val to_string : ?pretty:bool -> t -> string
          val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
          val get_bisimilar : State.t -> t -> States.t
          val filter_reachable : States.t -> t -> t
          val reachable : State.t -> EdgeMap.t' -> t -> t
          val reachable_by_label : State.t -> Label.t -> EdgeMap.t' -> t -> t *)
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
              val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit *)
         end

         module RocqLTS : sig
           type t

           (* val json : ?as_elt:bool -> t -> Yojson.t
              val to_string : ?pretty:bool -> t -> string
              val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit *)
         end

         type t =
           { is_complete : bool
           ; is_merged : bool
           ; bounds : Bounds.t
           ; lts : RocqLTS.t list
           }

         (* val json : ?as_elt:bool -> t -> Yojson.t
            val to_string : ?pretty:bool -> t -> string
            val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit *)
       end

       type t =
         { meta : Meta.t option
         ; weak_labels : Labels.t
         }

       (* val json : ?as_elt:bool -> t -> Yojson.t
          val to_string : ?pretty:bool -> t -> string
          val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
          val merge : t -> t -> t *)
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
       (* val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit *)
       val merge : t -> t -> t
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
    (Minimize : sig
       type t =
         { fsm : FSM.t
         ; pi : Partition.t
         }

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit *)

       exception CannotSplitEmptyBlock of unit

       (* val ensure_nonempty : States.t -> unit *)

       (* val split_block
          :  Partition.t
          -> State.t
          -> EdgeMap.t'
          -> States.t
          -> States.t * States.t option *)

       exception Split_OnlyReturnedOneBlock_ButNeqBlock of (States.t * States.t)

       (* val ensure_equal : States.t -> States.t -> unit

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
          -> unit *)

       (* val partition_states : FSM.t -> Partition.t *)
       val fsm : FSM.t -> t
     end) =
struct
  module FSMPair = struct
    type t =
      { original : FSM.t
      ; saturated : FSM.t
      }

    include
      Json.Thing.Make
        (Log)
        (struct
          type k = t

          let name = "FSM Pair"

          let json ?as_elt (x : t) : Yojson.t =
            `Assoc
              [ "original", FSM.json ~as_elt:true x.original
              ; "saturated", FSM.json ~as_elt:true x.saturated
              ]
          ;;
        end)

    let get (x : FSM.t) : t =
      { original = x; saturated = FSM.saturate ~only_if_weak:true x }
    ;;
  end

  type t =
    { fsm_a : FSMPair.t
    ; fsm_b : FSMPair.t
    ; merged : FSM.t
    ; result : result
    }

  and result =
    { bisim_states : Partition.t
    ; non_bisim_states : Partition.t
    }

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "Bisimilarity Results"

        let json ?as_elt (x : t) : Yojson.t =
          `Assoc
            [ ( "fsms"
              , `Assoc
                  [ "a", FSMPair.json ~as_elt:true x.fsm_a
                  ; "b", FSMPair.json ~as_elt:true x.fsm_b
                  ; "merged", FSM.json ~as_elt:true x.merged
                  ] )
            ; ( "bisimilar states"
              , Partition.json ~as_elt:true x.result.bisim_states )
            ; ( "non-bisimilar states"
              , Partition.json ~as_elt:true x.result.non_bisim_states )
            ]
        ;;
      end)

  let are_bisimilar ({ non_bisim_states; _ } : result) : bool =
    Log.trace __FUNCTION__;
    Partition.is_empty non_bisim_states
  ;;

  let the_cached_result : t option ref = ref None
  let set_the_result (x : t) : unit = the_cached_result := Some x

  exception NoCachedResult of unit

  let get_the_result () : t =
    Log.trace __FUNCTION__;
    match !the_cached_result with
    | None -> raise (NoCachedResult ())
    | Some x -> x
  ;;

  let split (pi : Partition.t) (a : States.t) (b : States.t) : result =
    Log.trace __FUNCTION__;
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

  let fsm (a : FSM.t) (b : FSM.t) : t =
    Log.trace __FUNCTION__;
    let fsm_a : FSMPair.t = FSMPair.get a in
    let fsm_b : FSMPair.t = FSMPair.get b in
    let merged : FSM.t = FSM.merge fsm_a.saturated fsm_b.saturated in
    let pi : Partition.t = (Minimize.fsm merged).pi in
    let result = split pi fsm_a.original.states fsm_b.original.states in
    { fsm_a; fsm_b; merged; result }
  ;;
end
