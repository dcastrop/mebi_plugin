module type S = sig
  type states
  type partition
  type fsm

  module FSMPair : sig
    type t =
      { original : fsm
      ; saturated : fsm
      }

    include Json.S with type k = t

    val get : fsm -> t
  end

  module Result : sig
    type t =
      { bisim_states : partition
      ; non_bisim_states : partition
      }

    include Json.S with type k = t

    val are_bisimilar : t -> bool
    val split : partition -> states -> states -> t
  end

  type t =
    { fsm_a : FSMPair.t
    ; fsm_b : FSMPair.t
    ; merged : fsm
    ; result : Result.t
    }

  include Json.S with type k = t

  val the_cached_result : t option ref
  val set_the_result : t -> unit

  exception NoCachedResult of unit

  val get_the_result : unit -> t
  val fsm : fsm -> fsm -> t
end

module Make
    (Log : Logger.S)
    (State : State.S)
    (States : States.S with type elt = State.t)
    (Label : Label.S with type base = State.base)
    (Labels : Labels.S with type elt = Label.t)
    (Action : Action.S with type label = Label.t)
    (ActionMap :
       Actionmap.S with type action = Action.t and type states = States.t)
    (EdgeMap :
       Edgemap.S with type state = State.t and type actionmap = ActionMap.t')
    (Partition :
       State_partition.S with type elt = States.t and type edgemap = EdgeMap.t')
    (Info : Info.S with type base = State.base and type labels = Labels.t)
    (FSM :
       FSM.S
       with type state = State.t
        and type states = States.t
        and type labels = Labels.t
        and type edgemap = EdgeMap.t'
        and type info = Info.t)
    (Minimize :
       Minimize.S
       with type state = State.t
        and type states = States.t
        and type label = Label.t
        and type labels = Labels.t
        and type edgemap = EdgeMap.t'
        and type partition = Partition.t
        and type fsm = FSM.t) :
  S
  with type states = States.t
   and type partition = Partition.t
   and type fsm = FSM.t = struct
  type states = States.t
  type partition = Partition.t
  type fsm = FSM.t

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

  module Result = struct
    type t =
      { bisim_states : Partition.t
      ; non_bisim_states : Partition.t
      }

    include
      Json.Thing.Make
        (Log)
        (struct
          type k = t

          let name = "Result"

          let json ?as_elt (x : t) : Yojson.t =
            `Assoc
              [ "bisimilar states", Partition.json ~as_elt:true x.bisim_states
              ; ( "non-bisimilar states"
                , Partition.json ~as_elt:true x.non_bisim_states )
              ]
          ;;
        end)

    let are_bisimilar ({ non_bisim_states; _ } : t) : bool =
      Log.trace __FUNCTION__;
      Partition.is_empty non_bisim_states
    ;;

    let split (pi : Partition.t) (a : States.t) (b : States.t) : t =
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
  end

  type t =
    { fsm_a : FSMPair.t
    ; fsm_b : FSMPair.t
    ; merged : FSM.t
    ; result : Result.t
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
            ; "result", Result.json ~as_elt:true x.result
            ]
        ;;
      end)

  let the_cached_result : t option ref = ref None
  let set_the_result (x : t) : unit = the_cached_result := Some x

  exception NoCachedResult of unit

  let get_the_result () : t =
    Log.trace __FUNCTION__;
    match !the_cached_result with
    | None -> raise (NoCachedResult ())
    | Some x -> x
  ;;

  let fsm (a : FSM.t) (b : FSM.t) : t =
    Log.trace __FUNCTION__;
    let fsm_a : FSMPair.t = FSMPair.get a in
    let fsm_b : FSMPair.t = FSMPair.get b in
    let merged : FSM.t = FSM.merge fsm_a.saturated fsm_b.saturated in
    let pi : Partition.t = (Minimize.fsm merged).pi in
    let result = Result.split pi fsm_a.original.states fsm_b.original.states in
    { fsm_a; fsm_b; merged; result }
  ;;
end
