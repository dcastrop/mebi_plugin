module type S = sig
  type state
  type states
  type label
  type labels
  type edgemap
  type partition
  type fsm

  type t =
    { fsm : fsm
    ; pi : partition
    }

  include Json.S with type k = t

  exception CannotSplitEmptyBlock of unit

  val ensure_nonempty : states -> unit

  val split_block
    :  partition
    -> state
    -> edgemap
    -> states
    -> states * states option

  exception Split_OnlyReturnedOneBlock_ButNeqBlock of (states * states)

  val ensure_equal : states -> states -> unit

  val for_each_label
    :  partition ref
    -> bool ref
    -> edgemap
    -> states ref
    -> label
    -> unit

  val for_each_block
    :  partition ref
    -> bool ref
    -> labels
    -> edgemap
    -> states
    -> unit

  val partition_states : fsm -> partition
  val fsm : fsm -> t
end

module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type base = Base.t)
    (States : States.S with type elt = State.t)
    (Label : Label.S with type base = Base.t)
    (Labels : Labels.S with type elt = Label.t)
    (Action : Action.S with type label = Label.t)
    (ActionMap :
       Actionmap.S with type action = Action.t and type states = States.t)
    (EdgeMap :
       Edgemap.S
       with type state = State.t
        and type actionmap = ActionMap.t'
        and type label = Label.t)
    (Partition :
       State_partition.S
       with type elt = States.t
        and type edgemap = EdgeMap.t'
        and type state = State.t)
    (Info : Info.S with type base = State.base and type labels = Labels.t)
    (FSM :
       FSM.S
       with type state = State.t
        and type states = States.t
        and type labels = Labels.t
        and type edgemap = EdgeMap.t'
        and type info = Info.t) :
  S
  with type state = State.t
   and type states = States.t
   and type label = Label.t
   and type labels = Labels.t
   and type edgemap = EdgeMap.t'
   and type partition = Partition.t
   and type fsm = FSM.t = struct
  type state = State.t
  type states = States.t
  type label = Label.t
  type labels = Labels.t
  type edgemap = EdgeMap.t'
  type partition = Partition.t
  type fsm = FSM.t

  type t =
    { fsm : FSM.t
    ; pi : Partition.t
    }

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "Minimization Results"

        let json ?as_elt (x : t) : Yojson.t =
          `Assoc
            [ "fsm", FSM.json ~as_elt:true x.fsm
            ; "pi", Partition.json ~as_elt:true x.pi
            ]
        ;;
      end)

  exception CannotSplitEmptyBlock of unit

  let ensure_nonempty (a : States.t) : unit =
    Log.trace __FUNCTION__;
    try assert (States.is_empty a |> Bool.not) with
    | Assert_failure _ -> raise (CannotSplitEmptyBlock ())
  ;;

  let split_block
        (pi : Partition.t)
        (s : State.t)
        (edges : EdgeMap.t')
        (block : States.t)
    : States.t * States.t option
    =
    Log.trace __FUNCTION__;
    ensure_nonempty block;
    let reachable_from_s : Partition.t = Partition.reachable s edges pi in
    Partition.log ~__FUNCTION__ ~s:"reachable from state" reachable_from_s;
    States.fold
      (fun (t : State.t) ((b1, b2) : States.t * States.t option) ->
        if State.equal s t
        then States.add s b1, b2
        else (
          let reachable_from_t : Partition.t = Partition.reachable t edges pi in
          (* NOTE: split if [s] and [t] can reach different blocks *)
          if Partition.equal reachable_from_s reachable_from_t
          then States.add t b1, b2
          else (
            State.log ~__FUNCTION__ ~s:"splitting" t;
            b1, Some (States.add_to_opt t b2))))
      block
      (States.empty, None)
  ;;

  exception Split_OnlyReturnedOneBlock_ButNeqBlock of (States.t * States.t)

  let ensure_equal (a : States.t) (b : States.t) : unit =
    Log.trace __FUNCTION__;
    try assert (States.equal a b) with
    | Assert_failure _ -> raise (Split_OnlyReturnedOneBlock_ButNeqBlock (a, b))
  ;;

  let for_each_label
        (pi : Partition.t ref)
        (changed : bool ref)
        (edges : EdgeMap.t')
        (block : States.t ref)
        (label : Label.t)
    : unit
    =
    Log.trace __FUNCTION__;
    Partition.log ~__FUNCTION__ ~s:"pi" !pi;
    Label.log ~__FUNCTION__ ~s:"split by label" label;
    let edges : EdgeMap.t' = EdgeMap.reduce_by_label edges label in
    (* NOTE: select some state [s] from [block] *)
    let s : State.t = States.min_elt !block in
    State.log ~__FUNCTION__ ~s:"split from state" s;
    match split_block !pi s edges !block with
    | a, None -> ensure_equal a !block
    | a, Some b ->
      pi := Partition.remove !block !pi |> Partition.add a |> Partition.add b;
      block := a;
      changed := true
  ;;

  let for_each_block
        (pi : Partition.t ref)
        (changed : bool ref)
        (alphabet : Labels.t)
        (edges : EdgeMap.t')
        (block : States.t)
    : unit
    =
    Log.trace __FUNCTION__;
    Labels.non_silent alphabet
    |> Labels.iter (for_each_label pi changed edges (ref block))
  ;;

  let partition_states (fsm : FSM.t) : Partition.t =
    Log.trace __FUNCTION__;
    let pi : Partition.t ref = ref (Partition.singleton fsm.states) in
    let changed : bool ref = ref true in
    while !changed do
      changed := false;
      Partition.iter (for_each_block pi changed fsm.alphabet fsm.edges) !pi
    done;
    !pi
  ;;

  let fsm (fsm : FSM.t) : t =
    Log.trace __FUNCTION__;
    { fsm; pi = FSM.saturate ~only_if_weak:true fsm |> partition_states }
  ;;
end
