module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type base = Base.t)
    (States : States.S with type elt = State.t)
    (Label : Label.S with type base = Base.t)
    (Labels : Labels.S with type elt = Label.t)
    (Action : sig
       type t
     end)
    (ActionMap : sig
       include Hashtbl.S with type key = Action.t

       type t' = States.t t
     end)
    (EdgeMap : sig
       include Hashtbl.S with type key = State.t

       type t' = ActionMap.t' t

       val reduce_by_label : t' -> Label.t -> t'
     end)
    (Partition : sig
       include Set.S with type elt = States.t

       val json : ?as_elt:bool -> t -> Yojson.t

       val log
         :  ?__FUNCTION__:string
         -> ?m:Output.Kind.t
         -> ?s:string
         -> t
         -> unit

       val reachable : State.t -> EdgeMap.t' -> t -> t
     end)
    (Info : sig
       type t
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
       val saturate : ?only_if_weak:bool -> t -> t
     end) : sig
  type t =
    { fsm : FSM.t
    ; pi : Partition.t
    }

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit

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
end = struct
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
    Partition.log ~__FUNCTION__ ~s:"reachable" reachable_from_s;
    States.fold
      (fun (t : State.t) ((b1, b2) : States.t * States.t option) ->
        if State.equal s t
        then States.add s b1, b2
        else (
          let reachable_from_t : Partition.t = Partition.reachable t edges pi in
          (* NOTE: split if [s] and [t] can reach different blocks *)
          if Partition.equal reachable_from_s reachable_from_t
          then States.add t b1, b2
          else b1, Some (States.add_to_opt t b2)))
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
