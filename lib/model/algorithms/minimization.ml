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

  val split_block_by
    :  (state -> partition)
    -> state
    -> states
    -> states * states option

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

  val silent_closures : edgemap -> state -> states

  val for_each_block
    :  ?closure:(state -> states)
    -> partition ref
    -> bool ref
    -> labels
    -> edgemap
    -> states
    -> unit

  val partition_states : ?silent:edgemap -> fsm -> partition
  val fsm : fsm -> t
end

module Make
    (C : Components.S)
    (FSM :
       FSM.S
       with type state = C.State.t
        and type states = C.State.Set.t
        and type labels = C.Label.Set.t
        and type edgemap = C.EdgeMap.t'
        and type info = C.Info.t) :
  S
  with type state = C.State.t
   and type states = C.State.Set.t
   and type label = C.Label.t
   and type labels = C.Label.Set.t
   and type edgemap = C.EdgeMap.t'
   and type partition = C.Partition.t
   and type fsm = FSM.t = struct
  module State = C.State
  module States = C.State.Set
  module Label = C.Label
  module Labels = C.Label.Set
  module EdgeMap = C.EdgeMap
  module Partition = C.Partition
  module Action = C.Action
  module ActionMap = C.Action.Map
  module StateTbl = Hashtbl.Make (C.State)

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

  include Json.Thing.Make (struct
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

  (* See the [.mli]. *)
  let ensure_nonempty (a : States.t) : unit =
    Logger.trace __FUNCTION__;
    try assert (States.is_empty a |> Bool.not) with
    | Assert_failure _ -> raise (CannotSplitEmptyBlock ())
  ;;

  (** [goes_with reach s reachable_from_s t] is whether [t] stays in [s]'s
      block: [t] is [s], or [reach t] is the same set of blocks as [s]'s,
      [reachable_from_s]. Raises nothing directly; propagates whatever
      [reach] raises. *)
  let goes_with
        (reach : State.t -> Partition.t)
        (s : State.t)
        (reachable_from_s : Partition.t)
        (t : State.t)
    : bool
    =
    State.equal s t || Partition.equal reachable_from_s (reach t)
  ;;

  (* See the [.mli]. One pass over [block], each state kept with [s] or
     split off by {!goes_with}. *)
  let split_block_by
        (reach : State.t -> Partition.t)
        (s : State.t)
        (block : States.t)
    : States.t * States.t option
    =
    Logger.trace __FUNCTION__;
    ensure_nonempty block;
    let reachable_from_s : Partition.t = reach s in
    Partition.log ~__FUNCTION__ ~s:"reachable from state" reachable_from_s;
    States.fold
      (fun (t : State.t) ((b1, b2) : States.t * States.t option) ->
        if goes_with reach s reachable_from_s t
        then States.add t b1, b2
        else (
          State.log ~__FUNCTION__ ~s:"splitting" t;
          b1, Some (States.add_to_opt t b2)))
      block
      (States.empty, None)
  ;;

  (* See the [.mli]: {!split_block_by}, with [reach] the blocks of [pi]
     each state reaches by one step of [edges]. *)
  let split_block
        (pi : Partition.t)
        (s : State.t)
        (edges : EdgeMap.t')
        (block : States.t)
    : States.t * States.t option
    =
    split_block_by (fun (x : State.t) -> Partition.reachable x edges pi) s block
  ;;

  exception Split_OnlyReturnedOneBlock_ButNeqBlock of (States.t * States.t)

  (* See the [.mli]. *)
  let ensure_equal (a : States.t) (b : States.t) : unit =
    Logger.trace __FUNCTION__;
    try assert (States.equal a b) with
    | Assert_failure _ -> raise (Split_OnlyReturnedOneBlock_ButNeqBlock (a, b))
  ;;

  (* See the [.mli]. *)
  let for_each_label
        (pi : Partition.t ref)
        (changed : bool ref)
        (edges : EdgeMap.t')
        (block : States.t ref)
        (label : Label.t)
    : unit
    =
    Logger.trace __FUNCTION__;
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

  (** [silent_successors edges s] is the destination of every silent step
      out of [s] in [edges], in storage order (repeats possible). Raises
      nothing. *)
  let silent_successors (edges : EdgeMap.t') (s : State.t) : State.t list =
    match EdgeMap.find_opt edges s with
    | None -> []
    | Some actions ->
      ActionMap.fold
        (fun (a : Action.t) (ds : States.t) acc ->
          if Action.is_silent a
          then States.fold (fun (d : State.t) acc -> d :: acc) ds acc
          else acc)
        actions
        []
      |> List.rev
  ;;

  (* See the [.mli]. A breadth-first search over silent steps
     ({!silent_successors}) per state, memoised by state. *)
  let silent_closures (edges : EdgeMap.t') : State.t -> States.t =
    let memo : States.t StateTbl.t = StateTbl.create 64 in
    (* [bfs frontier seen] is [seen] grown by everything reachable by silent
       steps from [frontier] (whose states are already in [seen]); a state's
       new successors go to the front of the frontier. *)
    let rec bfs (frontier : State.t list) (seen : States.t) : States.t =
      match frontier with
      | [] -> seen
      | s :: rest ->
        let frontier, seen =
          List.fold_left
            (fun ((fr, seen) : State.t list * States.t) (d : State.t) ->
              if States.mem d seen then fr, seen else d :: fr, States.add d seen)
            (rest, seen)
            (silent_successors edges s)
        in
        bfs frontier seen
    in
    fun (src : State.t) ->
      match StateTbl.find_opt memo src with
      | Some c -> c
      | None ->
        let c : States.t = bfs [ src ] (States.singleton src) in
        StateTbl.add memo src c;
        c
  ;;

  (** [for_silent_closure pi changed closure block] refines [block] once by
      [=ε=>]: it splits [block] by the blocks each state reaches by
      [closure], updating [pi], [block] and [changed] as {!for_each_label}
      does. The silent half of weak bisimilarity; see {!for_each_block}.

      @raise Not_found if [block] is empty (propagated from
                       [States.min_elt]).
      @raise Split_OnlyReturnedOneBlock_ButNeqBlock
        as {!for_each_label}
        (propagated from {!ensure_equal}). *)
  let for_silent_closure
        (pi : Partition.t ref)
        (changed : bool ref)
        (closure : State.t -> States.t)
        (block : States.t ref)
    : unit
    =
    Logger.trace __FUNCTION__;
    (* [reach x] is the blocks of [pi] that [x] reaches by [=eps=>] *)
    let reach (x : State.t) : Partition.t =
      Partition.filter_reachable (closure x) !pi
    in
    match split_block_by reach (States.min_elt !block) !block with
    | a, None -> ensure_equal a !block
    | a, Some b ->
      pi := Partition.remove !block !pi |> Partition.add a |> Partition.add b;
      block := a;
      changed := true
  ;;

  (* See the [.mli]. Weak bisimilarity on an LTS is strong bisimilarity on
     its saturation with both kinds of weak move: [=a=>] for each visible
     [a], and [=ε=>] (Milner 1989, ch. 5). Saturation builds only the
     first, so without [closure] the result is coarser. See
     [ASSISTED-CHANGES.md], 2026-10-02 (second session). *)
  let for_each_block
        ?(closure : (State.t -> States.t) option)
        (pi : Partition.t ref)
        (changed : bool ref)
        (alphabet : Labels.t)
        (edges : EdgeMap.t')
        (block : States.t)
    : unit
    =
    Logger.trace __FUNCTION__;
    let block : States.t ref = ref block in
    Labels.non_silent alphabet
    |> Labels.iter (for_each_label pi changed edges block);
    Option.iter
      (fun (closure : State.t -> States.t) ->
        for_silent_closure pi changed closure block)
      closure
  ;;

  (* See the [.mli]. Each round refines every block; rounds repeat while
     any block split. *)
  let partition_states ?(silent : EdgeMap.t' option) (fsm : FSM.t) : Partition.t
    =
    Logger.trace __FUNCTION__;
    let closure : (State.t -> States.t) option =
      Option.map silent_closures silent
    in
    let pi : Partition.t ref = ref (Partition.singleton fsm.states) in
    let changed : bool ref = ref true in
    while !changed do
      changed := false;
      Partition.iter
        (for_each_block ?closure pi changed fsm.alphabet fsm.edges)
        !pi
    done;
    !pi
  ;;

  (* See the [.mli]. *)
  let fsm (fsm : FSM.t) : t =
    Logger.trace __FUNCTION__;
    let silent : EdgeMap.t' option =
      if FSM.is_weak_mode fsm then Some fsm.edges else None
    in
    { fsm
    ; pi = FSM.saturate ~only_if_weak:true fsm |> partition_states ?silent
    }
  ;;
end
