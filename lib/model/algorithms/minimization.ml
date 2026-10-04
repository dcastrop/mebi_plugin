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

  (** [split_block_by reach s block] splits [block] into the states that
      [reach] maps to the same set of blocks as [s], and the rest. *)
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
        if State.equal s t
        then States.add s b1, b2
        else (
          let reachable_from_t : Partition.t = reach t in
          (* NOTE: split if [s] and [t] can reach different blocks *)
          if Partition.equal reachable_from_s reachable_from_t
          then States.add t b1, b2
          else (
            State.log ~__FUNCTION__ ~s:"splitting" t;
            b1, Some (States.add_to_opt t b2))))
      block
      (States.empty, None)
  ;;

  (* See the [.mli]. {!split_block_by}, with [reach] the blocks of [pi]
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

  (* See the [.mli]. Splits by [block]'s least state, on [edges] restricted
     to [label]; on a split, [pi] gets the two halves and [block] becomes the
     half containing that state. *)
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

  (** [silent_closures edges] is a function from a state to the states it
      reaches by {e zero} or more silent steps in [edges]: Milner's [=ε=>],
      reflexive by definition. Memoised, so each closure is computed once per
      partition. Only the silent edges of [edges] are read, so it can be given
      an unsaturated FSM's edges. *)
  let silent_closures (edges : EdgeMap.t') : State.t -> States.t =
    let memo : States.t StateTbl.t = StateTbl.create 64 in
    (* [bfs frontier seen]: [seen] grown by everything reachable by silent
       steps from [frontier] (whose states are already in [seen]). *)
    let rec bfs (frontier : State.t list) (seen : States.t) : States.t =
      match frontier with
      | [] -> seen
      | s :: rest ->
        (match EdgeMap.find_opt edges s with
         | None -> bfs rest seen
         | Some actions ->
           let frontier, seen =
             ActionMap.fold
               (fun (a : Action.t) (ds : States.t) acc ->
                 if Action.is_silent a
                 then
                   States.fold
                     (fun (d : State.t)
                       ((fr, seen) : State.t list * States.t) ->
                       if States.mem d seen
                       then fr, seen
                       else d :: fr, States.add d seen)
                     ds
                     acc
                 else acc)
               actions
               (rest, seen)
           in
           bfs frontier seen)
    in
    fun (src : State.t) ->
      match StateTbl.find_opt memo src with
      | Some c -> c
      | None ->
        let c : States.t = bfs [ src ] (States.singleton src) in
        StateTbl.add memo src c;
        c
  ;;

  (** The silent half of weak bisimilarity: split [block] by which blocks
      each state reaches by [=ε=>] ([closure]). See [for_each_block]. *)
  let for_silent_closure
        (pi : Partition.t ref)
        (changed : bool ref)
        (closure : State.t -> States.t)
        (block : States.t ref)
    : unit
    =
    Logger.trace __FUNCTION__;
    (* [reach x]: the blocks of [pi] that [x] reaches by [=eps=>] *)
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

  (** Refines [block] once by every visible label of the (saturated) [edges]
      and, given [closure], by [=ε=>] as well.

      Weak bisimilarity on an LTS is strong bisimilarity on its saturation
      with {e both} kinds of weak move: [=a=>] for each visible [a], and
      [=ε=>], zero or more silent steps (Milner 1989, ch. 5). Saturation
      builds only the first, so without [closure] this computes something
      coarser: it cannot tell [τ.a + b] from [a + b]. See
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

  (** [partition_states ?silent fsm] partitions [fsm]'s states by
      bisimilarity over its visible labels. Given [silent] -- edges holding
      the silent steps, normally the {e unsaturated} FSM's -- it also splits
      by [=ε=>], which is what makes the result weak bisimilarity when [fsm]
      is saturated. *)
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
