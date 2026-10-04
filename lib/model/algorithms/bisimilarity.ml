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

    val get : ?on_demand:int -> fsm -> t
  end

  module Result : sig
    type t =
      { bisim_states : partition
      ; non_bisim_states : partition
      ; roots_related : bool option
      }

    include Json.S with type k = t

    val are_bisimilar : t -> bool
    val split : ?roots_related:bool -> partition -> states -> states -> t
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

  type on_demand =
    { a : bool
    ; b : bool
    ; budget : int
    ; partition : fsm -> partition
    }

  val fsm : ?on_demand:on_demand -> fsm -> fsm -> t

  (** [conflicts a b] are the states [a] and [b] share (the same term,
      hence the same encoding) whose moves differ between the two: labels
      or targets. {!fsm} merges [a] and [b] assuming a shared state is one
      state, which is exact when both sides use the same relation (so the
      same term has the same moves) and wrong otherwise: two relations over
      [nat], both from [0], conflate their [0]s. Empty in every checked-in
      example. Found 2026-10-03 (notes/13). *)
  val conflicts : fsm -> fsm -> states
end

module Make
    (C : Components.S)
    (FSM :
       FSM.S
       with type state = C.State.t
        and type states = C.State.Set.t
        and type labels = C.Label.Set.t
        and type edgemap = C.EdgeMap.t'
        and type info = C.Info.t)
    (Minimization :
       Minimization.S
       with type state = C.State.t
        and type states = C.State.Set.t
        and type label = C.Label.t
        and type labels = C.Label.Set.t
        and type edgemap = C.EdgeMap.t'
        and type partition = C.Partition.t
        and type fsm = FSM.t) :
  S
  with type states = C.State.Set.t
   and type partition = C.Partition.t
   and type fsm = FSM.t = struct
  module States = C.State.Set
  module Partition = C.Partition

  type states = States.t
  type partition = Partition.t
  type fsm = FSM.t

  module FSMPair = struct
    type t =
      { original : FSM.t
      ; saturated : FSM.t
      }

    include Json.Thing.Make (struct
        type k = t

        let name = "FSM Pair"

        let json ?as_elt (x : t) : Yojson.t =
          `Assoc
            [ "original", FSM.json ~as_elt:true x.original
            ; "saturated", FSM.json ~as_elt:true x.saturated
            ]
        ;;
      end)

    (* See the [.mli]. With [on_demand], saturated on demand within that
       budget of weak actions instead of whole. *)
    let get ?(on_demand : int option) (x : FSM.t) : t =
      match on_demand with
      | None -> { original = x; saturated = FSM.saturate ~only_if_weak:true x }
      | Some budget ->
        { original = x; saturated = FSM.saturate_on_demand ~budget x }
    ;;
  end

  type on_demand =
    { a : bool
    ; b : bool
    ; budget : int
    ; partition : fsm -> partition
    }

  module Result = struct
    type t =
      { bisim_states : Partition.t
      ; non_bisim_states : Partition.t
      ; roots_related : bool option
      }

    include Json.Thing.Make (struct
        type k = t

        let name = "Result"

        let json ?as_elt (x : t) : Yojson.t =
          `Assoc
            [ "bisimilar states", Partition.json ~as_elt:true x.bisim_states
            ; ( "non-bisimilar states"
              , Partition.json ~as_elt:true x.non_bisim_states )
            ; ( "initial states related"
              , match x.roots_related with None -> `Null | Some b -> `Bool b )
            ]
        ;;
      end)

    (* Bisimilarity of two systems is bisimilarity of their initial states.
       "Every block holds states of both systems" is not it: [a.b.x] against
       [b.a.y] partitions into two shared blocks, {x, b.y} and {b.x, y}, with
       the two initial states in different ones. It survives only as the
       fallback when an FSM has no initial state. *)
    let are_bisimilar ({ non_bisim_states; roots_related; _ } : t) : bool =
      Logger.trace __FUNCTION__;
      match roots_related with
      | Some related -> related
      | None -> Partition.is_empty non_bisim_states
    ;;

    (* See the [.mli]. A block goes to [bisim_states] if it has a state of
       [a] and a state of [b] ([States.has_shared_origin]). *)
    let split
          ?(roots_related : bool option)
          (pi : Partition.t)
          (a : States.t)
          (b : States.t)
      : t
      =
      Logger.trace __FUNCTION__;
      let bisim_states, non_bisim_states =
        Partition.fold
          (fun (x : States.t) (bisim_states, non_bisim_states) ->
            if States.has_shared_origin x a b
            then Partition.add x bisim_states, non_bisim_states
            else bisim_states, Partition.add x non_bisim_states)
          pi
          (Partition.empty, Partition.empty)
      in
      { bisim_states; non_bisim_states; roots_related }
    ;;
  end

  type t =
    { fsm_a : FSMPair.t
    ; fsm_b : FSMPair.t
    ; merged : FSM.t
    ; result : Result.t
    }

  include Json.Thing.Make (struct
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

  (* See the [.mli] for these three. *)
  let the_cached_result : t option ref = ref None
  let set_the_result (x : t) : unit = the_cached_result := Some x

  exception NoCachedResult of unit

  let get_the_result () : t =
    Logger.trace __FUNCTION__;
    match !the_cached_result with
    | None -> raise (NoCachedResult ())
    | Some x -> x
  ;;

  (** A state's moves, as (label, target) pairs, for {!conflicts}. *)
  module Move = Set.Make (struct
      type t = C.Label.t * C.State.t

      let compare ((l, s) : t) ((l', s') : t) : int =
        match C.Label.compare l l' with 0 -> C.State.compare s s' | n -> n
      ;;
    end)

  (* See the [.mli]. Compares, for each shared state, its set of
     (label, target) moves in [a] and in [b]. *)
  let conflicts (a : FSM.t) (b : FSM.t) : States.t =
    Logger.trace __FUNCTION__;
    (* [moves x s]: [s]'s (label, target) moves in [x]. *)
    let moves (x : FSM.t) (s : C.State.t) : Move.t =
      match C.EdgeMap.find_opt x.edges s with
      | None -> Move.empty
      | Some actions ->
        C.Action.Map.fold
          (fun (act : C.Action.t) (ds : States.t) acc ->
            States.fold (fun d acc -> Move.add (act.label, d) acc) ds acc)
          actions
          Move.empty
    in
    States.filter
      (fun s -> Bool.not (Move.equal (moves a s) (moves b s)))
      (States.inter a.states b.states)
  ;;

  (** Whether the roots share a block, and the result split by [pi]. *)
  let finish
        (fsm_a : FSMPair.t)
        (fsm_b : FSMPair.t)
        (merged : FSM.t)
        (pi : Partition.t)
    : t
    =
    let roots_related : bool option =
      match fsm_a.original.init, fsm_b.original.init with
      | Some x, Some y ->
        Some
          (States.mem
             y
             (try Partition.get_bisimilar x pi with Not_found -> States.empty))
      | _ -> None
    in
    let result =
      Result.split ?roots_related pi fsm_a.original.states fsm_b.original.states
    in
    { fsm_a; fsm_b; merged; result }
  ;;

  (** [fsm_whole a b]: {!fsm} without on-demand saturation: both saturated
      whole and merged, the merge partitioned with the [=eps=>] split read
      from the originals' silent steps. *)
  let fsm_whole (a : FSM.t) (b : FSM.t) : t =
    let fsm_a : FSMPair.t = FSMPair.get a in
    let fsm_b : FSMPair.t = FSMPair.get b in
    let merged : FSM.t = FSM.merge fsm_a.saturated fsm_b.saturated in
    (* [merged] is already saturated, so partition it as it is.
       [Minimization.fsm] would saturate it again -- a pass that rebuilds
       every weak action of both FSMs (the merged FSM has no silent edges
       left, so it reproduces the same structure, and the partition only
       reads that), roughly doubling the check's saturation memory. *)
    (* The silent steps live only in the originals: saturation drops them.
       [partition_states] needs them for the [=ε=>] split, without which this
       is not weak bisimilarity (see [Minimization.for_each_block]). *)
    let silent : C.EdgeMap.t' option =
      if FSM.is_weak_mode fsm_a.original || FSM.is_weak_mode fsm_b.original
      then Some (FSM.merge fsm_a.original fsm_b.original).edges
      else None
    in
    let pi : Partition.t = Minimization.partition_states ?silent merged in
    finish fsm_a fsm_b merged pi
  ;;

  (* See the [.mli]: {!fsm_whole}, or, with either FSM on demand, the
     quotient route. *)
  let fsm ?(on_demand : on_demand option) (a : FSM.t) (b : FSM.t) : t =
    Logger.trace __FUNCTION__;
    match on_demand with
    | Some ({ a = od_a; b = od_b; budget; partition } : on_demand)
      when od_a || od_b ->
      (* [x] as given and saturated: on demand if [od], else whole *)
      let get (od : bool) (x : FSM.t) : FSMPair.t =
        FSMPair.get ?on_demand:(if od then Some budget else None) x
      in
      let fsm_a : FSMPair.t = get od_a a in
      let fsm_b : FSMPair.t = get od_b b in
      (* nothing saturated whole to merge: the originals, partitioned on
         their silent-SCC quotient *)
      let merged : FSM.t = FSM.merge a b in
      finish fsm_a fsm_b merged (partition merged)
    | _ -> fsm_whole a b
  ;;
end
