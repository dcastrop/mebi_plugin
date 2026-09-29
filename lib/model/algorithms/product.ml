module type S = sig
  type state
  type states
  type label
  type transition
  type fsm
  type partition

  module Pair : sig
    type t = state * state

    val compare : t -> t -> int
    val equal : t -> t -> bool

    module Set : Set.S with type elt = t
  end

  exception
    NoBisimilarResponse of
      { from : state
      ; label : label
      }

  val respond : fsm -> state -> label -> states -> transition
  val successors : fsm -> fsm -> partition -> Pair.t -> Pair.t list
  val reachable : fsm -> fsm -> partition -> Pair.t -> Pair.Set.t

  type cost =
    { pairs : int
    ; moves : int
    ; nested : int option
    }

  val estimate : ?cap_factor:int -> fsm -> fsm -> partition -> Pair.t -> cost
  val prefer_mutual : cost -> bool
end

module Make
    (Base : Base_term.S)
    (C : Components.S with type tree = Base.Tree.t and type trees = Base.Trees.t)
    (FSM :
       FSM.S
       with type state = C.State.t
        and type states = C.State.Set.t
        and type labels = C.Label.Set.t
        and type edgemap = C.EdgeMap.t'
        and type info = C.Info.t) =
struct
  type state = C.State.t
  type states = C.State.Set.t
  type label = C.Label.t
  type transition = C.Transition.t
  type fsm = FSM.t
  type partition = C.Partition.t

  module Pair = struct
    type t = C.State.t * C.State.t

    let compare ((a, b) : t) ((x, y) : t) : int =
      match C.State.compare a x with 0 -> C.State.compare b y | n -> n
    ;;

    let equal (a : t) (b : t) : bool = Int.equal (compare a b) 0

    module Set = Set.Make (struct
        type nonrec t = t

        let compare = compare
      end)
  end

  exception
    NoBisimilarResponse of
      { from : state
      ; label : label
      }

  (* Lifted verbatim out of [Proof_solver_step.try_get_visible_transition].
     The two lines that used to precede it there resolved [from] and [label]
     out of the Rocq goal; everything below is, and always was, pure model
     code. Keep it that way -- the caller resolves, this decides. *)
  let respond
        (m : FSM.t)
        (from : C.State.t)
        (label : C.Label.t)
        (bisimilar : C.State.Set.t)
    : C.Transition.t
    =
    Logger.trace __FUNCTION__;
    try
      let ({ annotation; trees; _ }, destinations) : C.Action.Pair.t =
        (* NOTE: get actions [from] with [label] *)
        C.Action.Map.reduce_by_label (C.EdgeMap.find m.edges from) label
        |> C.Action.Map.to_actionpairs
        (* NOTE: keep only those that are [bisimilar] *)
        |> C.Action.Pair.Set.filter_map (fun ((x, y) : C.Action.Pair.t) ->
          if C.State.Set.disjoint bisimilar y
          then None
          else Some (x, C.State.Set.inter bisimilar y))
        (* NOTE: get the pair with the shortest annotation (less steps to do) *)
        |> C.Action.Pair.Set.shortest_annotation
      in
      let tree : Base.Tree.t option = Base.Trees.min_opt trees in
      let goto : C.State.t = C.State.Set.min_elt destinations in
      { from; goto; label; annotation; tree }
    with
    | C.Action.Pair.Set.IsEmpty -> raise (NoBisimilarResponse { from; label })
  ;;

  let bisimilar_with (pi : C.Partition.t) (x : C.State.t) : C.State.Set.t =
    try C.Partition.get_bisimilar x pi with Not_found -> C.State.Set.empty
  ;;

  (* The obligations on the left-hand system: one per transition out of [x].
     Read from the UNSATURATED fsm, because that is what the proof solver sees
     -- its hypothesis comes from inverting the LTS relation itself. *)
  let obligations (a : FSM.t) (x : C.State.t) : (C.Label.t * C.State.t) list =
    match C.EdgeMap.find_opt a.edges x with
    | None -> []
    | Some actions ->
      C.Action.Pair.Set.fold
        (fun ((action, destinations) : C.Action.Pair.t) acc ->
          C.State.Set.fold
            (fun (d : C.State.t) acc -> (action.label, d) :: acc)
            destinations
            acc)
        (C.Action.Map.to_actionpairs actions)
        []
  ;;

  let successors (a : FSM.t) (b : FSM.t) (pi : C.Partition.t) ((x, y) : Pair.t)
    : Pair.t list
    =
    Logger.trace __FUNCTION__;
    List.filter_map
      (fun ((label, x') : C.Label.t * C.State.t) ->
        let bisimilar : C.State.Set.t = bisimilar_with pi x' in
        (* Mirrors [Proof_solver_step.handle_wk_concl]: a silent move to
           somewhere already bisimilar to [y] is answered by standing still,
           and everything else goes through [respond]. *)
        if C.Label.is_silent label && C.State.Set.mem y bisimilar
        then Some (x', y)
        else (
          match respond b y label bisimilar with
          | t -> Some (x', t.goto)
          | exception NoBisimilarResponse _ -> None))
      (obligations a x)
  ;;

  let reachable (a : FSM.t) (b : FSM.t) (pi : C.Partition.t) (root : Pair.t)
    : Pair.Set.t
    =
    Logger.trace __FUNCTION__;
    let rec go (seen : Pair.Set.t) : Pair.t list -> Pair.Set.t = function
      | [] -> seen
      | p :: rest ->
        let next : Pair.t list =
          successors a b pi p
          |> List.filter (fun q -> not (Pair.Set.mem q seen))
        in
        go
          (List.fold_left (fun acc q -> Pair.Set.add q acc) seen next)
          (List.rev_append next rest)
    in
    go (Pair.Set.singleton root) [ root ]
  ;;

  type cost =
    { pairs : int
    ; moves : int
    ; nested : int option
    }

  exception Capped

  let estimate
        ?(cap_factor : int = 4)
        (a : FSM.t)
        (b : FSM.t)
        (pi : C.Partition.t)
        (root : Pair.t)
    : cost
    =
    Logger.trace __FUNCTION__;
    let pairs : Pair.Set.t = reachable a b pi root in
    let moves : int =
      Pair.Set.fold
        (fun p acc -> acc + List.length (successors a b pi p))
        pairs
        0
    in
    (* The nested walk, simulated. [path] is the set of coinduction hypotheses
       a nested cofix would have in scope at this point -- the ancestors, and
       only the ancestors. Meeting one of them closes the goal; meeting any
       other already-proved pair does not, and the whole subtree below it is
       walked again. *)
    let cap : int = cap_factor * (Pair.Set.cardinal pairs + moves) in
    let seen : int ref = ref 0 in
    let rec walk (path : Pair.Set.t) (p : Pair.t) : unit =
      incr seen;
      if !seen > cap then raise Capped;
      if Pair.Set.mem p path
      then () (* closes against an ancestor *)
      else (
        let path = Pair.Set.add p path in
        List.iter (walk path) (successors a b pi p))
    in
    let nested : int option =
      try
        walk Pair.Set.empty root;
        Some !seen
      with
      | Capped -> None
    in
    { pairs = Pair.Set.cardinal pairs; moves; nested }
  ;;

  let prefer_mutual ({ pairs; moves; nested } : cost) : bool =
    match nested with None -> true | Some n -> n > pairs + moves
  ;;
end
