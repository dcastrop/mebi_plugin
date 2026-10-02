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

  type edgemap

  val respond : ?silent:edgemap -> fsm -> state -> label -> states -> transition

  val successors
    :  ?silent:edgemap
    -> refl:bool
    -> fsm
    -> fsm
    -> partition
    -> Pair.t
    -> Pair.t list

  val reachable
    :  ?silent:edgemap
    -> refl:bool
    -> fsm
    -> fsm
    -> partition
    -> Pair.t
    -> Pair.Set.t

  type cost =
    { pairs : int
    ; moves : int
    ; nested : int option
    }

  val estimate
    :  ?cap_factor:int
    -> ?silent:edgemap
    -> refl:bool
    -> fsm
    -> fsm
    -> partition
    -> Pair.t
    -> cost

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
        and type info = C.Info.t)
    (Saturation :
       Saturation.S
       with type state = C.State.t
        and type edgemap = C.EdgeMap.t'
        and type annotation = C.Annotation.t) =
struct
  type edgemap = C.EdgeMap.t'
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
  (* A silent move answered by moving silently: the nearest state, by one or
     more silent steps of [silent], that lies in [bisimilar]. Standing still
     (zero steps) is the caller's case, decided before [respond] is asked.
     Saturation keeps only weak moves with a visible action, so without this
     a silent move whose target is not bisimilar to [from] had no answer at
     all, though [=ε=>] allows one. See [ASSISTED-CHANGES.md], 2026-10-02
     (second session). *)
  let respond_silently
        (silent : C.EdgeMap.t')
        (from : C.State.t)
        (label : C.Label.t)
        (bisimilar : C.State.Set.t)
    : C.Transition.t
    =
    let nearest =
      Saturation.silent_paths silent from
      |> List.filter_map (fun (s, ann, len) ->
        match ann with
        | Some ann when len > 0 && C.State.Set.mem s bisimilar ->
          Some (s, ann, len)
        | _ -> None)
      |> List.sort (fun (s, _, l) (s', _, l') ->
        match Int.compare l l' with 0 -> C.State.compare s s' | n -> n)
    in
    match nearest with
    | (goto, annotation, _) :: _ ->
      { from; goto; label; annotation = Some annotation; tree = None }
    | [] -> raise (NoBisimilarResponse { from; label })
  ;;

  let respond
        ?(silent : C.EdgeMap.t' option)
        (m : FSM.t)
        (from : C.State.t)
        (label : C.Label.t)
        (bisimilar : C.State.Set.t)
    : C.Transition.t
    =
    Logger.trace __FUNCTION__;
    match silent with
    | Some silent when C.Label.is_silent label ->
      respond_silently silent from label bisimilar
    | _ ->
      (try
         let ({ annotation; trees; _ }, destinations) : C.Action.Pair.t =
           (* NOTE: get actions [from] with [label] *)
           (match C.EdgeMap.find_opt m.edges from with
            | Some actions -> C.Action.Map.reduce_by_label actions label
            | None ->
              (* No weak move at all from [from] (a terminal of the saturated
                 FSM): the same answer as no move under [label], rather than
                 [Not_found] escaping to a caller that only expects
                 [NoBisimilarResponse]. *)
              raise (NoBisimilarResponse { from; label }))
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
       | C.Action.Pair.Set.IsEmpty ->
         raise (NoBisimilarResponse { from; label }))
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

  (* [refl] says whether both sides of the game use the same LTS. When they do,
     a pair of equal states is closed outright by [weak_sim_refl] -- mirrors
     [Proof_solver_step.handle_weaksim]'s [is_weak_refl] test, which runs
     before anything else -- so it is a leaf with no obligations. Without
     this, a game whose two sides converge on a common state (e.g. [r] is one
     step of [q]'s unfolding, [Proc/Test1]'s [wsim_pr]) went on to enumerate
     that state's whole loop as surplus pairs the solver never visits. *)
  let successors
        ?(silent : C.EdgeMap.t' option)
        ~(refl : bool)
        (a : FSM.t)
        (b : FSM.t)
        (pi : C.Partition.t)
        ((x, y) : Pair.t)
    : Pair.t list
    =
    Logger.trace __FUNCTION__;
    if refl && C.State.equal x y
    then []
    else
      List.filter_map
        (fun ((label, x') : C.Label.t * C.State.t) ->
          let bisimilar : C.State.Set.t = bisimilar_with pi x' in
          (* Mirrors [Proof_solver_step.handle_wk_concl]: a silent move to
             somewhere already bisimilar to [y] is answered by standing still,
             and everything else goes through [respond]. *)
          if C.Label.is_silent label && C.State.Set.mem y bisimilar
          then Some (x', y)
          else (
            match respond ?silent b y label bisimilar with
            | t -> Some (x', t.goto)
            | exception NoBisimilarResponse _ -> None))
        (obligations a x)
  ;;

  let reachable
        ?(silent : C.EdgeMap.t' option)
        ~(refl : bool)
        (a : FSM.t)
        (b : FSM.t)
        (pi : C.Partition.t)
        (root : Pair.t)
    : Pair.Set.t
    =
    Logger.trace __FUNCTION__;
    let rec go (seen : Pair.Set.t) : Pair.t list -> Pair.Set.t = function
      | [] -> seen
      | p :: rest ->
        let next : Pair.t list =
          successors ?silent ~refl a b pi p
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
        ?(silent : C.EdgeMap.t' option)
        ~(refl : bool)
        (a : FSM.t)
        (b : FSM.t)
        (pi : C.Partition.t)
        (root : Pair.t)
    : cost
    =
    Logger.trace __FUNCTION__;
    let pairs : Pair.Set.t = reachable ?silent ~refl a b pi root in
    let moves : int =
      Pair.Set.fold
        (fun p acc -> acc + List.length (successors ?silent ~refl a b pi p))
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
        List.iter (walk path) (successors ?silent ~refl a b pi p))
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
