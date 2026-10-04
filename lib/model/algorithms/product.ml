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
    module Map : Map.S with type key = t
  end

  exception
    NoBisimilarResponse of
      { from : state
      ; label : label
      }

  type edgemap

  val respond : ?silent:edgemap -> fsm -> state -> label -> states -> transition
  val simulation : fsm -> fsm -> fsm -> Pair.t -> Pair.Set.t

  type answer =
    | Stay
    | Move of transition

  val answer
    :  ?silent:edgemap
    -> ?sim:(state -> states)
    -> fsm
    -> partition
    -> state
    -> label
    -> state
    -> answer option

  val successors
    :  ?silent:edgemap
    -> ?sim:(state -> states)
    -> refl:bool
    -> fsm
    -> fsm
    -> partition
    -> Pair.t
    -> Pair.t list

  (** Raised by a game walk ({!val:reachable}, the planners) that visits
      more pairs than the cap {!val:with_cap} set. *)
  exception Game_too_large of int

  (** [with_cap n f] runs [f] with every game walk capped at [n] pairs:
      past it, the walk raises {!exception:Game_too_large}. For FSMs
      saturated on demand, whose walks saturate as they go
      ([MeBi Config Bounds Game]). *)
  val with_cap : int -> (unit -> 'a) -> 'a

  val reachable
    :  ?silent:edgemap
    -> ?sim:(state -> states)
    -> refl:bool
    -> fsm
    -> fsm
    -> partition
    -> Pair.t
    -> Pair.Set.t

  type game =
    { a : fsm
    ; a_saturated : fsm
    ; b : fsm
    ; b_saturated : fsm
    }

  val successors_bisim : refl:bool -> game -> partition -> Pair.t -> Pair.t list
  val reachable_bisim : refl:bool -> game -> partition -> Pair.t -> Pair.Set.t

  module Policy : sig
    type t =
      | Default
      | Greedy
      | Minimal

    val name : t -> string

    type choice =
      { next : Pair.t
      ; cost : int
      ; answer : answer
      }

    type key =
      { swapped : bool
      ; mover : state
      ; answerer : state
      ; label : label
      ; target : state
      }

    type obligation =
      { key : key
      ; default : choice option
      ; candidates : choice list
      }

    type game_of = Pair.t -> obligation list

    val sim_game
      :  ?silent:edgemap
      -> ?sim:(state -> states)
      -> refl:bool
      -> fsm
      -> fsm
      -> partition
      -> game_of

    val bisim_game : refl:bool -> game -> partition -> game_of

    type measure =
      { pairs : int
      ; moves : int
      ; witness : int
      ; unanswered : int
      }

    module KeyMap : Map.S with type key = key

    type plan =
      { policy : t
      ; root : Pair.t
      ; relation : Pair.Set.t
      ; chosen : choice KeyMap.t
      ; next : Pair.t list Pair.Map.t
      ; measure : measure
      }

    val plan : t -> game_of -> Pair.t -> plan
    val minimal_relation : game_of -> Pair.t -> Pair.Set.t
    val measure : t -> game_of -> Pair.t -> measure
    val predicted : measure -> float
    val best : game_of -> Pair.t -> plan
    val choose : plan -> key -> answer option
    val successors : plan -> Pair.t -> Pair.t list
  end

  type cost =
    { pairs : int
    ; moves : int
    ; nested : int option
    }

  val estimate
    :  ?cap_factor:int
    -> ?silent:edgemap
    -> ?sim:(state -> states)
    -> refl:bool
    -> fsm
    -> fsm
    -> partition
    -> Pair.t
    -> cost

  val estimate_bisim
    :  ?cap_factor:int
    -> refl:bool
    -> game
    -> partition
    -> Pair.t
    -> cost

  val estimate_plan : ?cap_factor:int -> Policy.plan -> cost
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

    (* Lexicographic: left state first. *)
    let compare ((a, b) : t) ((x, y) : t) : int =
      match C.State.compare a x with 0 -> C.State.compare b y | n -> n
    ;;

    (* [compare] is [0]. *)
    let equal (a : t) (b : t) : bool = Int.equal (compare a b) 0

    module Set = Set.Make (struct
        type nonrec t = t

        let compare = compare
      end)

    module Map = Map.Make (struct
        type nonrec t = t

        let compare = compare
      end)
  end

  exception
    NoBisimilarResponse of
      { from : state
      ; label : label
      }

  (* [best_response actions label bisimilar]: the weak move [respond]
     answers with, among [actions] (one state's weak actions, each with its
     destinations): those under [label] whose destinations meet [bisimilar],
     the one with the shortest annotation ([None] counting as 0), ties going
     to the least action by [Action.compare]; returned with its destinations
     cut down to [bisimilar], or [None] if there is none.

     One pass over [actions]. Until 2026-10-04 [respond] built the same
     choice as a set: [reduce_by_label] (a copy of the table), then
     [to_actionpairs] (an ordered set of (action, destinations) pairs), then
     a [filter_map] and [shortest_annotation], which keeps the first of the
     shortest in that set's order -- the least action, as here, since two
     distinct actions never compare equal. Building that set cost ~85% of
     ~165ms per answer on [Proc/Test4] saturated on demand (~8,000 weak
     actions a state; notes/15). *)
  let best_response
        (actions : C.Action.Map.t')
        (label : C.Label.t)
        (bisimilar : C.State.Set.t)
    : (C.Action.t * C.State.Set.t) option
    =
    (* [a] answers strictly better than [b]: shorter, or as short and less *)
    let better (a : C.Action.t) (b : C.Action.t) : bool =
      match
        Int.compare
          (C.Annotation.opt_length a.annotation)
          (C.Annotation.opt_length b.annotation)
      with
      | 0 -> C.Action.compare a b < 0
      | n -> n < 0
    in
    C.Action.Map.fold
      (fun (a : C.Action.t) (ds : C.State.Set.t) best ->
        if
          Bool.not (C.Label.equal a.label label)
          || C.State.Set.disjoint bisimilar ds
        then best
        else (
          match best with
          | Some (b, _) when Bool.not (better a b) -> best
          | _ -> Some (a, ds)))
      actions
      None
    |> Stdlib.Option.map (fun (a, ds) -> a, C.State.Set.inter bisimilar ds)
  ;;

  (* [respond_silently silent from label bisimilar]: a silent move answered
     by moving silently: the nearest state, by one or
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

  (* See the [.mli]. A silent [label] with [silent] goes to
     {!respond_silently}; anything else to {!best_response} over [from]'s weak
     actions (saturating [from] first if [m] is saturated on demand), its
     least destination being the answer.

     Originally lifted verbatim out of
     [Proof_solver_step.try_get_visible_transition], whose two preceding
     lines resolved [from] and [label] out of the Rocq goal; everything here
     is, and always was, pure model code. Keep it that way -- the caller
     resolves, this decides. *)
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
      FSM.ensure m from;
      (* No weak move at all from [from] (a terminal of the saturated FSM) is
         the same answer as no move under [label], rather than [Not_found]
         escaping to a caller that only expects [NoBisimilarResponse]. *)
      let best : (C.Action.t * C.State.Set.t) option =
        match C.EdgeMap.find_opt m.edges from with
        | Some actions -> best_response actions label bisimilar
        | None -> None
      in
      (match best with
       | None -> raise (NoBisimilarResponse { from; label })
       | Some ({ annotation; trees; _ }, destinations) ->
         let tree : Base.Tree.t option = Base.Trees.min_opt trees in
         let goto : C.State.t = C.State.Set.min_elt destinations in
         { from; goto; label; annotation; tree })
  ;;

  (** [bisimilar_with pi x]: [x]'s block of [pi], or the empty set if [x] is
      in none. *)
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

  type answer =
    | Stay
    | Move of C.Transition.t

  (* See the [.mli]. THE choice of answer, for the solver and the product alike: the one place
     a policy for choosing answers would go. [y] must answer the other
     system's move [-label-> x'].

     A silent move to somewhere already bisimilar to [y] is answered by
     standing still, and everything else by [respond] into [x']'s
     bisimilarity class. Failing both, given [sim] (a [weak_sim] goal between
     states that are similar but not bisimilar), the same two tries against
     [x']'s simulators. Before 2026-10-02 this was written out twice, here
     and in [Proof_solver_step.handle_wk_concl]/[handle_visible_transition],
     which had to be kept in step by hand: a mismatch is a pair outside the
     mutual block. *)
  let answer
        ?(silent : C.EdgeMap.t' option)
        ?(sim : (C.State.t -> C.State.Set.t) option)
        (b : FSM.t)
        (pi : C.Partition.t)
        (y : C.State.t)
        (label : C.Label.t)
        (x' : C.State.t)
    : answer option
    =
    Logger.trace __FUNCTION__;
    (* [into target]: standing still if the move is silent and [y] is already
       in [target], else [respond]'s move into [target]; [None] if neither. *)
    let into (target : C.State.Set.t) : answer option =
      if C.Label.is_silent label && C.State.Set.mem y target
      then Some Stay
      else (
        match respond ?silent b y label target with
        | t -> Some (Move t)
        | exception NoBisimilarResponse _ -> None)
    in
    match into (bisimilar_with pi x'), sim with
    | Some a, _ -> Some a
    | None, Some sim -> into (sim x')
    | None, None -> None
  ;;

  (* See the [.mli]: one successor per obligation of [x] that {!answer}
     answers. [refl] says whether both sides of the game use the same LTS. When they do,
     a pair of equal states is closed outright by [weak_sim_refl] -- mirrors
     [Proof_solver_step.handle_weaksim]'s [is_weak_refl] test, which runs
     before anything else -- so it is a leaf with no obligations. Without
     this, a game whose two sides converge on a common state (e.g. [r] is one
     step of [q]'s unfolding, [Proc/Test1]'s [wsim_pr]) went on to enumerate
     that state's whole loop as surplus pairs the solver never visits. *)
  let successors
        ?(silent : C.EdgeMap.t' option)
        ?(sim : (C.State.t -> C.State.Set.t) option)
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
          match answer ?silent ?sim b pi y label x' with
          | Some Stay -> Some (x', y)
          | Some (Move t) -> Some (x', t.goto)
          | None -> None)
        (obligations a x)
  ;;

  exception Game_too_large of int

  (* The cap on game walks, set by [with_cap] (notes/13, 2026-10-03). *)
  let walk_cap : int option ref = ref None

  (* See the [.mli]. The previous cap is restored afterwards, even if [f]
     raises. *)
  let with_cap (n : int) (f : unit -> 'a) : 'a =
    let before = !walk_cap in
    walk_cap := Some n;
    Fun.protect ~finally:(fun () -> walk_cap := before) f
  ;;

  (* [count] pairs reached so far: past the cap, stop. *)
  let check_cap (count : int) : unit =
    match !walk_cap with
    | Some n when count > n -> raise (Game_too_large n)
    | _ -> ()
  ;;

  (* [reachable_by step root]: every pair reachable from [root] by repeatedly
     applying [step] ([root] included), breadth-first. The one game walk
     every other walk here goes through, so the [with_cap] cap applies to all
     of them: past it, {!Game_too_large}. *)
  let reachable_by (step : Pair.t -> Pair.t list) (root : Pair.t) : Pair.Set.t =
    let count : int ref = ref 1 in
    (* [go seen queue]: [seen] grown by everything reachable from [queue],
       which holds pairs already in [seen] still to be stepped. *)
    let rec go (seen : Pair.Set.t) : Pair.t list -> Pair.Set.t = function
      | [] -> seen
      | p :: rest ->
        let next : Pair.t list =
          step p |> List.filter (fun q -> not (Pair.Set.mem q seen))
        in
        count := !count + List.length next;
        check_cap !count;
        go
          (List.fold_left (fun acc q -> Pair.Set.add q acc) seen next)
          (List.rev_append next rest)
    in
    go (Pair.Set.singleton root) [ root ]
  ;;

  (* [weak_answers b b_saturated]: a function giving, for a state [y] of [b]
     and a label [l], every state [y] can answer a move [-l->] with, as
     [weak_sim] allows: [y =l=> y'] for a visible [l] (read off
     [b_saturated], saturating [y] first if [b] is saturated on demand), and
     [y =eps=> y'] for a silent one ([b]'s own silent closure, zero steps
     included, so [y] itself). Silent closures are computed once per state. *)
  let weak_answers (b : FSM.t) (b_saturated : FSM.t)
    : C.State.t -> C.Label.t -> C.State.Set.t
    =
    let closures : (C.State.t, C.State.Set.t) Hashtbl.t = Hashtbl.create 64 in
    (* [closure y]: the states [y] reaches by [=eps=>], memoised. *)
    let closure (y : C.State.t) : C.State.Set.t =
      match Hashtbl.find_opt closures y with
      | Some c -> c
      | None ->
        let c =
          Saturation.silent_paths b.edges y
          |> List.fold_left
               (fun acc (s, _, _) -> C.State.Set.add s acc)
               C.State.Set.empty
        in
        Hashtbl.add closures y c;
        c
    in
    fun (y : C.State.t) (l : C.Label.t) ->
      if C.Label.is_silent l
      then closure y
      else (
        FSM.ensure b_saturated y;
        match C.EdgeMap.find_opt b_saturated.edges y with
        | None -> C.State.Set.empty
        | Some actions ->
          C.Action.Map.destinations (C.Action.Map.reduce_by_label actions l))
  ;;

  (* [simulation_game a answers root]: the pairs of the simulation game
     reachable from [root]: from [(x, y)], every strong move [x -l-> x'] of
     [a] ({!obligations}) paired with every answer [y'] in [answers y l].
     Counted against [with_cap]'s cap, like every game walk. *)
  let simulation_game
        (a : FSM.t)
        (answers : C.State.t -> C.Label.t -> C.State.Set.t)
        (root : Pair.t)
    : Pair.Set.t
    =
    (* [step (x, y)]: every [(x', y')] with [x -l-> x'] and [y'] an answer. *)
    let step ((x, y) : Pair.t) : Pair.t list =
      List.concat_map
        (fun ((l, x') : C.Label.t * C.State.t) ->
          C.State.Set.fold (fun y' acc -> (x', y') :: acc) (answers y l) [])
        (obligations a x)
    in
    reachable_by step root
  ;;

  (* [refine_simulation a answers r]: the greatest weak simulation within
     the pairs [r]: repeatedly drop a pair [(x, y)] with a move [x -l-> x']
     that no answer [y'] in [answers y l] matches with [(x', y')] still in,
     until nothing changes. Naive: each round re-checks every pair. *)
  let refine_simulation
        (a : FSM.t)
        (answers : C.State.t -> C.Label.t -> C.State.Set.t)
        (r : Pair.Set.t)
    : Pair.Set.t
    =
    (* [answered r (x, y)]: every move of [x] has an answer [y'] with
       [(x', y')] in [r]. *)
    let answered (r : Pair.Set.t) ((x, y) : Pair.t) : bool =
      List.for_all
        (fun ((l, x') : C.Label.t * C.State.t) ->
          C.State.Set.exists (fun y' -> Pair.Set.mem (x', y') r) (answers y l))
        (obligations a x)
    in
    (* one round: drop the unanswered pairs; stop when none was dropped *)
    let rec go (r : Pair.Set.t) : Pair.Set.t =
      let r' = Pair.Set.filter (answered r) r in
      if Int.equal (Pair.Set.cardinal r') (Pair.Set.cardinal r)
      then r
      else go r'
    in
    go r
  ;;

  (* The greatest weak simulation from [a] to [b], as [weak_sim] defines it,
     restricted to the pairs reachable from [root] in the simulation game
     ({!simulation_game}, then {!refine_simulation}). That is exactly the
     greatest simulation's own pairs among them (any successor of a pair in
     it is reachable too), so [root] is in it iff [fst root] is weakly
     simulated by [snd root]. Until 2026-10-04 this started from all
     |a| x |b| pairs: ~94M for [Proc/Test4], ~6GB a copy. *)
  let simulation (a : FSM.t) (b : FSM.t) (b_saturated : FSM.t) (root : Pair.t)
    : Pair.Set.t
    =
    Logger.trace __FUNCTION__;
    let answers = weak_answers b b_saturated in
    refine_simulation a answers (simulation_game a answers root)
  ;;

  (* See the [.mli]: {!reachable_by} over {!successors}. *)
  let reachable
        ?(silent : C.EdgeMap.t' option)
        ?(sim : (C.State.t -> C.State.Set.t) option)
        ~(refl : bool)
        (a : FSM.t)
        (b : FSM.t)
        (pi : C.Partition.t)
        (root : Pair.t)
    : Pair.Set.t
    =
    Logger.trace __FUNCTION__;
    reachable_by (successors ?silent ?sim ~refl a b pi) root
  ;;

  type game =
    { a : FSM.t
    ; a_saturated : FSM.t
    ; b : FSM.t
    ; b_saturated : FSM.t
    }

  (* A bisimulation game state [(x, y)] has the obligations of both sides:
     [x]'s moves answered by [b] (as in [successors]), and [y]'s moves answered
     by [a] -- the same game with the two systems swapped, its pairs swapped
     back. Mirrors the two goals [Pack_bisim] leaves, [bisim_l] and
     [bisim_r]. *)
  let successors_bisim
        ~(refl : bool)
        (g : game)
        (pi : C.Partition.t)
        ((x, y) : Pair.t)
    : Pair.t list
    =
    Logger.trace __FUNCTION__;
    let left : Pair.t list =
      successors ~silent:g.b.edges ~refl g.a g.b_saturated pi (x, y)
    in
    let right : Pair.t list =
      successors ~silent:g.a.edges ~refl g.b g.a_saturated pi (y, x)
      |> List.map (fun ((y', x') : Pair.t) -> x', y')
    in
    left @ right
  ;;

  (* See the [.mli]: {!reachable_by} over {!successors_bisim}. *)
  let reachable_bisim
        ~(refl : bool)
        (g : game)
        (pi : C.Partition.t)
        (root : Pair.t)
    : Pair.Set.t
    =
    Logger.trace __FUNCTION__;
    reachable_by (successors_bisim ~refl g pi) root
  ;;

  (* Answer policies (see the [.mli]). Built first for measurement
     (2026-10-02); since [MeBi Config Solver Answers] the solver answers from
     a [plan] whenever the policy is not [Default].

     A game is described per state [p] as its obligations, each with every
     [candidate] answer -- the next pair and the witness length, i.e. the
     weak transitions to justify (0 for standing still) -- and the [default],
     the one [answer] picks. The policies choose a candidate per obligation:
     - [Default]: as the solver does now;
     - [Greedy]: breadth first, preferring a candidate whose pair has
       already been reached, else the default;
     - [Minimal]: from every pair reachable by any candidate (always a valid
       relation), delete pairs one at a time while every remaining pair can
       still answer all its obligations within what remains -- minimal by
       inclusion, not necessarily the smallest -- then answer each obligation
       with its cheapest remaining candidate. *)
  module Policy = struct
    type t =
      | Default
      | Greedy
      | Minimal

    (* [name p]: the policy as [MeBi Config Solver Answers] spells it, in
       lower case. *)
    let name : t -> string = function
      | Default -> "default"
      | Greedy -> "greedy"
      | Minimal -> "minimal"
    ;;

    type choice =
      { next : Pair.t
      ; cost : int
      ; answer : answer
      }

    (* An obligation is keyed the way the solver meets it: whether the roles
       are swapped ([bisim_r]), the moving system's state and move, and the
       answering system's state. *)
    type key =
      { swapped : bool
      ; mover : C.State.t
      ; answerer : C.State.t
      ; label : C.Label.t
      ; target : C.State.t
      }

    type obligation =
      { key : key
      ; default : choice option
      ; candidates : choice list
      }

    type game_of = Pair.t -> obligation list

    (* [annotation_length a]: the number of steps in the witness [a]. *)
    let rec annotation_length (a : C.Annotation.t) : int =
      match a.next with None -> 1 | Some n -> 1 + annotation_length n
    ;;

    (* [transition_length t]: the weak transitions [t] stands for: its
       witness's length, or 1 for a plain step. *)
    let transition_length (t : C.Transition.t) : int =
      match t.annotation with None -> 1 | Some a -> annotation_length a
    ;;

    (** [stay_candidate y label target]: standing still, at no cost, if
        [label] is silent and [y] is already in [target]. *)
    let stay_candidate
          (y : C.State.t)
          (label : C.Label.t)
          (target : C.State.Set.t)
      : (C.State.t * int * answer) list
      =
      if C.Label.is_silent label && C.State.Set.mem y target
      then [ y, 0, Stay ]
      else []
    ;;

    (** [silent_move_candidates silent y label target]: for a silent
        [label], every state of [target] that [y] reaches by one or more
        silent steps of [silent], with the path's length and the move
        (annotated with the path); none without [silent]. *)
    let silent_move_candidates
          (silent : C.EdgeMap.t' option)
          (y : C.State.t)
          (label : C.Label.t)
          (target : C.State.Set.t)
      : (C.State.t * int * answer) list
      =
      match silent with
      | None -> []
      | Some silent ->
        Saturation.silent_paths silent y
        |> List.filter_map (fun (s, ann, len) ->
          match ann with
          | Some ann when len > 0 && C.State.Set.mem s target ->
            Some
              ( s
              , len
              , Move
                  { from = y
                  ; goto = s
                  ; label
                  ; annotation = Some ann
                  ; tree = None
                  } )
          | _ -> None)
    ;;

    (** [visible_move_candidates b y label target]: for a visible [label],
        every weak move of [b] from [y] under [label] into [target] (one per
        destination), in the order of [y]'s actions, with its witness length
        (1 for a plain step); [y] is saturated first if [b] is on demand. *)
    let visible_move_candidates
          (b : FSM.t)
          (y : C.State.t)
          (label : C.Label.t)
          (target : C.State.Set.t)
      : (C.State.t * int * answer) list
      =
      FSM.ensure b y;
      match C.EdgeMap.find_opt b.edges y with
      | None -> []
      | Some actions ->
        C.Action.Map.reduce_by_label actions label
        |> C.Action.Map.to_actionpairs
        |> C.Action.Pair.Set.elements
        |> List.concat_map (fun ((action, ds) : C.Action.t * C.State.Set.t) ->
          let len =
            match action.annotation with
            | None -> 1
            | Some a -> annotation_length a
          in
          let tree = Base.Trees.min_opt action.trees in
          C.State.Set.elements (C.State.Set.inter ds target)
          |> List.map (fun d ->
            ( d
            , len
            , Move
                { from = y
                ; goto = d
                ; label
                ; annotation = action.annotation
                ; tree
                } )))
    ;;

    (* Every way [b], at [y], can answer [-label-> x'], with the answer itself
       -- the same targets, in the same order, as [answer] tries them -- and
       its witness length: standing still, then moves
       ({!silent_move_candidates} or {!visible_move_candidates}), into
       [x']'s bisimilarity class; failing any, and given [sim], into [x']'s
       simulators. *)
    let candidates
          ?(silent : C.EdgeMap.t' option)
          ?(sim : (C.State.t -> C.State.Set.t) option)
          (b : FSM.t)
          (pi : C.Partition.t)
          (y : C.State.t)
          (label : C.Label.t)
          (x' : C.State.t)
      : (C.State.t * int * answer) list
      =
      (* [into target]: every candidate into [target] *)
      let into (target : C.State.Set.t) : (C.State.t * int * answer) list =
        stay_candidate y label target
        @
        if C.Label.is_silent label
        then silent_move_candidates silent y label target
        else visible_move_candidates b y label target
      in
      match into (bisimilar_with pi x'), sim with
      | (_ :: _ as cs), _ -> cs
      | [], Some sim -> into (sim x')
      | [], None -> []
    ;;

    (* The simulation game: [x]'s moves answered by [b]. *)
    let sim_game
          ?(silent : C.EdgeMap.t' option)
          ?(sim : (C.State.t -> C.State.Set.t) option)
          ~(refl : bool)
          (a : FSM.t)
          (b : FSM.t)
          (pi : C.Partition.t)
      : game_of
      =
      fun ((x, y) : Pair.t) ->
      if refl && C.State.equal x y
      then []
      else
        List.map
          (fun ((label, x') : C.Label.t * C.State.t) ->
            let choice ((y', cost, answer) : C.State.t * int * answer) =
              { next = x', y'; cost; answer }
            in
            { key =
                { swapped = false; mover = x; answerer = y; label; target = x' }
            ; default =
                (match answer ?silent ?sim b pi y label x' with
                 | Some Stay -> Some (choice (y, 0, Stay))
                 | Some (Move t) ->
                   Some (choice (t.goto, transition_length t, Move t))
                 | None -> None)
            ; candidates =
                List.map choice (candidates ?silent ?sim b pi y label x')
            })
          (obligations a x)
    ;;

    (* The bisimulation game: both sides' obligations, the right-hand ones as
       the simulation game with the systems swapped, pairs swapped back. *)
    let bisim_game ~(refl : bool) (g : game) (pi : C.Partition.t) : game_of =
      let left = sim_game ~silent:g.b.edges ~refl g.a g.b_saturated pi in
      let right = sim_game ~silent:g.a.edges ~refl g.b g.a_saturated pi in
      (* a right-hand choice, its pair put back in left-right order *)
      let swap (c : choice) = { c with next = snd c.next, fst c.next } in
      fun ((x, y) : Pair.t) ->
        left (x, y)
        @ List.map
            (fun (o : obligation) ->
              { key = { o.key with swapped = true }
              ; default = Stdlib.Option.map swap o.default
              ; candidates = List.map swap o.candidates
              })
            (right (y, x))
    ;;

    type measure =
      { pairs : int
      ; moves : int
      ; witness : int
      ; unanswered : int
      }

    (* Moves as map keys, compared field by field. *)
    module KeyMap = Map.Make (struct
        type t = key

        let compare (a : key) (b : key) : int =
          match Bool.compare a.swapped b.swapped with
          | 0 ->
            (match C.State.compare a.mover b.mover with
             | 0 ->
               (match C.State.compare a.answerer b.answerer with
                | 0 ->
                  (match C.Label.compare a.label b.label with
                   | 0 -> C.State.compare a.target b.target
                   | n -> n)
                | n -> n)
             | n -> n)
          | n -> n
        ;;
      end)

    type plan =
      { policy : t
      ; root : Pair.t
      ; relation : Pair.Set.t
      ; chosen : choice KeyMap.t
      ; next : Pair.t list Pair.Map.t
      ; measure : measure
      }

    (** What a {!walk} has recorded so far: the pairs reached, the choice
        made for each move, each stepped pair's successors, and the
        measure's running counts. *)
    type walk_state =
      { reached : Pair.Set.t
      ; choices : choice KeyMap.t
      ; successors_of : Pair.t list Pair.Map.t
      ; moves_answered : int
      ; witness_total : int
      ; unanswered_moves : int
      }

    (** [answer_pair choose st p obligations]: [st] after answering each of
        [p]'s [obligations] with [choose] (given the pairs reached so far): a
        choice is recorded and its move and witness counted, and a pair not
        reached before is added (checked against the [with_cap] cap first);
        an obligation [choose] leaves unanswered is only counted. [p]'s
        successors are recorded in order. Also returns the newly reached
        pairs, in order. *)
    let answer_pair
          (choose : Pair.Set.t -> obligation -> choice option)
          (st : walk_state)
          (p : Pair.t)
          (obligations : obligation list)
      : walk_state * Pair.t list
      =
      let st, fresh, succ =
        List.fold_left
          (fun ((st, fresh, succ) : walk_state * Pair.t list * Pair.t list) o ->
            match choose st.reached o with
            | None ->
              ( { st with unanswered_moves = st.unanswered_moves + 1 }
              , fresh
              , succ )
            | Some c ->
              let st =
                { st with
                  choices = KeyMap.add o.key c st.choices
                ; moves_answered = st.moves_answered + 1
                ; witness_total = st.witness_total + c.cost
                }
              in
              if Pair.Set.mem c.next st.reached
              then st, fresh, c.next :: succ
              else (
                if Stdlib.Option.is_some !walk_cap
                then check_cap (Pair.Set.cardinal st.reached + 1);
                ( { st with reached = Pair.Set.add c.next st.reached }
                , c.next :: fresh
                , c.next :: succ )))
          (st, [], [])
          obligations
      in
      ( { st with
          successors_of = Pair.Map.add p (List.rev succ) st.successors_of
        }
      , List.rev fresh )
    ;;

    (* [walk policy game_of choose root]: the plan [policy] makes: [root]
       closed breadth-first under [choose] ({!answer_pair} per pair), then
       read off as a {!type-plan}. *)
    let walk
          (policy : t)
          (game_of : game_of)
          (choose : Pair.Set.t -> obligation -> choice option)
          (root : Pair.t)
      : plan
      =
      (* [go st queue]: step the pairs in [queue], appending each one's newly
         reached pairs to it *)
      let rec go (st : walk_state) : Pair.t list -> walk_state = function
        | [] -> st
        | p :: rest ->
          let st, fresh = answer_pair choose st p (game_of p) in
          go st (rest @ fresh)
      in
      let st =
        go
          { reached = Pair.Set.singleton root
          ; choices = KeyMap.empty
          ; successors_of = Pair.Map.empty
          ; moves_answered = 0
          ; witness_total = 0
          ; unanswered_moves = 0
          }
          [ root ]
      in
      { policy
      ; root
      ; relation = st.reached
      ; chosen = st.choices
      ; next = st.successors_of
      ; measure =
          { pairs = Pair.Set.cardinal st.reached
          ; moves = st.moves_answered
          ; witness = st.witness_total
          ; unanswered = st.unanswered_moves
          }
      }
    ;;

    (* [cheapest cs]: the choice of least witness length in [cs], the first
       of them on a tie; [None] if [cs] is empty. *)
    let cheapest (cs : choice list) : choice option =
      List.fold_left
        (fun acc (c : choice) ->
          match acc with
          | Some (best : choice) when best.cost <= c.cost -> acc
          | _ -> Some c)
        None
        cs
    ;;

    (* From every pair any answer reaches, delete pairs while every remaining
       pair can still answer all its moves within what remains: repeatedly
       the first removable pair, in [Pair.Set] order, then whatever [root] no
       longer reaches.

       The relation stays valid throughout (checked at the start; a removal
       is allowed only if it keeps it valid; trimming unreachable pairs
       cannot break a reachable one, whose answers are reachable too). So
       [p] is removable iff no move of a remaining pair other than [p] has
       [p] as its only remaining answer: a count per pair ([blocking]),
       maintained as answers disappear, with the removable pairs kept in an
       ordered set. Reachability from [root] is kept as a spanning tree:
       removing pairs can only disconnect their subtrees, which are re-attached
       through any other remaining predecessor, and the rest trimmed.

       Until 2026-10-03 every candidate removal re-validated every remaining
       pair, and every removal re-walked the whole relation: on
       [Proc/Test4]'s [weak_bisimilar] (6592 pairs, every state bisimilar,
       so many answers per move) planning took 19.5 minutes. The removals and
       their order are the same, so the relation is the same ([tests.exe]
       checks it against the original algorithm). *)
    let minimal_relation (game_of : game_of) (root : Pair.t) : Pair.Set.t =
      let memo : (Pair.t, Pair.t array array) Hashtbl.t = Hashtbl.create 256 in
      (* per pair, per obligation, its distinct answers *)
      let answers (p : Pair.t) : Pair.t array array =
        match Hashtbl.find_opt memo p with
        | Some a -> a
        | None ->
          let a =
            Array.of_list
              (List.map
                 (fun (o : obligation) ->
                   Array.of_list
                     (List.sort_uniq
                        Pair.compare
                        (List.map (fun (c : choice) -> c.next) o.candidates)))
                 (game_of p))
          in
          Hashtbl.add memo p a;
          a
      in
      let all : Pair.Set.t =
        reachable_by
          (fun p -> Array.to_list (answers p) |> List.concat_map Array.to_list)
          root
      in
      if
        Bool.not
          (Pair.Set.for_all
             (fun q -> Array.for_all (fun a -> Array.length a > 0) (answers q))
             all)
      then all
      else (
        let alive : (Pair.t, unit) Hashtbl.t = Hashtbl.create 1024 in
        Pair.Set.iter (fun p -> Hashtbl.replace alive p ()) all;
        let is_alive p = Hashtbl.mem alive p in
        (* reverse edges: p -> the obligations (q, i) that p answers *)
        let preds : (Pair.t, (Pair.t * int) list) Hashtbl.t =
          Hashtbl.create 1024
        in
        let rem : (Pair.t * int, int) Hashtbl.t = Hashtbl.create 1024 in
        Pair.Set.iter
          (fun q ->
            Array.iteri
              (fun i a ->
                Hashtbl.replace rem (q, i) (Array.length a);
                Array.iter
                  (fun p ->
                    Hashtbl.replace
                      preds
                      p
                      ((q, i)
                       :: Stdlib.Option.value
                            ~default:[]
                            (Hashtbl.find_opt preds p)))
                  a)
              (answers q))
          all;
        let preds_of p =
          Stdlib.Option.value ~default:[] (Hashtbl.find_opt preds p)
        in
        (* the pair an obligation pins: its only remaining answer, unless the
           obligation's own pair is gone or is that answer *)
        let contrib : (Pair.t * int, Pair.t) Hashtbl.t = Hashtbl.create 1024 in
        let blocking : (Pair.t, int) Hashtbl.t = Hashtbl.create 1024 in
        let removable : Pair.Set.t ref = ref Pair.Set.empty in
        let refresh (s : Pair.t) : unit =
          if
            is_alive s
            && (not (Pair.equal s root))
            && Stdlib.Option.value ~default:0 (Hashtbl.find_opt blocking s) = 0
          then removable := Pair.Set.add s !removable
          else removable := Pair.Set.remove s !removable
        in
        let recompute ((q, i) : Pair.t * int) : unit =
          let now : Pair.t option =
            if is_alive q && Hashtbl.find rem (q, i) = 1
            then (
              match Array.find_opt is_alive (answers q).(i) with
              | Some s when Bool.not (Pair.equal s q) -> Some s
              | _ -> None)
            else None
          in
          let before = Hashtbl.find_opt contrib (q, i) in
          if Stdlib.Option.equal Pair.equal before now
          then ()
          else (
            (match before with
             | Some s ->
               Hashtbl.replace blocking s (Hashtbl.find blocking s - 1);
               refresh s
             | None -> ());
            match now with
            | Some s ->
              Hashtbl.replace
                blocking
                s
                (1
                 + Stdlib.Option.value ~default:0 (Hashtbl.find_opt blocking s)
                );
              Hashtbl.replace contrib (q, i) s;
              refresh s
            | None -> Hashtbl.remove contrib (q, i))
        in
        Pair.Set.iter
          (fun q -> Array.iteri (fun i _ -> recompute (q, i)) (answers q))
          all;
        Pair.Set.iter refresh all;
        (* a batch of pairs leaves the relation *)
        let kill (xs : Pair.t list) : unit =
          List.iter (fun x -> Hashtbl.remove alive x) xs;
          List.iter
            (fun x ->
              removable := Pair.Set.remove x !removable;
              List.iter
                (fun (q, i) ->
                  Hashtbl.replace rem (q, i) (Hashtbl.find rem (q, i) - 1))
                (preds_of x))
            xs;
          List.iter
            (fun x ->
              Array.iteri (fun i _ -> recompute (x, i)) (answers x);
              List.iter recompute (preds_of x))
            xs
        in
        (* reachability from [root]: a spanning tree over remaining answers *)
        let parent : (Pair.t, Pair.t) Hashtbl.t = Hashtbl.create 1024 in
        let children : (Pair.t, Pair.t list) Hashtbl.t = Hashtbl.create 1024 in
        let adopt (p : Pair.t) (c : Pair.t) : unit =
          Hashtbl.replace parent c p;
          Hashtbl.replace
            children
            p
            (c :: Stdlib.Option.value ~default:[] (Hashtbl.find_opt children p))
        in
        let succ (u : Pair.t) : Pair.t list =
          Array.to_list (answers u)
          |> List.concat_map (fun a -> List.filter is_alive (Array.to_list a))
        in
        let seen : (Pair.t, unit) Hashtbl.t = Hashtbl.create 1024 in
        let rec bfs (frontier : Pair.t list) : unit =
          match frontier with
          | [] -> ()
          | _ ->
            let next =
              List.concat_map
                (fun u ->
                  List.filter_map
                    (fun v ->
                      if Hashtbl.mem seen v
                      then None
                      else (
                        Hashtbl.replace seen v ();
                        adopt u v;
                        Some v))
                    (succ u))
                frontier
            in
            bfs next
        in
        Hashtbl.replace seen root ();
        bfs [ root ];
        (* the tree's descendants of [p], through current parent links *)
        let subtree (p : Pair.t) : Pair.t list =
          let rec go acc = function
            | [] -> acc
            | u :: rest ->
              let cs =
                Stdlib.Option.value ~default:[] (Hashtbl.find_opt children u)
                |> List.filter (fun c ->
                  match Hashtbl.find_opt parent c with
                  | Some u' -> Pair.equal u u'
                  | None -> false)
              in
              Hashtbl.remove children u;
              go (List.rev_append cs acc) (List.rev_append cs rest)
          in
          go [] [ p ]
        in
        let remove (p : Pair.t) : unit =
          let s = subtree p in
          Hashtbl.remove parent p;
          kill [ p ];
          (* detach the subtree, then re-attach what another remaining
             predecessor still reaches *)
          let in_s : (Pair.t, unit) Hashtbl.t = Hashtbl.create 64 in
          List.iter
            (fun u ->
              Hashtbl.replace in_s u ();
              Hashtbl.remove parent u)
            s;
          let reattached : (Pair.t, unit) Hashtbl.t = Hashtbl.create 64 in
          let seeds =
            List.filter_map
              (fun u ->
                match
                  List.find_opt
                    (fun ((q, _) : Pair.t * int) ->
                      is_alive q
                      && (not (Hashtbl.mem in_s q))
                      && (Pair.equal q root || Hashtbl.mem parent q))
                    (preds_of u)
                with
                | Some (q, _) ->
                  adopt q u;
                  Hashtbl.replace reattached u ();
                  Some u
                | None -> None)
              s
          in
          let rec spread = function
            | [] -> ()
            | u :: rest ->
              let fresh =
                List.filter
                  (fun v ->
                    Hashtbl.mem in_s v && Bool.not (Hashtbl.mem reattached v))
                  (succ u)
              in
              List.iter
                (fun v ->
                  if Bool.not (Hashtbl.mem reattached v)
                  then (
                    Hashtbl.replace reattached v ();
                    adopt u v))
                fresh;
              spread (List.rev_append fresh rest)
          in
          spread seeds;
          kill (List.filter (fun u -> Bool.not (Hashtbl.mem reattached u)) s)
        in
        let rec shrink () : unit =
          match Pair.Set.min_elt_opt !removable with
          | None -> ()
          | Some p ->
            remove p;
            shrink ()
        in
        shrink ();
        Pair.Set.filter is_alive all)
    ;;

    (* See the [.mli]. [Default] takes each obligation's default; [Greedy]
       the cheapest candidate already reached, else the default; [Minimal]
       the cheapest candidate within {!minimal_relation}. *)
    let plan (policy : t) (game_of : game_of) (root : Pair.t) : plan =
      match policy with
      | Default -> walk policy game_of (fun _ o -> o.default) root
      | Greedy ->
        walk
          policy
          game_of
          (fun seen o ->
            match
              cheapest
                (List.filter
                   (fun (c : choice) -> Pair.Set.mem c.next seen)
                   o.candidates)
            with
            | Some c -> Some c
            | None -> o.default)
          root
      | Minimal ->
        let rel = minimal_relation game_of root in
        walk
          policy
          game_of
          (fun _ o ->
            cheapest
              (List.filter
                 (fun (c : choice) -> Pair.Set.mem c.next rel)
                 o.candidates))
          root
    ;;

    (* See the [.mli]: the measure of {!plan}. *)
    let measure (policy : t) (game_of : game_of) (root : Pair.t) : measure =
      (plan policy game_of root).measure
    ;;

    (* Iterations predicted from a plan's measure, by the linear fit to all
       41 checked-in proofs' real counts (2026-10-02, [ASSISTED-CHANGES.md]):
       about 3 per pair, 6 per move and 3.3 per weak transition of witness. *)
    let predicted (m : measure) : float =
      (3.0 *. float_of_int m.pairs)
      +. (6.0 *. float_of_int m.moves)
      +. (3.3 *. float_of_int m.witness)
    ;;

    (* The cheapest plan by [predicted]; ties go to the earlier policy in
       [Default; Greedy; Minimal], so [Default] wins unless another is
       strictly cheaper. *)
    let best (game_of : game_of) (root : Pair.t) : plan =
      let plans =
        List.map (fun p -> plan p game_of root) [ Default; Greedy; Minimal ]
      in
      List.fold_left
        (fun (best : plan) (p : plan) ->
          if
            p.measure.unanswered = 0
            && predicted p.measure < predicted best.measure
          then p
          else best)
        (List.hd plans)
        (List.tl plans)
    ;;

    (* See the [.mli]. *)
    let choose (p : plan) (k : key) : answer option =
      Stdlib.Option.map
        (fun (c : choice) -> c.answer)
        (KeyMap.find_opt k p.chosen)
    ;;

    (* See the [.mli]: [x]'s successors as recorded by the plan, none if it
       does not reach [x]. *)
    let successors (p : plan) (x : Pair.t) : Pair.t list =
      Stdlib.Option.value ~default:[] (Pair.Map.find_opt x p.next)
    ;;
  end

  type cost =
    { pairs : int
    ; moves : int
    ; nested : int option
    }

  (* Raised inside {!estimate_by} when the simulated nested walk passes its
     cap; never escapes. *)
  exception Capped

  (* [memo_step step]: [step], computing each pair's successors once and
     then returning them from a table. [step] is a function of the pair (an
     FSM saturated on demand only fills a cache underneath it), so the
     results are the same; the table holds one list per pair stepped. *)
  let memo_step (step : Pair.t -> Pair.t list) : Pair.t -> Pair.t list =
    let table : Pair.t list Pair.Map.t ref = ref Pair.Map.empty in
    fun (p : Pair.t) ->
      match Pair.Map.find_opt p !table with
      | Some next -> next
      | None ->
        let next = step p in
        table := Pair.Map.add p next !table;
        next
  ;;

  (* The cost of a proof of the game reachable from [root] under [step]; see
     [estimate]. [step] is memoised ({!memo_step}): the walk below steps
     every pair three times (to reach it, to count its moves, and in the
     simulated nested walk, which may revisit it many times), and a step can
     be costly on an FSM saturated on demand (notes/15: ~165ms an answer on
     [Proc/Test4] until 2026-10-04's one-pass [respond]). *)
  let estimate_by
        ?(cap_factor : int = 4)
        (step : Pair.t -> Pair.t list)
        (root : Pair.t)
    : cost
    =
    let step : Pair.t -> Pair.t list = memo_step step in
    let pairs : Pair.Set.t = reachable_by step root in
    let moves : int =
      Pair.Set.fold (fun p acc -> acc + List.length (step p)) pairs 0
    in
    (* The nested walk, simulated. [path] is the set of coinduction hypotheses
       a nested cofix would have in scope at this point -- the ancestors, and
       only the ancestors. Meeting one of them closes the goal; meeting any
       other already-proved pair does not, and the whole subtree below it is
       walked again. *)
    let cap : int = cap_factor * (Pair.Set.cardinal pairs + moves) in
    let seen : int ref = ref 0 in
    (* [walk path p]: visit [p] below the ancestors [path], counting every
       visit in [seen]; raises [Capped] past [cap]. *)
    let rec walk (path : Pair.Set.t) (p : Pair.t) : unit =
      incr seen;
      if !seen > cap then raise Capped;
      if Pair.Set.mem p path
      then () (* closes against an ancestor *)
      else (
        let path = Pair.Set.add p path in
        List.iter (walk path) (step p))
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

  (* See the [.mli]: {!estimate_by} over {!successors}. *)
  let estimate
        ?(cap_factor : int = 4)
        ?(silent : C.EdgeMap.t' option)
        ?(sim : (C.State.t -> C.State.Set.t) option)
        ~(refl : bool)
        (a : FSM.t)
        (b : FSM.t)
        (pi : C.Partition.t)
        (root : Pair.t)
    : cost
    =
    Logger.trace __FUNCTION__;
    estimate_by ~cap_factor (successors ?silent ?sim ~refl a b pi) root
  ;;

  (* See the [.mli]: {!estimate_by} over {!successors_bisim}. *)
  let estimate_bisim
        ?(cap_factor : int = 4)
        ~(refl : bool)
        (g : game)
        (pi : C.Partition.t)
        (root : Pair.t)
    : cost
    =
    Logger.trace __FUNCTION__;
    estimate_by ~cap_factor (successors_bisim ~refl g pi) root
  ;;

  (* See the [.mli]: {!estimate_by} over the plan's recorded successors. *)
  let estimate_plan ?(cap_factor : int = 4) (p : Policy.plan) : cost =
    Logger.trace __FUNCTION__;
    estimate_by ~cap_factor (Policy.successors p) p.root
  ;;

  (* See the [.mli]. *)
  let prefer_mutual ({ pairs; moves; nested } : cost) : bool =
    match nested with None -> true | Some n -> n > pairs + moves
  ;;
end
