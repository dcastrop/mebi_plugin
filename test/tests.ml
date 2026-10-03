(** Pure-OCaml tests for the model layer.

    This binary links [rocq-mebi.model] and nothing Rocq-related — no plugin, no
    [rocq-runtime]. That is the point: the model is a plain OCaml library over an
    abstract element type, so it can be exercised directly instead of only
    through a [.v] file driving the plugin.

    Output goes to stdout, because no sink has been installed (see
    [Logger.set_sink], which [src/] calls when running inside Rocq).

    Run with: [dune exec test/tests.exe] *)

(* ------------------------------------------------------------------ *)
(* Instantiate the model over [int].                                    *)

module Base : Base_term.S with type t = int = Base_term.Make (struct
    include Int

    let to_string : int -> string = Printf.sprintf "%i"
  end)

(** [Model.Make]'s last parameter is only stored and serialised, so any
    [Json.S] will do. In the plugin it is [Constructor_bindings]; here there is
    no Rocq provenance to record. *)
module NoBindings : Json.S with type k = unit = Json.Thing.Make (struct
    type k = unit

    let name = "NoBindings"
    let json ?as_elt:_ () : Yojson.t = `Null
  end)

module M = Model.Make (Base) (NoBindings)

(* ------------------------------------------------------------------ *)
(* Helpers for building a small LTS by hand.                            *)

let state (i : int) : M.State.t = { base = i }

let label ?(silent : bool = false) (i : int) : M.Label.t =
  { base = i; is_silent = Some silent }
;;

let transition (from : int) (l : M.Label.t) (goto : int) : M.Transition.t =
  { from = state from
  ; goto = state goto
  ; label = l
  ; tree = None
  ; annotation = None
  }
;;

let info ?(weak_labels : M.Label.Set.t = M.Label.Set.empty) () : M.Info.t =
  { meta = None; weak_labels; nums = None }
;;

(** Builds an LTS from a transition list, deriving the state set, alphabet and
    terminals rather than requiring the caller to keep them in sync.
    [weak_labels] must list which labels are silent for [FSM.saturate] to do
    anything -- it defaults to [only_if_weak:true] and is a no-op otherwise. *)
let lts
      ?(weak_labels : M.Label.Set.t = M.Label.Set.empty)
      (init : int)
      (ts : M.Transition.t list)
  : M.LTS.t
  =
  let transitions =
    List.fold_left
      (fun acc t -> M.Transition.Set.add t acc)
      M.Transition.Set.empty
      ts
  in
  let states =
    List.fold_left
      (fun acc (t : M.Transition.t) ->
        M.State.Set.add t.from (M.State.Set.add t.goto acc))
      M.State.Set.empty
      ts
  in
  let alphabet =
    List.fold_left
      (fun acc (t : M.Transition.t) -> M.Label.Set.add t.label acc)
      M.Label.Set.empty
      ts
  in
  let sources =
    List.fold_left
      (fun acc (t : M.Transition.t) -> M.State.Set.add t.from acc)
      M.State.Set.empty
      ts
  in
  { init = Some (state init)
  ; alphabet
  ; states
  ; transitions
  ; terminals = M.State.Set.diff states sources
  ; info = info ~weak_labels ()
  }
;;

let fsm
      ?(weak_labels : M.Label.Set.t = M.Label.Set.empty)
      (init : int)
      (ts : M.Transition.t list)
  : M.FSM.t
  =
  M.FSM.of_lts (lts ~weak_labels init ts)
;;

(* ------------------------------------------------------------------ *)
(* Test harness.                                                        *)

let failures : int ref = ref 0
let total : int ref = ref 0

let check (name : string) (expected : bool) (actual : bool) : unit =
  incr total;
  if Bool.equal expected actual
  then Printf.printf "  ok    %s\n" name
  else (
    incr failures;
    Printf.printf "  FAIL  %s (expected %b, got %b)\n" name expected actual)
;;

let check_int (name : string) (expected : int) (actual : int) : unit =
  incr total;
  if Int.equal expected actual
  then Printf.printf "  ok    %s\n" name
  else (
    incr failures;
    Printf.printf "  FAIL  %s (expected %i, got %i)\n" name expected actual)
;;

(* ------------------------------------------------------------------ *)
(* Tests.                                                               *)

let a = label 0
let b = label 1
let tau = label ~silent:true 2

(* The two FSMs passed to [Bisimilarity.fsm] are merged before partitioning, so
   they must use disjoint state ids to be treated as separate systems. A state
   present in both is deliberately treated as shared -- see
   [States.origin_of_state], which returns 0 for it. In the plugin this falls
   out of the encoding, which gives distinct terms distinct ids; here it has to
   be arranged by hand. Using {0,1} for both systems would compare a system
   against itself and pass regardless of what the algorithm does. *)

(** Two structurally identical two-state loops over disjoint states. *)
let test_bisim_identical () : unit =
  print_endline "bisimilarity: identical systems";
  let x = fsm 0 [ transition 0 a 1; transition 1 b 0 ] in
  let y = fsm 10 [ transition 10 a 11; transition 11 b 10 ] in
  let r = M.Bisimilarity.fsm x y in
  check
    "identical systems are bisimilar"
    true
    (M.Bisimilarity.Result.are_bisimilar r.result)
;;

(* [Result.are_bisimilar] asks whether the two initial states share a block of
   the merged FSM's partition. Until 2026-10-02 it asked instead whether every
   block held states of both systems, and a comment here argued that this was
   sound because every state is reachable from the root. It was not: the pair
   in [test_bisim_not_rooted] is fully reachable, every block is shared, and
   the two roots are not bisimilar. The case below distinguishes the two
   systems by giving one a behaviour the other cannot match at all. *)

(** A two-state alternation against a one-state self-loop: no matching. *)
let test_bisim_different () : unit =
  print_endline "bisimilarity: unmatchable behaviour";
  let x = fsm 0 [ transition 0 a 1; transition 1 b 0 ] in
  let y = fsm 10 [ transition 10 a 10 ] in
  let r = M.Bisimilarity.fsm x y in
  check
    "systems with unmatchable behaviour are not bisimilar"
    false
    (M.Bisimilarity.Result.are_bisimilar r.result)
;;

(** [a.b.x] against [b.a.y]: both blocks of the partition, {x, b.y} and
    {b.x, y}, hold states of both systems, but the roots are in different
    blocks. *)
let test_bisim_not_rooted () : unit =
  print_endline "bisimilarity: rooted";
  let x = fsm 0 [ transition 0 a 1; transition 1 b 0 ] in
  let y = fsm 10 [ transition 10 b 11; transition 11 a 10 ] in
  let r = M.Bisimilarity.fsm x y in
  check
    "systems whose roots differ are not bisimilar"
    false
    (M.Bisimilarity.Result.are_bisimilar r.result)
;;

(** Milner's [τ.a + b] against [a + b], with a [c]-branch to [a] on each side
    so that every state has a partner. Their visible weak moves coincide, so
    only the [=ε=>] split separates them. *)
let test_bisim_silent_closure () : unit =
  print_endline "bisimilarity: silent closure";
  let c = label 3 in
  let weak_labels = M.Label.Set.singleton tau in
  let x =
    fsm
      ~weak_labels
      0
      [ transition 0 tau 1
      ; transition 0 b 2
      ; transition 0 c 1
      ; transition 1 a 2
      ]
  in
  let y =
    fsm
      ~weak_labels
      10
      [ transition 10 a 12
      ; transition 10 b 12
      ; transition 10 c 11
      ; transition 11 a 12
      ]
  in
  check
    "tau.a + b + c.a and a + b + c.a are not weakly bisimilar"
    false
    (M.Bisimilarity.Result.are_bisimilar (M.Bisimilarity.fsm x y).result);
  (* [a] against [τ.a]: weakly bisimilar. Without the reflexive step in
     [=ε=>], [τ.a] would reach a block by silence and [a] none, and the split
     would separate them. *)
  let u = fsm ~weak_labels 20 [ transition 20 a 21 ] in
  let v = fsm ~weak_labels 30 [ transition 30 tau 31; transition 31 a 32 ] in
  check
    "a and tau.a are weakly bisimilar"
    true
    (M.Bisimilarity.Result.are_bisimilar (M.Bisimilarity.fsm u v).result)
;;

(** [respond] for a silent move that standing still cannot answer: [10 -τ-> 11 -τ-> 12], only [12] is acceptable, so the answer is two silent steps
    -- given the unsaturated edges, and nothing without them. *)
let test_product_respond_silently () : unit =
  print_endline "product: silent move answered by moving";
  let weak_labels = M.Label.Set.singleton tau in
  let b = fsm ~weak_labels 10 [ transition 10 tau 11; transition 11 tau 12 ] in
  let b' = M.FSM.saturate b in
  let only_12 = M.State.Set.singleton (state 12) in
  let t = M.Product.respond ~silent:b.edges b' (state 10) tau only_12 in
  check "moves to the acceptable state" true (M.State.equal t.goto (state 12));
  check_int
    "along the shortest silent path"
    2
    (match t.annotation with
     | None -> 0
     | Some ann ->
       let rec len (a : M.Annotation.t) =
         match a.next with None -> 1 | Some n -> 1 + len n
       in
       len ann);
  check
    "without the unsaturated edges there is no answer"
    true
    (match M.Product.respond b' (state 10) tau only_12 with
     | _ -> false
     | exception M.Product.NoBisimilarResponse _ -> true)
;;

(** The bisimulation game adds the right-hand system's obligations. [0 -a-> 1] against [10 -a-> 11] and [10 -a-> 12]: the simulation game answers
    [0]'s move once, landing on [(1, 11)]; the bisimulation game must also
    answer both of [10]'s moves from [0], which adds [(1, 12)]. *)
let test_product_bisim () : unit =
  print_endline "product: bisimulation game";
  let a' = fsm 0 [ transition 0 a 1 ] in
  let b' = fsm 10 [ transition 10 a 11; transition 10 a 12 ] in
  let pi = (M.Bisimilarity.fsm a' b').result.bisim_states in
  let g : M.Product.game =
    { a = a'; a_saturated = a'; b = b'; b_saturated = b' }
  in
  let root = state 0, state 10 in
  check_int
    "simulation: two pairs"
    2
    (M.Product.Pair.Set.cardinal
       (M.Product.reachable ~refl:false a' b' pi root));
  let pairs = M.Product.reachable_bisim ~refl:false g pi root in
  check_int "bisimulation: three pairs" 3 (M.Product.Pair.Set.cardinal pairs);
  check
    "including the right-hand system's second move"
    true
    (M.Product.Pair.Set.mem (state 1, state 12) pairs)
;;

(** The weak simulation preorder: [a.b] (0) is simulated by [a.(b + c)] (10),
    not the converse; and with a silent step, [tau.a] (20) and [a] (30)
    simulate each other. *)
let test_product_simulation () : unit =
  print_endline "product: weak simulation preorder";
  let c = label 3 in
  let weak_labels = M.Label.Set.singleton tau in
  let x = fsm ~weak_labels 0 [ transition 0 a 1; transition 1 b 2 ] in
  let y =
    fsm
      ~weak_labels
      10
      [ transition 10 a 11; transition 11 b 12; transition 11 c 12 ]
  in
  let sim f g = M.Product.simulation f g (M.FSM.saturate g) in
  check
    "a.b <= a.(b + c)"
    true
    (M.Product.Pair.Set.mem (state 0, state 10) (sim x y));
  check
    "not a.(b + c) <= a.b"
    false
    (M.Product.Pair.Set.mem (state 10, state 0) (sim y x));
  let u = fsm ~weak_labels 20 [ transition 20 tau 21; transition 21 a 22 ] in
  let v = fsm ~weak_labels 30 [ transition 30 a 31 ] in
  check
    "tau.a <= a"
    true
    (M.Product.Pair.Set.mem (state 20, state 30) (sim u v));
  check
    "a <= tau.a"
    true
    (M.Product.Pair.Set.mem (state 30, state 20) (sim v u))
;;

(** Answer policies (measurement only). On the bisimulation game of
    [test_product_bisim], [Default] must reproduce the product exactly, and
    no policy may leave a move unanswered or do worse than [Default] on
    pairs when it is [Minimal]. *)
let test_product_policies () : unit =
  print_endline "product: answer policies";
  let a' = fsm 0 [ transition 0 a 1 ] in
  let b' = fsm 10 [ transition 10 a 11; transition 10 a 12 ] in
  let pi = (M.Bisimilarity.fsm a' b').result.bisim_states in
  let g : M.Product.game =
    { a = a'; a_saturated = a'; b = b'; b_saturated = b' }
  in
  let root = state 0, state 10 in
  let game = M.Product.Policy.bisim_game ~refl:false g pi in
  let m p = M.Product.Policy.measure p game root in
  let d = m M.Product.Policy.Default in
  check_int
    "default reproduces the product"
    (M.Product.Pair.Set.cardinal
       (M.Product.reachable_bisim ~refl:false g pi root))
    d.pairs;
  List.iter
    (fun p ->
      let r = m p in
      check_int
        (M.Product.Policy.name p ^ ": every move answered")
        0
        r.unanswered)
    M.Product.Policy.[ Default; Greedy; Minimal ];
  check
    "minimal is no larger than default"
    true
    ((m M.Product.Policy.Minimal).pairs <= d.pairs)
;;

(** Converting an LTS to an FSM must preserve the state set. *)
let test_of_lts_preserves_states () : unit =
  print_endline "FSM.of_lts";
  let l = lts 0 [ transition 0 a 1; transition 1 b 2 ] in
  let f = M.FSM.of_lts l in
  check_int
    "state count preserved"
    (M.State.Set.cardinal l.states)
    (M.State.Set.cardinal f.states);
  check "init preserved" true (Option.equal M.State.equal l.init f.init)
;;

(** A system with no silent labels must be unchanged by saturation. *)
let test_saturate_no_tau () : unit =
  print_endline "saturation: no silent actions";
  let f = fsm 0 [ transition 0 a 1; transition 1 b 0 ] in
  let s = M.FSM.saturate f in
  check_int
    "state count unchanged"
    (M.State.Set.cardinal f.states)
    (M.State.Set.cardinal s.states)
;;

(** Saturation across a silent step must keep every original state.
    [~weak_labels] is required here -- without it [FSM.saturate] (default
    [only_if_weak:true]) sees an empty [Info.weak_labels], treats the FSM as
    not in weak mode, and returns it unchanged, which this test's assertion
    (state count preserved) cannot distinguish from actually saturating.
    Found missing, 2026-09-27, while adding
    [test_saturate_multi_destination_action] below -- this test predates
    [~weak_labels] existing on these helpers and was passing vacuously. *)
let test_saturate_with_tau () : unit =
  print_endline "saturation: with a silent action";
  let f =
    fsm
      ~weak_labels:(M.Label.Set.singleton tau)
      0
      [ transition 0 a 1; transition 1 tau 2; transition 2 b 0 ]
  in
  let s = M.FSM.saturate f in
  check_int
    "state count unchanged by saturation"
    (M.State.Set.cardinal f.states)
    (M.State.Set.cardinal s.states)
;;

(** A single silent action with two destinations must keep both after
    saturation — regression test for [Saturation.edge_action_destinations]
    silently dropping all but the last-visited destination when one action
    genuinely branches to more than one state (see [ASSISTED-CHANGES.md],
    2026-09-27). State 0's one [tau] action reaches both 1 and 2, which
    then diverge under different visible labels ([a] to 3, [b] to 4); both
    weak transitions from state 0 must survive. *)
let test_saturate_multi_destination_action () : unit =
  print_endline "saturation: one action with two destinations";
  let f =
    fsm
      ~weak_labels:(M.Label.Set.singleton tau)
      0
      [ transition 0 tau 1
      ; transition 0 tau 2
      ; transition 1 a 3
      ; transition 2 b 4
      ]
  in
  let s = M.FSM.saturate f in
  let has_weak_transition (from_i : int) (l : M.Label.t) (goto_i : int) : bool =
    match M.EdgeMap.find_opt s.edges (state from_i) with
    | None -> false
    | Some actions ->
      M.Action.Map.to_seq actions
      |> Seq.exists (fun ((act, dests) : M.Action.t * M.State.Set.t) ->
        M.Label.equal act.label l && M.State.Set.mem (state goto_i) dests)
  in
  check
    "weak transition 0 -a-> 3 (via state 1) survives"
    true
    (has_weak_transition 0 a 3);
  check
    "weak transition 0 -b-> 4 (via state 2) survives"
    true
    (has_weak_transition 0 b 4)
;;

(** Minimising an already-minimal system must not lose states. *)
let test_minimize () : unit =
  print_endline "minimization";
  let f = fsm 0 [ transition 0 a 1; transition 1 b 0 ] in
  let { fsm = m; _ } : M.Minimization.t = M.Minimization.fsm f in
  check
    "minimal system keeps at least one state"
    true
    (M.State.Set.cardinal m.states > 0)
;;

(* ------------------------------------------------------------------ *)
(* Product: the simulation game's response choice, and the relation it
   generates. See [lib/model/algorithms/product.ml] and backlog item B2. *)

(** Runs the bisimilarity check and hands back exactly what the proof solver
    reads: the UNSATURATED left-hand FSM (its transitions are the
    obligations), the SATURATED right-hand FSM (its actions are the weak
    transitions available in reply) and the bisimilar partition. *)
let game (x : M.FSM.t) (y : M.FSM.t) : M.FSM.t * M.FSM.t * M.Partition.t =
  let r : M.Bisimilarity.t = M.Bisimilarity.fsm x y in
  r.fsm_a.original, r.fsm_b.saturated, r.result.bisim_states
;;

let reachable (x : M.FSM.t) (y : M.FSM.t) (root : int * int)
  : M.Product.Pair.Set.t
  =
  let a, b, pi = game x y in
  M.Product.reachable ~refl:false a b pi (state (fst root), state (snd root))
;;

(** Two matching loops: the product is a loop of the same length, and the
    breadth-first closure must terminate on it rather than going round. *)
let test_product_loop () : unit =
  print_endline "product: matching loops";
  let x = fsm 0 [ transition 0 a 1; transition 1 b 0 ] in
  let y = fsm 10 [ transition 10 a 11; transition 11 b 10 ] in
  check_int
    "a two-state loop against itself gives two game states"
    2
    (M.Product.Pair.Set.cardinal (reachable x y (0, 10)))
;;

(** The case the proof search gets wrong. Two routes out of the root rejoin
    at a common state, so the product is a diamond rather than a tree. The
    closure must return the meeting point ONCE; the solver's depth-first
    walk re-derives it on the second route, which is what makes
    [Proc/Test3] enumerate paths instead of pairs. *)
let test_product_diamond () : unit =
  print_endline "product: diamond rejoins once";
  let x =
    fsm
      0
      [ transition 0 a 1; transition 0 b 2; transition 1 b 3; transition 2 a 3 ]
  in
  let y =
    fsm
      10
      [ transition 10 a 11
      ; transition 10 b 12
      ; transition 11 b 13
      ; transition 12 a 13
      ]
  in
  let a', b', pi = game x y in
  let pairs = M.Product.reachable ~refl:false a' b' pi (state 0, state 10) in
  check_int
    "a diamond gives four game states"
    4
    (M.Product.Pair.Set.cardinal pairs);
  check
    "the meeting point is present"
    true
    (M.Product.Pair.Set.mem (state 3, state 13) pairs);
  (* The point of the closure. Counting the moves instead of the game states
     gives more moves than a tree over these states could have, and two of
     them land on the meeting point -- so a walk that can only close against
     its own ancestors has to prove that state twice. That is exactly what
     [Proc/Test3] does 806 times over in 20,000 iterations. *)
  let moves =
    M.Product.Pair.Set.fold
      (fun p acc -> M.Product.successors ~refl:false a' b' pi p @ acc)
      pairs
      []
  in
  check_int "the diamond has four moves over four states" 4 (List.length moves);
  check_int
    "two of them land on the meeting point"
    2
    (List.length
       (List.filter
          (fun (p : M.Product.Pair.t) ->
            M.Product.Pair.equal p (state 3, state 13))
          moves))
;;

(** A silent move to a state already bisimilar to the right-hand one is
    answered by standing still, so the right-hand component does not change.
    Mirrors [Proof_solver_step.handle_wk_concl]'s [wk_none] branch. *)
let test_product_silent_stays_put () : unit =
  print_endline "product: silent move stands still";
  let weak_labels = M.Label.Set.singleton tau in
  let x = fsm ~weak_labels 0 [ transition 0 tau 1; transition 1 a 0 ] in
  let y = fsm ~weak_labels 10 [ transition 10 a 10 ] in
  let a', b', pi = game x y in
  let succs =
    M.Product.successors ~refl:false a' b' pi (state 0, state 10)
    |> List.filter (fun ((_, r) : M.Product.Pair.t) ->
      M.State.equal r (state 10))
  in
  check
    "the silent obligation is answered without moving"
    true
    (List.exists
       (fun ((l, _) : M.Product.Pair.t) -> M.State.equal l (state 1))
       succs)
;;

(** Both sides drawn from ONE LTS, converging on a shared loop -- the shape of
    [Proc/Test1]'s [wsim_pr], where [r] is one step of [q]'s unfolding. Once
    the game reaches [(2, 2)] the solver closes it by [weak_sim_refl], so with
    [~refl:true] it must be a leaf; without, the loop behind it is enumerated
    as pairs the proof never visits. *)
let test_product_refl_leaf () : unit =
  print_endline "product: equal states close by reflexivity";
  let f =
    fsm
      0
      [ transition 0 a 2; transition 1 a 2; transition 2 b 3; transition 3 a 2 ]
  in
  let a', b', pi = game f f in
  let root = state 0, state 1 in
  let with_refl = M.Product.reachable ~refl:true a' b' pi root in
  check_int
    "the shared state is reached and stops there"
    2
    (M.Product.Pair.Set.cardinal with_refl);
  check
    "the leaf is still in the relation"
    true
    (M.Product.Pair.Set.mem (state 2, state 2) with_refl);
  check_int
    "two different LTSs keep walking"
    3
    (M.Product.Pair.Set.cardinal
       (M.Product.reachable ~refl:false a' b' pi root));
  check_int
    "and the estimate agrees: one move, not three"
    1
    (M.Product.estimate ~refl:true a' b' pi root).moves
;;

(** [respond] answers with a state bisimilar to the one it was asked for,
    and raises rather than guessing when there is no such action. *)
let test_product_respond () : unit =
  print_endline "product: respond";
  let x = fsm 0 [ transition 0 a 1; transition 1 b 0 ] in
  let y = fsm 10 [ transition 10 a 11; transition 11 b 10 ] in
  let _, b', pi = game x y in
  let bisim_with_1 = M.Partition.get_bisimilar (state 1) pi in
  let t : M.Transition.t = M.Product.respond b' (state 10) a bisim_with_1 in
  check
    "respond lands on the bisimilar state"
    true
    (M.State.equal t.goto (state 11));
  check
    "respond raises when no action carries the label"
    true
    (match M.Product.respond b' (state 10) b bisim_with_1 with
     | _ -> false
     | exception M.Product.NoBisimilarResponse _ -> true)
;;

(** [estimate] must separate the two strategies without running a proof: the
    same cost on a tree, a higher nested cost on a diamond, and [None] --
    "will not finish" -- when the nested walk passes its cap. *)
let test_product_estimate () : unit =
  print_endline "product: cost estimate";
  (* A loop: the product is a cycle, every repeat is an ancestor, so a nested
     walk closes it exactly where a mutual cofix would. *)
  let x = fsm 0 [ transition 0 a 1; transition 1 b 0 ] in
  let y = fsm 10 [ transition 10 a 11; transition 11 b 10 ] in
  let a', b', pi = game x y in
  let c = M.Product.estimate ~refl:false a' b' pi (state 0, state 10) in
  check_int "a loop has two game states" 2 c.pairs;
  check
    "on a loop the nested walk costs no more than the moves"
    true
    (match c.nested with Some n -> n <= c.pairs + c.moves | None -> false);
  check
    "prefer_mutual leaves a loop on the nested cofix"
    false
    (M.Product.prefer_mutual c);
  (* A diamond: two routes meet, and only one of them can be an ancestor, so
     the nested walk pays for the meeting point twice. *)
  let x =
    fsm
      0
      [ transition 0 a 1; transition 0 b 2; transition 1 b 3; transition 2 a 3 ]
  in
  let y =
    fsm
      10
      [ transition 10 a 11
      ; transition 10 b 12
      ; transition 11 b 13
      ; transition 12 a 13
      ]
  in
  let a', b', pi = game x y in
  let d = M.Product.estimate ~refl:false a' b' pi (state 0, state 10) in
  check_int "a diamond has four game states" 4 d.pairs;
  check
    "on a diamond the nested walk costs strictly more than the mutual one"
    true
    (match d.nested with Some n -> n > d.pairs && n > 0 | None -> false);
  (* One diamond is not enough to prefer a mutual cofix, and the estimate says
     so: the nested walk pays for the meeting point twice, five goals in all,
     where a mutual block would prove four pairs and close four moves. Small
     graphs favour the nested strategy, which is exactly why six of the
     repository's eighteen cheap proofs get slower under a mutual cofix. *)
  check
    "one diamond stays on the nested cofix"
    false
    (M.Product.prefer_mutual d);
  check
    "the cap reports non-termination rather than hanging"
    true
    (match
       M.Product.estimate ~cap_factor:0 ~refl:false a' b' pi (state 0, state 10)
     with
     | { nested = None; _ } -> true
     | _ -> false);
  (* Diamonds in series are what tips it: each one doubles the nested walk
     while adding only a constant to the mutual cost. Three of them already
     cross over. *)
  let chain (o : int) : M.Transition.t list =
    List.concat_map
      (fun (i : int) ->
        let n k = o + (3 * i) + k in
        [ transition (n 0) a (n 1)
        ; transition (n 0) b (n 2)
        ; transition (n 1) b (n 3)
        ; transition (n 2) a (n 3)
        ])
      [ 0; 1; 2 ]
  in
  let xa = fsm 0 (chain 0) in
  let yb = fsm 100 (chain 100) in
  let a', b', pi = game xa yb in
  let e = M.Product.estimate ~refl:false a' b' pi (state 0, state 100) in
  check_int "three diamonds give ten game states" 10 e.pairs;
  check_int "and twelve moves" 12 e.moves;
  check
    "the nested walk costs more than the mutual one"
    true
    (match e.nested with Some n -> n > e.pairs + e.moves | None -> true);
  check
    "so prefer_mutual picks the mutual cofix"
    true
    (M.Product.prefer_mutual e)
;;

(** The JSON round-trip that [Json.S] provides for every model type. *)
(* SaturationEstimate: the size of [FSM.saturate]'s output, computed on the
   silent-SCC quotient without saturating. The reference is the saturation
   itself -- the number of distinct [(from, label, goto)] it materialises. *)

let saturated_triples (f : M.FSM.t) : int =
  let s = M.FSM.saturate f in
  M.EdgeMap.fold
    (fun (from : M.State.t) actions acc ->
      M.Action.Map.fold
        (fun (x : M.Action.t) ds acc ->
          M.State.Set.fold
            (fun (goto : M.State.t) acc ->
              (from.base, x.label.base, goto.base) :: acc)
            ds
            acc)
        actions
        acc)
    s.edges
    []
  |> List.sort_uniq compare
  |> List.length
;;

(** [k] silent cycles of [m] states each, chained by silent edges (cycle [i]
    to cycle [i + 1]), with every state also able to do [a] back to the first
    state of its own cycle. From cycle [i], [tau* a tau*] reaches cycles [i]
    to [k - 1], so the saturation has [m^2 * k(k+1)/2] weak actions. *)
let chained_cycles (k : int) (m : int) : M.FSM.t =
  let st (i : int) (j : int) : int = (i * m) + j in
  let ts =
    List.concat_map
      (fun i ->
        List.concat_map
          (fun j ->
            [ transition (st i j) tau (st i ((j + 1) mod m))
            ; transition (st i j) a (st i 0)
            ]
            @
            if i + 1 < k && j = 0
            then [ transition (st i 0) tau (st (i + 1) 0) ]
            else [])
          (List.init m Fun.id))
      (List.init k Fun.id)
  in
  fsm ~weak_labels:(M.Label.Set.singleton tau) 0 ts
;;

let test_saturation_estimate () : unit =
  print_endline "saturation estimate";
  let small = chained_cycles 3 4 in
  let e = M.SaturationEstimate.fsm small in
  check_int "chained cycles: one SCC per cycle" 3 e.sccs;
  check_int "chained cycles: largest SCC" 4 e.largest_scc;
  check_int "chained cycles: weak = m^2 k(k+1)/2" (16 * 6) e.weak;
  check_int "chained cycles: weak = saturated" (saturated_triples small) e.weak;
  (* [Proc/Test4]'s shape (81 silent SCCs of ~120 states) at about its
     size: far too big to saturate here, but the estimate is immediate. *)
  let big = M.SaturationEstimate.fsm (chained_cycles 81 120) in
  check_int "81 x 120 chained cycles: weak" (14400 * 81 * 82 / 2) big.weak;
  (* Differential: small pseudo-random LTSs, label 0 silent. *)
  let rng = Random.State.make [| 2026; 10; 1 |] in
  let mismatches = ref 0 in
  let nonempty = ref 0 in
  for _ = 1 to 300 do
    let n = 2 + Random.State.int rng 7 in
    let ts =
      List.init
        (n * (1 + Random.State.int rng 3))
        (fun _ ->
          let l = Random.State.int rng 3 in
          transition
            (Random.State.int rng n)
            (if l = 0 then tau else label l)
            (Random.State.int rng n))
    in
    let f = fsm ~weak_labels:(M.Label.Set.singleton tau) 0 ts in
    let expected = saturated_triples f in
    if expected > 0 then incr nonempty;
    if (M.SaturationEstimate.fsm f).weak <> expected then incr mismatches
  done;
  check_int "300 random LTSs: estimate = saturated" 0 !mismatches;
  check "random LTSs are mostly non-trivial" true (!nonempty > 250)
;;

(** A canonical rendering of one state's saturated actions, for comparing
    two saturations ([EdgeMap] and [Action.Map] are hash tables, so their
    order is not a contract). As in [satdiff.ml]. *)
let render_actions (x : M.Action.Map.t' option) : string =
  match x with
  | None -> "none"
  | Some actions ->
    M.Action.Map.to_seq actions
    |> List.of_seq
    |> List.map (fun ((act, dests) : M.Action.t * M.State.Set.t) ->
      Printf.sprintf
        "%i %s -> {%s}"
        act.label.base
        (match act.annotation with
         | None -> "-"
         | Some a -> M.Annotation.to_string ~pretty:false a)
        (M.State.Set.elements dests
         |> List.map (fun (s : M.State.t) -> string_of_int s.base)
         |> String.concat ","))
    |> List.sort compare
    |> String.concat "; "
;;

(** Small pseudo-random LTSs with label 0 silent, as in
    [test_saturation_estimate]. *)
let random_fsms (seed : int array) (count : int) : M.FSM.t list =
  let rng = Random.State.make seed in
  List.init count (fun _ ->
    let n = 2 + Random.State.int rng 7 in
    let ts =
      List.init
        (n * (1 + Random.State.int rng 3))
        (fun _ ->
          let l = Random.State.int rng 3 in
          transition
            (Random.State.int rng n)
            (if l = 0 then tau else label l)
            (Random.State.int rng n))
    in
    fsm ~weak_labels:(M.Label.Set.singleton tau) 0 ts)
;;

(* note 13: an FSM saturated on demand answers every state exactly as the
   FSM saturated whole, whatever the budget and whatever order states are
   asked in; and the partition computed on the silent-SCC quotient is the
   one computed on the saturated FSM. *)
let test_on_demand_saturation () : unit =
  print_endline "on-demand saturation";
  let mismatches = ref 0 in
  List.iter
    (fun (f : M.FSM.t) ->
      let full = M.FSM.saturate f in
      List.iter
        (fun budget ->
          let od = M.FSM.saturate_on_demand ~budget f in
          (* twice over, the second time in reverse, so that evicted states
             are recomputed *)
          let states = M.State.Set.elements f.states in
          List.iter
            (fun (s : M.State.t) ->
              M.FSM.ensure od s;
              if
                render_actions (M.EdgeMap.find_opt od.edges s)
                <> render_actions (M.EdgeMap.find_opt full.edges s)
              then incr mismatches)
            (states @ List.rev states))
        [ 1_000_000; 1 ])
    (random_fsms [| 2026; 10; 3 |] 300);
  check_int "300 random LTSs, budgets 1M and 1: on demand = whole" 0 !mismatches;
  (* without silent labels there is nothing to saturate *)
  let strong = fsm 0 [ transition 0 a 1 ] in
  check
    "not weak: unchanged"
    true
    (Option.is_none (M.FSM.saturate_on_demand strong).fill)
;;

let test_quotient_partition () : unit =
  print_endline "quotient partition";
  let reference (f : M.FSM.t) : M.Partition.t =
    M.Minimization.partition_states ~silent:f.edges (M.FSM.saturate f)
  in
  let mismatches = ref 0 in
  let split = ref 0 in
  let fs = random_fsms [| 2026; 10; 3; 1 |] 300 in
  List.iter
    (fun (f : M.FSM.t) ->
      let p = M.SaturationEstimate.partition f in
      if M.Partition.cardinal p > 1 then incr split;
      if Bool.not (M.Partition.equal p (reference f)) then incr mismatches)
    fs;
  (* and on pairs merged, as [Bisimilarity.fsm] partitions them: the second
     FSM's states renumbered apart from the first's *)
  let shift (f : M.FSM.t) : M.FSM.t =
    let moved =
      M.EdgeMap.fold
        (fun (from : M.State.t) (actions : M.Action.Map.t') acc ->
          M.Action.Map.fold
            (fun (act : M.Action.t) (ds : M.State.Set.t) acc ->
              M.State.Set.fold
                (fun (d : M.State.t) acc ->
                  transition (from.base + 100) act.label (d.base + 100) :: acc)
                ds
                acc)
            actions
            acc)
        f.edges
        []
    in
    fsm ~weak_labels:(M.Label.Set.singleton tau) 100 moved
  in
  let rec pairs = function
    | x :: y :: tl -> (x, shift y) :: pairs tl
    | _ -> []
  in
  List.iter
    (fun ((x, y) : M.FSM.t * M.FSM.t) ->
      let m = M.FSM.merge x y in
      if
        Bool.not
          (M.Partition.equal (M.SaturationEstimate.partition m) (reference m))
      then incr mismatches)
    (pairs fs);
  check_int
    "300 random LTSs and 150 merged pairs: quotient = saturated"
    0
    !mismatches;
  check "random partitions are mostly non-trivial" true (!split > 150);
  (* Milner's pair, by hand: [tau.a + b] and [a + b] are not weakly
     bisimilar *)
  let m =
    fsm
      ~weak_labels:(M.Label.Set.singleton tau)
      0
      [ transition 0 tau 1
      ; transition 1 a 2
      ; transition 0 b 3
      ; transition 10 a 12
      ; transition 10 b 13
      ]
  in
  let p = M.SaturationEstimate.partition m in
  check
    "Milner's pair: roots apart"
    false
    (M.State.Set.mem (state 10) (M.Partition.get_bisimilar (state 0) p))
;;

(* 2026-10-03: two systems whose states share terms. A shared state with
   the same moves on both sides is one state (same relation); with
   different moves it is a conflict, which [Bisimilarity.fsm] would merge. *)
let test_conflicts () : unit =
  print_endline "conflicting shared states";
  let lin = fsm 0 [ transition 0 a 1 ] in
  let other = fsm 0 [ transition 0 b 1 ] in
  let same = fsm 0 [ transition 0 a 1 ] in
  let apart = fsm 10 [ transition 10 b 11 ] in
  let c = M.Bisimilarity.conflicts lin other in
  check "a vs b from a shared 0: 0 conflicts" true (M.State.Set.mem (state 0) c);
  check
    "... and 1 does not (no moves either side)"
    false
    (M.State.Set.mem (state 1) c);
  check
    "same moves: no conflict"
    true
    (M.State.Set.is_empty (M.Bisimilarity.conflicts lin same));
  check
    "numbered apart: no conflict"
    true
    (M.State.Set.is_empty (M.Bisimilarity.conflicts lin apart))
;;

(* ------------------------------------------------------------------ *)
(* lib/terms: constructor trees and the encoding counter (backlog E(c)). *)

module Tree = Base.Tree
module Trees = Base.Trees

let node (enc : int) (i : int) : Tree.Node.t = enc, i
let leaf (enc : int) (i : int) : Tree.t = Tree.N (node enc i, [])

let nodes_equal (a : Tree.Node.t list) (b : Tree.Node.t list) : bool =
  List.equal Tree.Node.equal a b
;;

let test_tree_order () : unit =
  print_endline "terms: tree equality and order";
  let t = Tree.N (node 1 0, [ leaf 2 1; leaf 3 0 ]) in
  let t' = Tree.N (node 1 0, [ leaf 2 1; leaf 3 0 ]) in
  let u = Tree.N (node 1 0, [ leaf 2 1; leaf 3 1 ]) in
  check "structurally equal trees are equal" true (Tree.equal t t');
  check_int "and compare as 0" 0 (Tree.compare t t');
  check
    "a different constructor index deep down is not equal"
    false
    (Tree.equal t u);
  check
    "compare is antisymmetric"
    true
    (Int.equal (compare (Tree.compare t u) 0) (-compare (Tree.compare u t) 0));
  (* Trees is a Set: equal trees collapse. *)
  check_int
    "Trees dedups equal trees"
    2
    (Trees.cardinal (Trees.of_list [ t; t'; u ]));
  let c : Base.Constructor_tree.t = 1, 2, t in
  check
    "Constructor_tree: equal on equal parts"
    true
    (Base.Constructor_tree.equal c (1, 2, t'));
  check
    "Constructor_tree: differs if the tree differs"
    false
    (Base.Constructor_tree.equal c (1, 2, u))
;;

(** [Tree.preorder] is what the proof solver applies, node by node
    ([Proof_solver_step.handle_appconstrs_update_args]). A node's children
    are its constructor's premises, all required, so all of them are
    replayed, depth-first, left to right -- the order Rocq focuses premise
    goals in. It replaced [minimize], which kept only the shortest child
    (backlog A6; [theories/Test.v]'s [TwoPremises]). *)
let test_tree_preorder () : unit =
  print_endline "terms: Tree.preorder, Tree.size and Trees.min";
  let chain = Tree.N (node 1 0, [ Tree.N (node 2 1, [ leaf 3 2 ]) ]) in
  check
    "a chain flattens to its nodes, root first"
    true
    (nodes_equal [ node 1 0; node 2 1; node 3 2 ] (Tree.preorder chain));
  let two = Tree.N (node 1 0, [ leaf 2 0; leaf 3 1 ]) in
  check
    "two premises: both children, in order"
    true
    (nodes_equal [ node 1 0; node 2 0; node 3 1 ] (Tree.preorder two));
  (* A two-premise node under a two-premise node: the first premise's whole
     subtree comes before the second premise. *)
  let nested = Tree.N (node 9 0, [ two; leaf 4 2 ]) in
  check
    "nested: depth-first, left to right"
    true
    (nodes_equal
       [ node 9 0; node 1 0; node 2 0; node 3 1; node 4 2 ]
       (Tree.preorder nested));
  check_int "size counts every node" 5 (Tree.size nested);
  check
    "size = length of preorder"
    true
    (List.for_all
       (fun t -> Int.equal (Tree.size t) (List.length (Tree.preorder t)))
       [ chain; two; nested; leaf 1 0 ]);
  (* [Trees.min] chooses among alternative derivations by size. *)
  let short = leaf 5 0 in
  check
    "Trees.min picks the derivation with the fewest constructors"
    true
    (Tree.equal short (Trees.min (Trees.of_list [ chain; two; short ])));
  check
    "a wide tree is not 'short' because one branch is"
    true
    (Tree.equal chain (Trees.min (Trees.of_list [ chain; nested ])));
  check
    "Trees.min_opt of empty is None"
    true
    (Option.is_none (Trees.min_opt Trees.empty));
  check
    "Trees.min of empty raises"
    true
    (match Trees.min Trees.empty with
     | _ -> false
     | exception Trees.EmptyHasNoMin -> true)
;;

(** [Bi_encoding] hands out encodings with [incr] and restarts them with
    [reset]. *)
let test_encoding_counter () : unit =
  print_endline "terms: encoding counter";
  let module E = Encoding.Packed.Unpack (Encoding.Packed.Int) in
  E.reset ();
  let a = E.incr () in
  let b = E.incr () in
  check_int "first encoding is init" E.init a;
  check_int "then next init" (E.next E.init) b;
  check "encodings are distinct" false (E.equal a b);
  E.reset ();
  check_int "reset restarts at init" E.init (E.incr ())
;;

let test_json () : unit =
  print_endline "json serialisation";
  let f = fsm 0 [ transition 0 a 1 ] in
  let s = M.FSM.to_string ~pretty:false f in
  check "FSM serialises to non-empty json" true (String.length s > 2);
  check
    "state serialises"
    true
    (String.length (M.State.to_string ~pretty:false (state 0)) > 2)
;;

let () =
  print_endline "\n=== mebi model tests (pure OCaml, no Rocq) ===\n";
  test_of_lts_preserves_states ();
  test_saturate_no_tau ();
  test_saturate_with_tau ();
  test_saturate_multi_destination_action ();
  test_minimize ();
  test_bisim_identical ();
  test_bisim_different ();
  test_bisim_not_rooted ();
  test_bisim_silent_closure ();
  test_product_loop ();
  test_product_diamond ();
  test_product_silent_stays_put ();
  test_product_refl_leaf ();
  test_product_respond ();
  test_product_respond_silently ();
  test_product_bisim ();
  test_product_simulation ();
  test_product_policies ();
  test_product_estimate ();
  test_saturation_estimate ();
  test_on_demand_saturation ();
  test_quotient_partition ();
  test_conflicts ();
  test_tree_order ();
  test_tree_preorder ();
  test_encoding_counter ();
  test_json ();
  Printf.printf "\n%i/%i passed\n" (!total - !failures) !total;
  if !failures > 0 then exit 1
;;
