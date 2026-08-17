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

let info () : M.Info.t =
  { meta = None; weak_labels = M.Labels.empty; nums = None }
;;

(** Builds an LTS from a transition list, deriving the state set, alphabet and
    terminals rather than requiring the caller to keep them in sync. *)
let lts (init : int) (ts : M.Transition.t list) : M.LTS.t =
  let transitions =
    List.fold_left (fun acc t -> M.Transitions.add t acc) M.Transitions.empty ts
  in
  let states =
    List.fold_left
      (fun acc (t : M.Transition.t) ->
        M.States.add t.from (M.States.add t.goto acc))
      M.States.empty
      ts
  in
  let alphabet =
    List.fold_left
      (fun acc (t : M.Transition.t) -> M.Labels.add t.label acc)
      M.Labels.empty
      ts
  in
  let sources =
    List.fold_left
      (fun acc (t : M.Transition.t) -> M.States.add t.from acc)
      M.States.empty
      ts
  in
  { init = Some (state init)
  ; alphabet
  ; states
  ; transitions
  ; terminals = M.States.diff states sources
  ; info = info ()
  }
;;

let fsm (init : int) (ts : M.Transition.t list) : M.FSM.t =
  M.FSM.of_lts (lts init ts)
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

(* [Result.are_bisimilar] is [non_bisim_states] being empty, where the merged
   FSM's minimisation partition is split into blocks that contain states from
   both systems and blocks that do not. It is therefore a statement about the
   whole state space, and does not consult [init]: swapping the labels of the
   two systems above yields a pair that is not bisimilar *as rooted systems*
   but still reports true, because every block is still shared. That is sound
   for the plugin, where each FSM is explored outwards from its initial term so
   every state is reachable from the root. The case below distinguishes the two
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

(** Converting an LTS to an FSM must preserve the state set. *)
let test_of_lts_preserves_states () : unit =
  print_endline "FSM.of_lts";
  let l = lts 0 [ transition 0 a 1; transition 1 b 2 ] in
  let f = M.FSM.of_lts l in
  check_int
    "state count preserved"
    (M.States.cardinal l.states)
    (M.States.cardinal f.states);
  check "init preserved" true (Option.equal M.State.equal l.init f.init)
;;

(** A system with no silent labels must be unchanged by saturation. *)
let test_saturate_no_tau () : unit =
  print_endline "saturation: no silent actions";
  let f = fsm 0 [ transition 0 a 1; transition 1 b 0 ] in
  let s = M.FSM.saturate f in
  check_int
    "state count unchanged"
    (M.States.cardinal f.states)
    (M.States.cardinal s.states)
;;

(** Saturation across a silent step must keep every original state. *)
let test_saturate_with_tau () : unit =
  print_endline "saturation: with a silent action";
  let f = fsm 0 [ transition 0 a 1; transition 1 tau 2; transition 2 b 0 ] in
  let s = M.FSM.saturate f in
  check_int
    "state count unchanged by saturation"
    (M.States.cardinal f.states)
    (M.States.cardinal s.states)
;;

(** Minimising an already-minimal system must not lose states. *)
let test_minimize () : unit =
  print_endline "minimization";
  let f = fsm 0 [ transition 0 a 1; transition 1 b 0 ] in
  let { fsm = m; _ } : M.Minimization.t = M.Minimization.fsm f in
  check
    "minimal system keeps at least one state"
    true
    (M.States.cardinal m.states > 0)
;;

(** The JSON round-trip that [Json.S] provides for every model type. *)
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
  test_minimize ();
  test_bisim_identical ();
  test_bisim_different ();
  test_json ();
  Printf.printf "\n%i/%i passed\n" (!total - !failures) !total;
  if !failures > 0 then exit 1
;;
