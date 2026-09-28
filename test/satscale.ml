(** Scaling probe for [Saturation]: does it cost what the STATE space costs, or
    what the PATH space costs?

    Infrastructure, not plugin capability — a throwaway measurement harness in
    the same vein as [tests.ml], linking [rocq-mebi.model] only.

    The shape measured is a k x k grid of silent transitions: from (i,j) there
    is a silent step right to (i,j+1) and down to (i+1,j), with one visible
    action off the far corner. This is exactly what parallel interleaving
    produces — [Layered.compLTS]'s [do_parl] and [do_parr] let either side of a
    [cpar] move, so the reachable silent structure between two components is a
    grid.

    The point of the shape: states grow as (k+1)^2 while the number of distinct
    simple paths from (0,0) to (k,k) grows as C(2k,k). If saturation is
    explanation-by-state-space, the time tracks the former; if it enumerates
    paths, it tracks the latter. *)

module Base : Base_term.S with type t = int = Base_term.Make (struct
    include Int

    let to_string : int -> string = Printf.sprintf "%i"
  end)

module NoBindings : Json.S with type k = unit = Json.Thing.Make (struct
    type k = unit

    let name = "NoBindings"
    let json ?as_elt:_ () : Yojson.t = `Null
  end)

module M = Model.Make (Base) (NoBindings)

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

let lts
      ?(weak_labels : M.Label.Set.t = M.Label.Set.empty)
      (init : int)
      (ts : M.Transition.t list)
  : M.LTS.t
  =
  let add_t acc t = M.Transition.Set.add t acc in
  let transitions = List.fold_left add_t M.Transition.Set.empty ts in
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

let tau : M.Label.t = label ~silent:true 0
let a : M.Label.t = label 1

(** C(2k,k) — the number of simple (0,0)->(k,k) paths in a k x k grid. *)
let central_binomial (k : int) : float =
  let r = ref 1.0 in
  for i = 1 to k do
    r := !r *. float_of_int (k + i) /. float_of_int i
  done;
  !r
;;

(** A k x k grid of silent steps, plus one visible action off the far corner. *)
let grid (k : int) : M.FSM.t =
  let id (i : int) (j : int) : int = (i * (k + 1)) + j in
  let ts = ref [] in
  for i = 0 to k do
    for j = 0 to k do
      if j < k then ts := transition (id i j) tau (id i (j + 1)) :: !ts;
      if i < k then ts := transition (id i j) tau (id (i + 1) j) :: !ts
    done
  done;
  (* the visible action that saturation must find a weak path to *)
  ts := transition (id k k) a (((k + 1) * (k + 1)) + 1) :: !ts;
  fsm ~weak_labels:(M.Label.Set.singleton tau) 0 !ts
;;

(** A plain silent chain of the same STATE count as [grid k], with no
    branching at all — the control. Path count here is 1. *)
let chain (k : int) : M.FSM.t =
  let n = (k + 1) * (k + 1) in
  let ts = ref [ transition (n - 1) a n ] in
  for i = 0 to n - 2 do
    ts := transition i tau (i + 1) :: !ts
  done;
  fsm ~weak_labels:(M.Label.Set.singleton tau) 0 !ts
;;

let time (f : unit -> 'a) : float * 'a =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  Unix.gettimeofday () -. t0, r
;;

let () =
  Printf.printf
    "%-6s %-8s %-8s %-14s %-12s %-12s\n"
    "k"
    "states"
    "trans"
    "simple paths"
    "grid (s)"
    "chain (s)";
  print_endline (String.make 68 '-');
  let kmax = try int_of_string Sys.argv.(1) with _ -> 6 in
  for k = 1 to kmax do
    let g = grid k in
    let c = chain k in
    let tg, _ = time (fun () -> M.FSM.saturate g) in
    let tc, _ = time (fun () -> M.FSM.saturate c) in
    Printf.printf
      "%-6i %-8i %-8i %-14.0f %-12.4f %-12.4f\n%!"
      k
      (M.State.Set.cardinal g.states)
      ((2 * k * (k + 1)) + 1)
      (central_binomial k)
      tg
      tc
  done
;;
