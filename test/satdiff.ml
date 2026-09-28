(** Differential harness for the saturation rewrite.

    Infrastructure, not plugin capability — a pure-OCaml binary linking
    [rocq-mebi.model] only, same constraint as [tests.ml] and [satscale.ml].

    The 18-count proof baseline in [CLAUDE.md] is a coarse gate for a change to
    [Saturation]: it says the proofs still close in the same number of steps,
    not that the saturated FSM holds the same weak transitions. A rewrite could
    change which annotations survive and still pass it — a realistic failure
    mode, since [ActionPair.try_update] merges on *exactly equal* destination
    sets, so a different emission order can change the survivor.

    So: generate deterministic pseudo-random LTSs, saturate each, and print a
    canonical rendering. Capture the output before a change and diff it after.
    Determinism is the whole point, hence the explicit sorting below —
    [EdgeMap] is a [Hashtbl] and its iteration order is not a contract.

    Workflow:

    {[
      # before changing Saturation -- already committed as the golden file
      dune exec test/satdiff.exe -- 200 > test/satdiff.expected 2>/dev/null

      # after changing it
      dune exec test/satdiff.exe -- 200 2>/dev/null | diff test/satdiff.expected -
    ]}

    An empty diff means the rewrite preserves every weak transition, every
    destination set and every surviving annotation on 200 generated LTSs. A
    non-empty diff is not automatically a regression -- but it must be
    explained, not waved through, because [try_update] keeping
    [Annotation.shorter] means a changed emission order can silently change
    which annotation survives.

    Run with: [dune exec test/satdiff.exe] (optionally [-- <seed-count>]) *)

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

(* ------------------------------------------------------------------ *)
(* Generation.                                                          *)

(** A pseudo-random LTS. Kept small deliberately: the differential check is
    only meaningful while the OLD, path-enumerating implementation can still
    finish, and [satscale.ml] shows that stops being true around 100 states on
    a branching shape. Silent labels are what make saturation do any work, so
    roughly half the alphabet is silent. *)
let gen (rng : Random.State.t) (n_states : int) (n_labels : int)
  : M.Transition.t list * M.Label.Set.t
  =
  let silent_of (i : int) : bool = i mod 2 = 0 in
  let weak_labels =
    List.init n_labels (fun i -> i)
    |> List.filter silent_of
    |> List.fold_left
         (fun acc i -> M.Label.Set.add (label ~silent:true i) acc)
         M.Label.Set.empty
  in
  let ts = ref [] in
  (* Every state gets 1-2 outgoing transitions. This is deliberately tiny:
     the differential check only means anything while the OLD,
     path-enumerating implementation can still finish, and it cannot -- a
     first attempt at 3-8 states with out-degree up to 3 did not clear even
     one seed in ten minutes. That is the very blow-up being fixed. *)
  for from = 0 to n_states - 1 do
    let out = 1 + Random.State.int rng 2 in
    for _ = 1 to out do
      let l = Random.State.int rng n_labels in
      let goto = Random.State.int rng n_states in
      ts := transition from (label ~silent:(silent_of l) l) goto :: !ts
    done
  done;
  !ts, weak_labels
;;

(* ------------------------------------------------------------------ *)
(* Canonical rendering.                                                 *)

let render_label (l : M.Label.t) : string =
  Printf.sprintf
    "%i%s"
    l.base
    (match l.is_silent with Some true -> "(t)" | _ -> "")
;;

let render_annotation (a : M.Annotation.t option) : string =
  match a with None -> "-" | Some a -> M.Annotation.to_string ~pretty:false a
;;

let render (edges : M.EdgeMap.t') : string =
  let buf = Buffer.create 4096 in
  let rows =
    M.EdgeMap.fold
      (fun (from : M.State.t) (actions : M.Action.Map.t') acc ->
        let entries =
          M.Action.Map.to_seq actions
          |> List.of_seq
          |> List.map (fun ((act, dests) : M.Action.t * M.State.Set.t) ->
            let ds =
              M.State.Set.elements dests
              |> List.map (fun (s : M.State.t) -> s.base)
              |> List.sort compare
              |> List.map string_of_int
              |> String.concat ","
            in
            Printf.sprintf
              "    %s ann=%s -> {%s}"
              (render_label act.label)
              (render_annotation act.annotation)
              ds)
          |> List.sort compare
        in
        (from.base, entries) :: acc)
      edges
      []
    |> List.sort compare
  in
  List.iter
    (fun ((from, entries) : int * string list) ->
      Buffer.add_string buf (Printf.sprintf "  from %i\n" from);
      List.iter (fun e -> Buffer.add_string buf (e ^ "\n")) entries)
    rows;
  Buffer.contents buf
;;

(* ------------------------------------------------------------------ *)

let () =
  let seeds = try int_of_string Sys.argv.(1) with _ -> 40 in
  print_endline "=== saturation differential dump ===";
  Printf.printf "seeds=%i\n%!" seeds;
  for seed = 1 to seeds do
    let rng = Random.State.make [| seed |] in
    let n_states = 3 + Random.State.int rng 3 in
    let n_labels = 2 + Random.State.int rng 2 in
    let ts, weak_labels = gen rng n_states n_labels in
    let f = M.FSM.of_lts (lts ~weak_labels 0 ts) in
    let t0 = Sys.time () in
    let s = M.FSM.saturate f in
    let dt = Sys.time () -. t0 in
    (* Timing goes to stderr on purpose: stdout is the artifact being
       diffed across implementations, so nothing that varies run to run may
       appear in it. *)
    Printf.eprintf "seed %i: %.3fs\n%!" seed dt;
    Printf.printf
      "\n--- seed %i (states=%i labels=%i trans=%i) ---\n%!"
      seed
      n_states
      n_labels
      (List.length ts);
    print_string (render s.edges);
    flush stdout
  done
;;
