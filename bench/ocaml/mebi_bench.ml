(** Pure-OCaml benchmarks of the model's algorithms: saturation,
    minimization and bisimilarity, timed with the [benchmark] library on
    fixtures extracted from the Rocq examples and on generated families.
    Tooling, not plugin capability: it links [rocq-mebi.model] only.

    Run with [dune exec bench/ocaml/mebi_bench.exe -- --help]; see
    [bench/ocaml/README.md]. Prints one tab-separated row per input and
    algorithm on stdout. *)

module M = Input.M

let fixtures = ref "bench/ocaml/fixtures"
let widths = ref [ 0; 1; 2; 3 ]
let depths = ref [ 1; 2; 4; 8; 16 ]
let algos = ref [ "saturate"; "minimize"; "bisim" ]
let only = ref ""
let seconds = ref 1
let repeat = ref 3
let check_only = ref false

let int_list s =
  if s = "" then [] else List.map int_of_string (String.split_on_char ',' s)
;;

let spec =
  [ ( "--fixtures"
    , Arg.Set_string fixtures
    , "DIR fixtures to load (\"\" for none)" )
  ; ( "--width"
    , Arg.String (fun s -> widths := int_list s)
    , "N,... width family sizes (\"\" for none; default 0,1,2,3)" )
  ; ( "--depth"
    , Arg.String (fun s -> depths := int_list s)
    , "K,... depth family sizes (\"\" for none; default 1,2,4,8,16)" )
  ; ( "--algos"
    , Arg.String (fun s -> algos := String.split_on_char ',' s)
    , "A,... of saturate, minimize, bisim (default all)" )
  ; "--only", Arg.Set_string only, "S only inputs whose name contains S"
  ; ( "--seconds"
    , Arg.Set_int seconds
    , "T run each timing for T seconds (default 1)" )
  ; "--repeat", Arg.Set_int repeat, "R repeat each timing R times (default 3)"
  ; ( "--check"
    , Arg.Set check_only
    , " check each input's verdict once and print its size; time nothing" )
  ]
;;

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  n = 0 || go 0
;;

(** [verdict i] is whether [i]'s initial states are bisimilar, by
    [Bisimilarity.fsm] as the plugin calls it. *)
let verdict (a : M.FSM.t) (b : M.FSM.t) : bool =
  M.Bisimilarity.Result.are_bisimilar (M.Bisimilarity.fsm a b).result
;;

(** [time f] is [f]'s CPU time per run in milliseconds: the mean and the
    least over [!repeat] timings of [!seconds] each. *)
let time (f : unit -> 'a) : float * float * int64 =
  let samples =
    Benchmark.throughput1
      ~style:Benchmark.Nil
      ~repeat:!repeat
      !seconds
      (fun () -> ignore (f ()))
      ()
  in
  let per (t : Benchmark.t) =
    (t.utime +. t.stime) /. Int64.to_float t.iters *. 1000.
  in
  let ts = List.concat_map snd samples in
  let ms = List.map per ts in
  ( List.fold_left ( +. ) 0. ms /. float (List.length ms)
  , List.fold_left min infinity ms
  , List.fold_left (fun acc (t : Benchmark.t) -> Int64.add acc t.iters) 0L ts )
;;

let row name side (s : Input.side) algo (mean, least, runs) =
  Printf.printf
    "%s\t%s\t%i\t%i\t%s\t%Ld\t%.3f\t%.3f\n%!"
    name
    side
    (Input.num_states s)
    (Input.num_edges s)
    algo
    runs
    mean
    least
;;

let run (i : Input.t) : bool =
  let a = Input.fsm ~silent:i.silent i.a in
  let b = Option.map (Input.fsm ~silent:i.silent) i.b in
  let ok =
    match b, i.bisimilar with
    | Some b, Some expected ->
      let got = verdict a b in
      if got <> expected
      then
        Printf.eprintf
          "%s: bisimilar is %b, expected %b\n%!"
          i.name
          got
          expected;
      got = expected
    | _ -> true
  in
  if !check_only
  then (
    let size (s : Input.side) =
      Printf.sprintf
        "%i states, %i edges"
        (Input.num_states s)
        (Input.num_edges s)
    in
    Printf.printf
      "%s\t%s\t%s\t%s\n%!"
      (if ok then "ok" else "WRONG")
      i.name
      (size i.a)
      (Option.fold ~none:"" ~some:size i.b))
  else (
    let sides =
      match i.b, b with
      | Some s, Some f -> [ "a", i.a, a; "b", s, f ]
      | _ -> [ "a", i.a, a ]
    in
    List.iter
      (fun algo ->
        match algo, b with
        | "saturate", _ ->
          List.iter
            (fun (n, s, f) ->
              row i.name n s algo (time (fun () -> M.FSM.saturate f)))
            sides
        | "minimize", _ ->
          List.iter
            (fun (n, s, f) ->
              row i.name n s algo (time (fun () -> M.Minimization.fsm f)))
            sides
        | "bisim", Some b ->
          row i.name "a+b" i.a algo (time (fun () -> M.Bisimilarity.fsm a b))
        | "bisim", None -> ()
        | _ -> failwith ("unknown algorithm " ^ algo))
      !algos);
  ok
;;

let () =
  Arg.parse
    (Arg.align spec)
    (fun x -> raise (Arg.Bad ("unexpected argument " ^ x)))
    "mebi_bench [options]: time the model's algorithms";
  let inputs =
    (if !fixtures = "" then [] else Fixture.load_dir !fixtures)
    @ List.map Families.Width.make !widths
    @ List.map Families.Depth.make !depths
    |> List.filter (fun (i : Input.t) -> contains i.name !only)
  in
  if not !check_only
  then
    print_endline "input\tside\tstates\tedges\talgorithm\truns\tms_mean\tms_min";
  let failed = List.filter (fun i -> not (run i)) inputs in
  if !check_only
  then
    Printf.printf
      "%i inputs, %i wrong verdicts\n"
      (List.length inputs)
      (List.length failed);
  if failed <> [] then exit 1
;;
