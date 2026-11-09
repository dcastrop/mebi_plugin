(** Tests of [lib/ldd], the binding to Sylvan's LDDs. No Rocq.
    Run with: [dune exec test/ldd_tests.exe] *)

let total = ref 0
let failures = ref 0

let check name expected actual =
  incr total;
  if expected = actual
  then Printf.printf "  ok    %s\n" name
  else (
    incr failures;
    Printf.printf "  FAIL  %s\n" name)
;;

let () =
  Ldd.init ~memory_mb:256 ();
  print_endline "\n=== ldd tests (Sylvan, no Rocq) ===\n";
  (* sets *)
  let a = Ldd.of_list [ [| 1; 2 |]; [| 3; 4 |] ] in
  let b = Ldd.of_list [ [| 3; 4 |]; [| 5; 6 |] ] in
  check "count" 2. (Ldd.count a);
  check "union" 3. (Ldd.count (Ldd.union a b));
  check "minus" [ [| 1; 2 |] ] (Ldd.elements (Ldd.minus a b));
  check "inter" [ [| 3; 4 |] ] (Ldd.elements (Ldd.inter a b));
  check "mem" (true, false) (Ldd.mem a [| 3; 4 |], Ldd.mem a [| 3; 5 |]);
  check "canonical" true (Ldd.equal (Ldd.union a b) (Ldd.union b a));
  check "empty" true (Ldd.is_empty (Ldd.minus a a));
  check
    "export"
    ([| 4; 1; 0; 3; 2; 0; 2; 1; 0; 1; 4; 3 |], [| 5; 0 |])
    (Ldd.export [| Ldd.of_list [ [| 1; 2 |]; [| 3; 4 |] ]; Ldd.empty () |]);
  check
    "elements_flat"
    (2, [ 1; 2; 3; 4; 5; 6 ])
    (let w, d = Ldd.elements_flat (Ldd.union a b) in
     w, List.init (Bigarray.Array1.dim d) (fun i -> Int32.to_int d.{i}));
  check
    "large values"
    true
    (Ldd.mem (Ldd.cube [| 0; 0xFFFFFFFE |]) [| 0; 0xFFFFFFFE |]);
  (* relations: position 1 read and written, position 0 copied *)
  let rel = Ldd.of_list [ [| 2; 3 |]; [| 4; 5 |] ] in
  let meta = Ldd.rel_meta [ Ldd.Copy; Ldd.Read; Ldd.Write ] in
  check
    "image"
    [ [| 1; 3 |]; [| 3; 5 |] ]
    (Ldd.elements (Ldd.relprod a ~rel ~meta));
  check
    "preimage"
    [ [| 1; 2 |] ]
    (Ldd.elements
       (Ldd.relprev (Ldd.cube [| 1; 3 |]) ~rel ~meta ~within:(Ldd.union a b)));
  (* a label in the relation: (old, new, label) *)
  let rell = Ldd.of_list [ [| 2; 3; 7 |]; [| 4; 5; 8 |] ] in
  let metal = Ldd.rel_meta [ Ldd.Copy; Ldd.Read; Ldd.Write; Ldd.Label ] in
  check "labelled image" 2. (Ldd.count (Ldd.relprod a ~rel:rell ~meta:metal));
  (* projection *)
  check
    "project"
    [ [| 1 |]; [| 3 |] ]
    (Ldd.elements (Ldd.project a [ true; false ]));
  (* product *)
  let p = Ldd.product a ~width:2 (Ldd.of_list [ [| 9 |]; [| 8 |] ]) in
  check
    "product"
    [ [| 1; 2; 8 |]; [| 1; 2; 9 |]; [| 3; 4; 8 |]; [| 3; 4; 9 |] ]
    (Ldd.elements p);
  (* release *)
  let c = Ldd.union a b in
  Ldd.release c;
  check "release" true (Ldd.is_empty c);
  (* locality: two independent counters mod 100, one group each *)
  let n = 100 in
  let step = Ldd.of_list (List.init n (fun i -> [| i; (i + 1) mod n |])) in
  let g1 = Ldd.rel_meta [ Ldd.Read; Ldd.Write; Ldd.Copy ] in
  let g2 = Ldd.rel_meta [ Ldd.Copy; Ldd.Read; Ldd.Write ] in
  let visited = ref (Ldd.cube [| 0; 0 |])
  and frontier = ref (Ldd.cube [| 0; 0 |]) in
  while not (Ldd.is_empty !frontier) do
    let n1 = Ldd.relprod !frontier ~rel:step ~meta:g1 in
    let n2 = Ldd.relprod !frontier ~rel:step ~meta:g2 in
    let next = Ldd.union n1 n2 in
    Ldd.release n1;
    Ldd.release n2;
    let f = Ldd.minus next !visited in
    Ldd.release next;
    let v = Ldd.union !visited f in
    Ldd.release !visited;
    Ldd.release !frontier;
    visited := v;
    frontier := f
  done;
  check "two counters: states" (float_of_int (n * n)) (Ldd.count !visited);
  check
    "two counters: nodes stay linear"
    true
    (Ldd.nodes !visited <= (2 * n) + 2);
  Printf.printf
    "  (two counters: %.0f states in %d nodes)\n"
    (Ldd.count !visited)
    (Ldd.nodes !visited);
  (* the proof log (doc/certificate.md, A): operations recorded while
     it is open, read back; each union, minus and intersection checked
     against the sets of the nodes the log dumped *)
  (* next to the test's binary (never in /tmp: a tmpfs here) *)
  let prefix =
    Filename.temp_file
      ~temp_dir:(Filename.dirname Sys.executable_name)
      ".ldd_tests."
      ".log"
  in
  let x = Ldd.of_list [ [| 1; 2 |]; [| 3; 4 |]; [| 5; 6 |] ] in
  let y = Ldd.of_list [ [| 3; 4 |]; [| 5; 7 |]; [| 8; 9 |] ] in
  check "log: opens" true (Ldd.log_open prefix);
  let u = Ldd.union x y in
  let m = Ldd.minus x y in
  let i = Ldd.inter x y in
  let im = Ldd.relprod x ~rel ~meta in
  (* an image backwards (the relation's tuples swapped), recorded as a
     top-level call whose result is used sound *)
  let inv = Ldd.of_list [ [| 3; 2 |]; [| 5; 4 |] ] in
  let back = Ldd.relprod im ~rel:inv ~meta in
  Ldd.log_top_relprod ~role:1 im ~rel:inv ~meta back;
  let n = Ldd.log_close () in
  let recs, nodes = Proof_log.read prefix in
  check "log: a record per result" n (List.length recs);
  let rec elems k =
    if k = 0
    then []
    else if k = 1
    then [ [] ]
    else (
      let nd = Hashtbl.find nodes k in
      List.map (fun v -> nd.Proof_log.value :: v) (elems nd.Proof_log.down)
      @ elems nd.Proof_log.right)
  in
  let set k = List.sort_uniq compare (elems k) in
  check
    "log: every recorded node dumped"
    true
    (List.for_all
       (fun (r : Proof_log.record) ->
         List.for_all
           (fun k -> k <= 1 || Hashtbl.mem nodes k)
           [ r.a; r.b; r.c; r.d; r.r ])
       recs);
  check
    "log: unions, minuses, intersections are what they say"
    true
    (List.for_all
       (fun (r : Proof_log.record) ->
         let a = set r.a
         and b = set r.b
         and res = set r.r in
         match r.op with
         | Proof_log.Union -> res = List.sort_uniq compare (a @ b)
         | Proof_log.Minus ->
           res = List.filter (fun v -> Bool.not (List.mem v b)) a
         | Proof_log.Intersect -> res = List.filter (fun v -> List.mem v b) a
         | Proof_log.Relprod | Proof_log.Relprev | Proof_log.Top -> true)
       recs);
  (* the top-level image: its role, operation, arguments and result *)
  check
    "log: the top-level call recorded as made"
    true
    (match
       List.filter (fun (r : Proof_log.record) -> r.op = Proof_log.Top) recs
     with
     | [ t ] ->
       Proof_log.top_role t = 1
       && Proof_log.top_op t = Proof_log.Relprod
       && set t.a
          = List.sort_uniq compare (List.map Array.to_list (Ldd.elements im))
       && set t.r
          = List.sort_uniq compare (List.map Array.to_list (Ldd.elements back))
       && set t.r = [ [ 1; 2 ]; [ 3; 4 ] ]
     | _ -> false);
  (* each top-level result: some record of its operation has its set *)
  let as_set t =
    List.sort_uniq compare (List.map Array.to_list (Ldd.elements t))
  in
  check
    "log: the top results are recorded"
    true
    (List.for_all
       (fun (op, t) ->
         List.exists
           (fun (r : Proof_log.record) -> r.op = op && set r.r = as_set t)
           recs)
       [ Proof_log.Union, u
       ; Proof_log.Minus, m
       ; Proof_log.Intersect, i
       ; Proof_log.Relprod, im
       ]);
  check "log: the image's elements" [ [| 1; 3 |]; [| 3; 5 |] ] (Ldd.elements im);
  (try Sys.remove prefix with Sys_error _ -> ());
  (* the same operations, the log in memory: as many records, each union,
     minus and intersection what it says, by the nodes of Sylvan's table *)
  check "log in memory: opens" true (Ldd.log_open_mem ());
  let u' = Ldd.union x y in
  let m' = Ldd.minus x y in
  let i' = Ldd.inter x y in
  let im' = Ldd.relprod x ~rel ~meta in
  let back' = Ldd.relprod im' ~rel:inv ~meta in
  Ldd.log_top_relprod ~role:1 im' ~rel:inv ~meta back';
  let n' = Ldd.log_close_mem () in
  let recs' = ref [] in
  let node_of =
    Proof_log.iter Proof_log.Memory (fun r -> recs' := r :: !recs')
  in
  let rec elems' k =
    if k = 0
    then []
    else if k = 1
    then [ [] ]
    else (
      let nd = node_of k in
      List.map (fun v -> nd.Proof_log.value :: v) (elems' nd.Proof_log.down)
      @ elems' nd.Proof_log.right)
  in
  let set' k = List.sort_uniq compare (elems' k) in
  check "log in memory: as many records as the file" n n';
  check "log in memory: a record per result" n' (List.length !recs');
  check
    "log in memory: unions, minuses, intersections are what they say"
    true
    (List.for_all
       (fun (r : Proof_log.record) ->
         let a = set' r.a
         and b = set' r.b
         and res = set' r.r in
         match r.op with
         | Proof_log.Union -> res = List.sort_uniq compare (a @ b)
         | Proof_log.Minus ->
           res = List.filter (fun v -> Bool.not (List.mem v b)) a
         | Proof_log.Intersect -> res = List.filter (fun v -> List.mem v b) a
         | Proof_log.Relprod | Proof_log.Relprev | Proof_log.Top -> true)
       !recs');
  check
    "log in memory: the results' sets"
    true
    (List.for_all
       (fun t ->
         List.exists (fun (r : Proof_log.record) -> set' r.r = as_set t) !recs')
       [ u'; m'; i'; im'; back' ]);
  Ldd.log_free ();
  Printf.printf "\n%i/%i passed\n" (!total - !failures) !total;
  if !failures > 0 then exit 1
;;
