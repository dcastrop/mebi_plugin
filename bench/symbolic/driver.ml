(** The command line, over one BDD backend. *)

let usage =
  "symbolic [--family proc|proc-bad|buffers|buffers-bad|abp] [--n 4,8,16] \
   [--order zip|blocks] [--oracle N]\n\
   Prints one tab-separated row per (system, order). --oracle N also runs the \
   explicit check (the plugin's model) when both sides have at most N states, \
   and fails on a disagreement."
;;

module Run (B : Bdd.S) = struct
  module S = Symbolic.Make (B)

  let main () =
    let families =
      ref [ "proc"; "proc-bad"; "buffers"; "buffers-bad"; "abp" ]
    in
    let ns = ref [ 4; 8 ] in
    let orders = ref [ Symbolic.Zip; Symbolic.Blocks ] in
    let oracle = ref 20_000 in
    let split s = String.split_on_char ',' s in
    Arg.parse
      [ ( "--family"
        , Arg.String (fun s -> families := split s)
        , " families, comma-separated" )
      ; ( "--n"
        , Arg.String (fun s -> ns := List.map int_of_string (split s))
        , " sizes" )
      ; ( "--order"
        , Arg.String
            (fun s ->
              orders
              := List.map
                   (function
                     | "zip" -> Symbolic.Zip
                     | "blocks" -> Symbolic.Blocks
                     | o -> raise (Arg.Bad o))
                   (split s))
        , " variable orders" )
      ; ( "--oracle"
        , Arg.Set_int oracle
        , " explicit check up to this many states a side" )
      ]
      (fun _ -> ())
      usage;
    let pairs =
      List.concat_map
        (function
          | "proc" -> List.map (fun n -> Families.proc n) !ns
          | "proc-bad" -> List.map (fun n -> Families.proc ~bad:true n) !ns
          | "buffers" -> List.map (fun n -> Families.buffers n) !ns
          | "buffers-bad" ->
            List.map (fun n -> Families.buffers ~m:(n - 1) n) !ns
          | "abp" -> [ Families.abp () ]
          | f -> failwith ("unknown family " ^ f))
        !families
    in
    print_endline
      "backend\tsystem\torder\tverdict\texpected\tstates_a\tstates_b\tpairs_in_R\tR_nodes\tpeak_nodes\titerations\tseconds\texplicit";
    let failed = ref false in
    List.iter
      (fun (p : Families.pair) ->
        List.iter
          (fun order ->
            let r = S.check ~order p.a p.b in
            let explicit =
              if
                r.states1 <= float_of_int !oracle
                && r.states2 <= float_of_int !oracle
              then (
                (* the model's memory guard (Bounds Memory) can stop the
                   explicit check: report it in the row, and go on *)
                match Explicit.check p.a p.b with
                | exception Memory_guard.Exceeded _ -> "out-of-memory-guard"
                | x ->
                  if
                    x.bisimilar <> r.bisimilar
                    || float_of_int x.states1 <> r.states1
                    || float_of_int x.states2 <> r.states2
                  then failed := true;
                  Printf.sprintf
                    "%s %d/%d %.2fs"
                    (if x.bisimilar then "bisim" else "not")
                    x.states1
                    x.states2
                    x.seconds)
              else "-"
            in
            if r.bisimilar <> p.expect then failed := true;
            Printf.printf
              "%s\t%s\t%s\t%s\t%s\t%.0f\t%.0f\t%.0f\t%d\t%d\t%d\t%.2f\t%s\n%!"
              B.name
              p.name
              (Symbolic.order_to_string order)
              (if r.bisimilar then "bisim" else "not")
              (if p.expect then "bisim" else "not")
              r.states1
              r.states2
              r.pairs
              r.rel_nodes
              r.peak_nodes
              r.iterations
              r.seconds
              explicit)
          !orders)
      pairs;
    if !failed
    then (
      prerr_endline "symbolic: a verdict or state count disagreed";
      exit 1)
  ;;
end
