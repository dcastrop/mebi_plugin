(* test/cert_tool.exe --log LOG [OUT]: the proof log's translation alone
   (lib/certify/dd_cert.ml's [write_log]), with the facts per lemma; OUT
   by default a file next to LOG, removed after. Tooling, for measuring the
   translation without Rocq. *)

(* a temporary file next to [path] (never in /tmp: a tmpfs here) *)
let beside path ext =
  Filename.temp_file
    ~temp_dir:(Filename.dirname path)
    (Filename.basename path ^ ".")
    ext
;;

(* [cert_tool --log LOG [OUT]]: the proof log's translation
   ({!Dd_cert.write_log}) alone, with the number of facts per lemma *)
let log_mode () =
  let log = Sys.argv.(2) in
  let out =
    if Array.length Sys.argv > 3 then Sys.argv.(3) else beside log ".facts"
  in
  let st =
    try Dd_cert.write_log ~log:(Proof_log.File log) (Dd_cert.To_file out) with
    | Dd_cert.Fails m ->
      Printf.printf "fails: %s\n" m;
      exit 1
  in
  Printf.printf
    "%d chunks, %d facts, %d chunk nodes, %.1fs, heap at most %dMB\n"
    st.Dd_cert.chunks
    st.Dd_cert.facts
    st.Dd_cert.locals
    st.Dd_cert.seconds
    ((Gc.quick_stat ()).Gc.top_heap_words * 8 / 1048576);
  let by : (string, int ref) Hashtbl.t = Hashtbl.create 64 in
  let ic = open_in_bin out in
  (try
     while true do
       let l = input_line ic in
       if String.length l > 2 && l.[0] = 'f'
       then (
         match String.split_on_char ' ' l with
         | _ :: _ :: lemma :: _ ->
           (match Hashtbl.find_opt by lemma with
            | Some c -> incr c
            | None -> Hashtbl.add by lemma (ref 1))
         | _ -> ())
     done
   with
   | End_of_file -> ());
  close_in ic;
  Hashtbl.fold (fun k c acc -> (!c, k) :: acc) by []
  |> List.sort compare
  |> List.rev
  |> List.iter (fun (c, k) -> Printf.printf "  %-24s %9d\n" k c);
  if Array.length Sys.argv <= 3 then Sys.remove out
;;

let () =
  if Array.length Sys.argv > 2 && Sys.argv.(1) = "--log"
  then log_mode ()
  else (
    prerr_endline "usage: cert_tool --log LOG [OUT]";
    exit 2)
;;
