open Fsm
open Bisimilarity

(** [ks90_exas] ... *)
let rec ks90_exas (exas : RCP.Examples.example list) : unit =
  match exas with
  | [] -> ()
  | exa :: exas' ->
    (match exa with
     | { name; s; t; _ } ->
       Printf.printf "\n= = = = = = = = = =\nRCP.KS90 (%s)\n\n" name;
       Printf.printf "%s.s: %s.\n\n" name (pstr (Fsm s));
       Printf.printf "%s.t: %s.\n\n" name (pstr (Fsm t));
       (* Printf.printf "%s.s: %s.\n" name (pstr ~options:(Debug ()) (Fsm s));
          Printf.printf "%s.t: %s.\n" name (pstr ~options:(Debug ()) (Fsm t)); *)
       (* run algorithm *)
       let are_bisimilar, pi = RCP.KS90.run s t in
       (* print out results *)
       Printf.printf
         "[KS90] (%s) Results: (s ~ t) = %b.%s\n"
         name
         are_bisimilar
         (match are_bisimilar with
          | true ->
            Printf.sprintf
              "\nbisimilar states: %s."
              (pstr (pp_wrap_as_supported (Partition pi)))
          | _ ->
            Printf.sprintf
              "\nnon-bisimilar partition: %s."
              (pstr (pp_wrap_as_supported (Partition pi))));
       (* Printf.printf
          "where s = %s\nand t = %s.\n"
          (pstr (pp_wrap_as_supported (Fsm s)))
          (pstr (pp_wrap_as_supported (Fsm t))); *)
       (* Printf.printf
          "\n--------\npi: %s.\n"
          (pstr (pp_wrap_as_supported (Partition pi))); *)
       Printf.printf "\n= = = = = = = = = =\n";
       ();
       (* continue *)
       ks90_exas exas')
;;

let run_all_ks90 : unit = ks90_exas [ RCP.Examples.exa_1; RCP.Examples.exa_2 ]

let run_all : unit =
  Printf.printf "\nRunning Tests.ml:\n\n";
  run_all_ks90;
  Printf.printf "\n\nEnd of Tests.ml.\n"
;;

let () = run_all
