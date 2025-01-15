open Fsm
open Bisimilarity
open Pp

(** [ks90_exas] ... *)
let rec ks90_exas ?(pp : unit option) (exas : RCP.Examples.example list) : unit =
  match exas with
  | [] -> ()
  | exa :: exas' ->
    (match exa with
     | { name; s; t; _ } ->
       (* safely print depending on if coq or not *)
       let print_str =
         Printf.sprintf
           "%s\n%s\n%s"
           (Printf.sprintf "\n= = = = = = = = = =\nRCP.KS90 (%s)\n" name)
           (Printf.sprintf "%s.s: %s" name (pstr ~options:(Debug ()) (Fsm s)))
           (Printf.sprintf "%s.t: %s" name (pstr ~options:(Debug ()) (Fsm t)))
       in
       (match pp with
        | None -> Printf.printf "%s" print_str
        | Some () -> Feedback.msg_info (str print_str));
       (* run algorithm *)
       let are_bisimilar, pi = RCP.KS90.run s t in
       (* print out results *)
       let print_str' =
         Printf.sprintf
           "%s\n%s\n%s\n%s"
           (Printf.sprintf
              "[KS90] (%s) Results: (s ~ t) = %b.%s"
              name
              are_bisimilar
              (if Bool.not are_bisimilar
               then
                 Printf.sprintf
                   "\nnon-bisimilar partition: %s"
                   (pstr (pp_wrap_as_supported (Partition pi)))
               else ""))
           (Printf.sprintf
              "where s = %s\nand t = %s."
              (pstr (pp_wrap_as_supported (Fsm s)))
              (pstr (pp_wrap_as_supported (Fsm t))))
           (Printf.sprintf
              "\n--------\npi: %s"
              (pstr (pp_wrap_as_supported (Partition pi))))
           (Printf.sprintf "\n= = = = = = = = = =\n")
       in
       (match pp with
        | None -> Printf.printf "%s" print_str'
        | Some () -> Feedback.msg_info (str print_str'));
       (* continue *)
       (match pp with
        | None -> ks90_exas exas'
        | Some () -> ks90_exas ~pp:() exas'))
;;

let run_all_ks90 ?(pp : unit option) () : unit =
  match pp with
  | None -> ks90_exas [ RCP.Examples.exa_1; RCP.Examples.exa_2 ]
  | Some () -> ks90_exas ~pp:() [ RCP.Examples.exa_1; RCP.Examples.exa_2 ]
;;

let run_all ?(pp : unit option) () : unit =
  match pp with
  | None ->
    Printf.printf "\nRunning Tests.ml:\n\n";
    run_all_ks90 ();
    Printf.printf "\n\nEnd of Tests.ml.\n"
  | Some () ->
    Feedback.msg_info (str (Printf.sprintf "\nRunning Tests.ml:\n\n"));
    run_all_ks90 ~pp:() ();
    Feedback.msg_info (str (Printf.sprintf "\n\nEnd of Tests.ml.\n"))
;;

(* ! vscoq language server breaks if this is uncommented. *)
(* let () = run_all () *)
