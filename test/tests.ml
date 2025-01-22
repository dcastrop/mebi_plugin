open Mebi_plugin.Fsm
open Mebi_plugin.Bisimilarity
open Mebi_plugin.Utils
open Mebi_plugin.Examples

(**  *)
let pstr_results
      ?(show : bool = false)
      ?(details : bool = true)
      ?(debug : bool = false)
      (results : (string * (string * bool * bool) list) list)
  : string
  =
  Printf.sprintf
    "\n\n\
     = = = Test.ml Results = = =\n\n\
     %s\n\n\
     = = = (end of Test.ml Results) = = =\n\n"
    (List.fold_left
       (fun (acc : string)
         ((suite_name, suite_results) : string * (string * bool * bool) list) ->
          Printf.sprintf
            "%s(=>) %s: [%s].\n\n"
            acc
            suite_name
            (List.fold_left
               (fun (acc' : string)
                 ((name, expected_result, actual_result) : string * bool * bool) ->
                  Printf.sprintf
                    "%s  %s  | %s  | %s\n"
                    acc'
                    (if expected_result then "true " else "false")
                    (if actual_result then "true " else "false")
                    name)
               "\n  EXPECT | ACTUAL | EXAMPLE\n  ---------------------------\n"
               suite_results))
       "\n"
       results)
;;

(** [ks90_exas] ... *)
let rec ks90_exas
          ?(show : bool = false)
          ?(details : bool = true)
          ?(debug : bool = false)
          (exas : example list)
  : (string * bool * bool) list
  =
  match exas with
  | [] -> []
  | exa :: exas' ->
    (match exa with
     | { name; s; t; are_bisimilar; _ } ->
       let _are_bisimilar = are_bisimilar in
       (* safely print depending on if coq or not *)
       print
         ~show
         (Printf.sprintf
            "\n= = = = = = = = = =\nRCP.KS90 (%s)\n\n%s.s: %s.\n\n%s.t: %s.\n\n"
            name
            name
            (pstr ~options:(pstr_options details) (Fsm s))
            name
            (pstr ~options:(pstr_options details) (Fsm t)));
       (* run algorithm *)
       let result = RCP.KS90.run ~show ~details ~debug s t in
       (match result with
        | { are_bisimilar; bisimilar_states; non_bisimilar_states; _ } ->
          (* print out results *)
          print
            ~show
            (Printf.sprintf
               "[KS90] (%s) Results: (s ~ t) = %b.\n\n\
                Bisimilar states: %s.\n\n\
                Non-bisimilar states: %s.\n\n\
                = = = = = = = = = =\n\n"
               name
               are_bisimilar
               (pstr
                  ~options:(pstr_options details)
                  ~tabs:1
                  (pp_wrap_as_supported (Partition bisimilar_states)))
               (pstr
                  ~options:(pstr_options details)
                  ~tabs:1
                  (pp_wrap_as_supported (Partition non_bisimilar_states))));
          (* continue *)
          (name, _are_bisimilar, are_bisimilar) :: ks90_exas ~show ~debug exas'))
;;

let run_all_ks90
      ?(show : bool = false)
      ?(details : bool = true)
      ?(debug : bool = false)
      ()
  : (string * bool * bool) list
  =
  ks90_exas ~show ~details ~debug [ exa_1; exa_2 ]
;;

let run_all
      ?(show : bool = false)
      ?(details : bool = true)
      ?(debug : bool = false)
      ()
  : unit
  =
  print ~show "\nRunning Tests.ml:\n\n";
  let ks90_results : (string * bool * bool) list =
    run_all_ks90 ~show ~details ~debug ()
  in
  print ~show (pstr_results [ "KS90", ks90_results ]);
  print ~show "\n\nEnd of Tests.ml.\n"
;;

(** To run tests...

    - First build the project:

    make .merlin clean; dune build; coq_makefile -f _CoqProject -o CoqMakeFile; make -f CoqMakeFile

    - Next, run the tests:

    _build/default/test/tests.exe *)
let () = run_all ~show:true ~details:true ~debug:false ()
