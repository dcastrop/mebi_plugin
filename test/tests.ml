open Mebi_plugin.Fsm
open Mebi_plugin.Bisimilarity
open Mebi_plugin.Utils
open Mebi_plugin.Examples

(**  *)
let pstr_results
      ?(params : logging_params = default_logging_params ~mode:(OCaml ()) ())
      (results : (string * (string * bool * bool) list) list)
  : string
  =
  Printf.sprintf
    "\n\n\
     = = = Test.ml Results = = = = = = = =\n\n\
     %s\n\
     - - - - - - - - - - - - - - - - - - -\n\n\
     Passed all tests: %b.\n\n\
     = = = = = = = = = = = = = = = = = = =\n\n"
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
    (List.for_all
       (fun ((_suite_name, suite_results) :
              string * (string * bool * bool) list) ->
          List.for_all
            (fun ((_name, expected_result, actual_result) :
                   string * bool * bool) -> expected_result == actual_result)
            suite_results)
       results)
;;

(** [ks90_exas] ... *)
let rec ks90_exas
          ?(params : logging_params =
            default_logging_params ~mode:(OCaml ()) ())
          (exas : example list)
  : (string * bool * bool) list
  =
  params.kind <- Details ();
  match exas with
  | [] -> []
  | exa :: exas' ->
    (match exa with
     | { name; s; t; are_bisimilar; _ } ->
       let _are_bisimilar = are_bisimilar in
       (* safely print depending on if coq or not *)
       log
         ~params:(override params)
         (Printf.sprintf
            "\n\
             = = = = = = = = = = = = = = = = = = =\n\
             RCP.KS90 (%s)\n\n\
             %s.s: %s.\n\n\
             %s.t: %s.\n\n"
            name
            name
            (PStr.fsm ~params:(Logging params) s)
            name
            (PStr.fsm ~params:(Logging params) t));
       (* run algorithm *)
       let result = RCP.KS90.run ~params s t in
       (match result with
        | { are_bisimilar
          ; merged_fsm
          ; bisimilar_states
          ; non_bisimilar_states
          ; _
          } ->
          (* print out results *)
          log
            ~params:(override params)
            (Printf.sprintf
               "[KS90] (%s) Results: (s ~ t) = %b.\n\n\
                Bisimilar states: %s.\n\n\
                Non-bisimilar states: %s.\n\n\
                Using merged FSM: %s.\n\n\
                = = = = = = = = = = = = = = = = = = =\n\n"
               name
               are_bisimilar
               (PStr.partition ~params:(Logging params) bisimilar_states)
               (PStr.partition ~params:(Logging params) non_bisimilar_states)
               (PStr.fsm ~params:(Logging params) merged_fsm));
          (* continue *)
          (name, _are_bisimilar, are_bisimilar) :: ks90_exas ~params exas'))
;;

let run_all_ks90
      ?(params : logging_params = default_logging_params ~mode:(OCaml ()) ())
      ()
  : (string * bool * bool) list
  =
  ks90_exas
    ~params
    [ exa_1
    ; exa_2
    ; exa_self_rec_nondet
    ; exa_self_rec_nondet_inf
    ; exa_self_rec_det
    ; exa_self_rec_det_inf
    ; exa_rec_1
    ; exa_rec_2
    ; exa_par_1
    ; exa_self_act1
    ]
;;

let run_all
      ?(params : logging_params = default_logging_params ~mode:(OCaml ()) ())
      ()
  : unit
  =
  log ~params "\nRunning Tests.ml:\n\n";
  let ks90_results : (string * bool * bool) list = run_all_ks90 ~params () in
  log ~params (pstr_results [ "KS90", ks90_results ]);
  log ~params "\n\nEnd of Tests.ml.\n"
;;

(** To run tests...

    - First build the project:

    make .merlin clean; dune build; coq_makefile -f _CoqProject -o CoqMakeFile; make -f CoqMakeFile

    - Next, run the tests:

    _build/default/test/tests.exe *)
let () = run_all ()
