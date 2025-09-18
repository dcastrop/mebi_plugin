(** To run tests...

    - First build the project:

    make .merlin clean; dune build; coq_makefile -f _CoqProject -o CoqMakeFile; make -f CoqMakeFile

    - Next, run the tests:

    _build/default/test/tests.exe *)
(* let () =
   let open Saturation in
   dump "m1" m1;
   dump "m2" m2;
   ()
   ;; *)

(* open Mebi_plugin.Fsm
(* open Mebi_plugin.Bisimilarity *)
open Mebi_plugin.Utils
open Mebi_plugin.Logging
open Mebi_plugin.Examples

exception UnexpectedExampleKind of Mebi_plugin.Examples.exa_kind

(**  *)
let pstr_results
  ?(params : Params.log = Params.Default.log ~mode:(OCaml ()) ())
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
           "%s(=>) %s: [%s-----------------------------\n].\n\n"
           acc
           suite_name
           (List.fold_left
              (fun (acc' : string)
                ((name, expected_result, actual_result) : string * bool * bool) ->
                Printf.sprintf
                  "%s%s | %s  | %s  | %s\n"
                  acc'
                  (if expected_result == actual_result then " " else "X")
                  (if expected_result then "true " else "false")
                  (if actual_result then "true " else "false")
                  name)
              "\n? | EXPECT | ACTUAL | EXAMPLE\n-----------------------------\n"
              suite_results))
       "\n"
       results)
    (List.for_all
       (fun ((_suite_name, suite_results) :
              string * (string * bool * bool) list) ->
         List.for_all
           (fun ((_name, expected_result, actual_result) : string * bool * bool) ->
             expected_result == actual_result)
           suite_results)
       results)
;;

let pstr_exa_bisim
  ?(params : Params.log = Params.Default.log ~mode:(OCaml ()) ())
  (name : string)
  (s : fsm)
  (t : fsm)
  : string
  =
  Printf.sprintf
    "\n\
     = = = = = = = = = = = = = = = = = = =\n\
     RCP.KS90 (%s)\n\n\
     %s.s: %s.\n\n\
     %s.t: %s.\n\n"
    name
    name
    (Mebi_plugin.Fsm.PStr.fsm ~params:(Log params) s)
    name
    (Mebi_plugin.Fsm.PStr.fsm ~params:(Log params) t)
;;

(* let pstr_exa_bisim_result
  ?(params : Params.log = Params.Default.log ~mode:(OCaml ()) ())
  (name : string)
  (result : bisim_result)
  : string
  =
  match result with
  | { are_bisimilar; merged_fsm; bisimilar_states; non_bisimilar_states } ->
    Printf.sprintf
      "[KS90] (%s) Results: (s ~ t) = %b.\n\n\
       Bisimilar states: %s.\n\n\
       Non-bisimilar states: %s.\n\n\
       Using merged FSM: %s.\n\n\
       = = = = = = = = = = = = = = = = = = =\n\n"
      name
      are_bisimilar
      (Mebi_plugin.Fsm.PStr.partition ~params:(Log params) bisimilar_states)
      (Mebi_plugin.Fsm.PStr.partition ~params:(Log params) non_bisimilar_states)
      (Mebi_plugin.Fsm.PStr.fsm ~params:(Log params) merged_fsm)
;; *)

(* let rec ks90_run_bisim
  ?(params : Params.log = Params.Default.log ~mode:(OCaml ()) ())
  ((name, kind) : string * exa_kind)
  ((s, t) : fsm * fsm)
  (weak : RCP.KS90.check_weak_bisim)
  : result
  =
  (* safely print depending on if coq or not *)
  log ~params (pstr_exa_bisim ~params name s t);
  (* run algorithm *)
  let raw_result : of_bisim_result =
    RCP.KS90.run ~params (ToMerge (s, t)) weak
  in
  RCP.KS90.result ~params raw_result

(** [ks90_exas] ... *)
and ks90_exas
  ?(params : Params.log = Params.Default.log ~mode:(OCaml ()) ())
  (exas : example list)
  : (string * bool * bool) list
  =
  params.kind <- Details ();
  match exas with
  | [] -> []
  | exa :: exas' ->
    (match exa with
     | { name; kind } ->
       let (result, expected_result) : result * bool =
         match kind with
         | Bisim { s; t; are_bisimilar } ->
           ks90_run_bisim ~params (name, kind) (s, t) (), are_bisimilar
         | Weak { s; t; are_bisimilar } ->
           ks90_run_bisim ~params (name, kind) (s, t) Weak, are_bisimilar
         | _ -> raise (UnexpectedExampleKind kind)
       in
       (match result with
        | BisimResult result' ->
          (* print out results *)
          params.kind <- Details ();
          log ~params (pstr_exa_bisim_result ~params name result');
          (* continue *)
          (name, expected_result, result'.are_bisimilar)
          :: ks90_exas ~params exas'
        | _ -> raise (UnexpectedBisimResult result)))
;; *)

(* let run_all_ks90
  ?(params : Params.log = Params.Default.log ~mode:(OCaml ()) ())
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
    ; exa_weak1
    ; exa_weak2
    ]
;; *)

let run_all ?(params : Params.log = Params.Default.log ~mode:(OCaml ()) ()) ()
  : unit
  =
  log ~params "\nRunning Tests.ml:\n\n";
  log ~params "\n\nTODO (fix after refactoring bisimilarity)\n\n";
  (* let ks90_results : (string * bool * bool) list = run_all_ks90 ~params () in
  log ~params (pstr_results [ "KS90", ks90_results ]); *)
  log ~params "\n\nEnd of Tests.ml.\n"
;;

exception QuickTestFailed of example

let quick_test_saturate_fsm
  ?(params : Params.log = Params.Default.log ~mode:(OCaml ()) ())
  (example_to_test : Mebi_plugin.Examples.example)
  ()
  : unit
  =
  (* params.kind <- Details (); *)
  log ~params (Printf.sprintf "\nQuickTest: %s." example_to_test.name);
  let to_test : Mebi_plugin.Fsm.fsm =
    match example_to_test.kind with
    | Saturate s -> s
    | _ -> raise (QuickTestFailed example_to_test)
  in
  log
    ~params
    (Printf.sprintf
       "\nUnsaturated: %s."
       (Mebi_plugin.Fsm.PStr.fsm ~params:(Log params) to_test));
  let saturated : Mebi_plugin.Fsm.fsm =
    Mebi_plugin.Fsm.Saturate.fsm ~params to_test
  in
  log
    ~params
    (Printf.sprintf
       "\nSaturated: %s."
       (Mebi_plugin.Fsm.PStr.fsm ~params:(Log params) saturated));
  log ~params (Printf.sprintf "\nEnd of Tests.ml (%s)" example_to_test.name)
;;

let quick_test_saturate_fsm_states
  ?(params : Params.log = Params.Default.log ~mode:(OCaml ()) ())
  (example_to_test : Mebi_plugin.Examples.example)
  ()
  : unit
  =
  (* params.kind <- Details (); *)
  log ~params (Printf.sprintf "\nQuickTest: %s." example_to_test.name);
  let to_test : Mebi_plugin.Fsm.fsm =
    match example_to_test.kind with
    | Saturate s -> s
    | _ -> raise (QuickTestFailed example_to_test)
  in
  log
    ~params
    (Printf.sprintf
       "\nUnsaturated: %s."
       (Mebi_plugin.Fsm.PStr.fsm ~params:(Log params) to_test));
  let saturated : Mebi_plugin.Fsm.fsm =
    Mebi_plugin.Fsm.Saturate.fsm_states ~params to_test
  in
  log
    ~params
    (Printf.sprintf
       "\nSaturated: %s."
       (Mebi_plugin.Fsm.PStr.fsm ~params:(Log params) saturated));
  log ~params (Printf.sprintf "\nEnd of Tests.ml (%s)" example_to_test.name)
;;

(** To run tests...

    - First build the project:

    make .merlin clean; dune build; coq_makefile -f _CoqProject -o CoqMakeFile; make -f CoqMakeFile

    - Next, run the tests:

    _build/default/test/tests.exe *)
(* let () = run_all () *)

let () =
  (* quick_test_saturate_fsm Mebi_plugin.Examples.exa_saturated1 (); *)
  quick_test_saturate_fsm_states Mebi_plugin.Examples.exa_saturated1 ();
  (*  *)
  (* quick_test_saturate_fsm Mebi_plugin.Examples.exa_saturated2 (); *)
  quick_test_saturate_fsm_states Mebi_plugin.Examples.exa_saturated2 ();
  ()
;; *)
