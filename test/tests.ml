open Mebi_plugin.Fsm
open Mebi_plugin.Bisimilarity
open Mebi_plugin.Utils
open Mebi_plugin.Examples

(** [ks90_exas] ... *)
let rec ks90_exas
  ?(show : bool = false)
  ?(details : bool = false)
  ?(debug : bool = false)
  (exas : example list)
  : unit
  =
  match exas with
  | [] -> ()
  | exa :: exas' ->
    (match exa with
     | { name; s; t; _ } ->
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
       let result = RCP.KS90.run ~show s t in
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
          ks90_exas ~show ~debug exas'))
;;

let run_all_ks90
  ?(show : bool = false)
  ?(details : bool = false)
  ?(debug : bool = false)
  ()
  : unit
  =
  ks90_exas ~show ~details ~debug [ exa_1; exa_2 ]
;;

let run_all
  ?(show : bool = false)
  ?(details : bool = false)
  ?(debug : bool = false)
  ()
  : unit
  =
  print ~show "\nRunning Tests.ml:\n\n";
  run_all_ks90 ~show ~details ~debug ();
  print ~show "\n\nEnd of Tests.ml.\n"
;;

(** To run tests...

    - First build the project:

    make .merlin clean; dune build; coq_makefile -f _CoqProject -o CoqMakeFile; make -f CoqMakeFile

    - Next, run the tests:

    _build/default/test/tests.exe *)
let () = run_all ~show:true ~details:true ~debug:false ()
