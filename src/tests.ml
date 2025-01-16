open Fsm
open Bisimilarity
open Pp_ext

(** [ks90_exas] ... *)
let rec ks90_exas
          ?(coq : bool = false)
          ?(show : bool = false)
          ?(debug : bool = false)
          (exas : RCP.Examples.example list)
  : unit
  =
  match exas with
  | [] -> ()
  | exa :: exas' ->
    (match exa with
     | { name; s; t; _ } ->
       (* safely print depending on if coq or not *)
       handle_pp
         ~coq
         ~show:true
         ~debug:false
         (Printf.sprintf
            "%s\n%s\n\n%s\n\n"
            (Printf.sprintf "\n= = = = = = = = = =\nRCP.KS90 (%s)\n" name)
            (Printf.sprintf
               "%s.s: %s"
               name
               (pstr ~options:(if debug then Debug () else Default ()) (Fsm s)))
            (Printf.sprintf
               "%s.t: %s"
               name
               (pstr ~options:(if debug then Debug () else Default ()) (Fsm t))));
       (* run algorithm *)
       let result = RCP.KS90.run ~coq ~show ~debug s t in
       (match result with
        | { are_bisimilar; bisimilar_states; non_bisimilar_states; _ } ->
          (* print out results *)
          handle_pp
            ~coq
            ~show:true
            ~debug:false
            (Printf.sprintf
               "%s\n%s\n%s\n\n"
               (Printf.sprintf
                  "[KS90] (%s) Results: (s ~ t) = %b.\n\n\
                   Bisimilar states: %s.\n\n\
                   Non-bisimilar states: %s.\n"
                  name
                  are_bisimilar
                  (pstr
                     ~tabs:1
                     (pp_wrap_as_supported (Partition bisimilar_states)))
                  (pstr
                     ~tabs:1
                     (pp_wrap_as_supported (Partition non_bisimilar_states))))
               (match debug with
                | true ->
                  Printf.sprintf
                    "where s = %s\n\nand t = %s.\n"
                    (pstr (pp_wrap_as_supported (Fsm s)))
                    (pstr (pp_wrap_as_supported (Fsm t)))
                | false -> "")
               (Printf.sprintf "= = = = = = = = = ="));
          (* continue *)
          ks90_exas ~coq ~show ~debug exas'))
;;

let run_all_ks90
      ?(coq : bool = false)
      ?(show : bool = false)
      ?(debug : bool = false)
      ()
  : unit
  =
  ks90_exas ~coq ~show ~debug [ RCP.Examples.exa_1; RCP.Examples.exa_2 ]
;;

let run_all
      ?(coq : bool = false)
      ?(show : bool = false)
      ?(debug : bool = false)
      ()
  : unit
  =
  handle_pp ~coq ~show:true ~debug:true "\nRunning Tests.ml:\n\n";
  run_all_ks90 ~coq ~show ~debug ();
  handle_pp ~coq ~show:true ~debug:true "\n\nEnd of Tests.ml.\n"
;;

(* ! vscoq language server breaks if this function somehow causes [Printf.printf] to be called. *)
let () = run_all ~coq:true ()
