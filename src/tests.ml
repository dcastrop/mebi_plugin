open Pp
open Fsm
open Bisimilarity

(** [bisim_exa1_ks90] *)
let bisim_exa1_ks90 : unit =
  let s, t = RCP.Examples.exa_1 in
  Feedback.msg_debug
    (str (Printf.sprintf "\n= = = = = = = = = =\nRCP.KS90 (Exa1)\n"));
  Feedback.msg_warning (str (Printf.sprintf "exa1.s: %s" (pstr (Fsm s))));
  Feedback.msg_warning
    (str (Printf.sprintf "exa1.s: %s" (pstr ~options:(Debug ()) (Fsm s))));
  Feedback.msg_warning (str (Printf.sprintf "exa1.t: %s" (pstr (Fsm t))));
  Feedback.msg_warning
    (str (Printf.sprintf "exa1.t: %s" (pstr ~options:(Debug ()) (Fsm t))));
  (* run algorithm *)
  let are_bisimilar, pi = RCP.KS90.run s t in
  (* print out results *)
  Feedback.msg_notice
    (str
       (Printf.sprintf
          "[KS90] (exa1) Results: (s ~ t) = %b.%s"
          are_bisimilar
          (if Bool.not are_bisimilar
           then
             Printf.sprintf
               "\nnon-bisimilar partition: %s"
               (pstr (pp_wrap_as_supported (Partition pi)))
           else "")));
  Feedback.msg_info
    (str
       (Printf.sprintf
          "where s = %s\nand t = %s."
          (pstr (pp_wrap_as_supported (Fsm s)))
          (pstr (pp_wrap_as_supported (Fsm t)))));
  Feedback.msg_debug
    (str
       (Printf.sprintf
          "\n--------\npi: %s"
          (pstr (pp_wrap_as_supported (Partition pi)))));
  Feedback.msg_debug (str (Printf.sprintf "\n= = = = = = = = = =\n"));
  ()
;;

(** [bisim_exa2_ks90] *)
let bisim_exa2_ks90 : unit =
  let s, t = RCP.Examples.exa_2 in
  Feedback.msg_debug
    (str (Printf.sprintf "\n= = = = = = = = = =\nRCP.KS90 (Exa2)\n"));
  Feedback.msg_warning (str (Printf.sprintf "exa2.s: %s" (pstr (Fsm s))));
  Feedback.msg_warning (str (Printf.sprintf "exa2.t: %s" (pstr (Fsm t))));
  (* run algorithm *)
  let are_bisimilar, pi = RCP.KS90.run s t in
  (* print out results *)
  Feedback.msg_notice
    (str
       (Printf.sprintf
          "[KS90] (exa2) Results: (s ~ t) = %b.%s"
          are_bisimilar
          (if Bool.not are_bisimilar
           then
             Printf.sprintf
               "\nnon-bisimilar partition: %s"
               (pstr (pp_wrap_as_supported (Partition pi)))
           else "")));
  Feedback.msg_info
    (str
       (Printf.sprintf
          "where s = %s\nand t = %s."
          (pstr (pp_wrap_as_supported (Fsm s)))
          (pstr (pp_wrap_as_supported (Fsm t)))));
  Feedback.msg_debug
    (str
       (Printf.sprintf
          "\n--------\npi: %s"
          (pstr (pp_wrap_as_supported (Partition pi)))));
  Feedback.msg_debug (str (Printf.sprintf "\n= = = = = = = = = =\n"));
  ()
;;
