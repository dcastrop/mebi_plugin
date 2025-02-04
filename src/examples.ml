(** [Examples] contains examples to be used by either [KS90] or [PT87]. *)
open Fsm

(** [example] is a type for denoting pairs of fsms that we check are bisimilar. *)
type example =
  { name : string
  ; s : fsm
  ; t : fsm
  ; are_bisimilar : bool
  }

let exa (name : string) (s : fsm) (t : fsm) (are_bisimilar : bool) : example =
  { name; s; t; are_bisimilar }
;;

(** [exa_1] is `Example 3.2.5` on page 106. *)
let exa_1 : example =
  (* s *)
  let (s : fsm) =
    make_fsm_from_lts
      "s0"
      [ "s0", [ "a", [ "s1"; "s2" ] ]
      ; "s1", [ "b", [ "s2" ] ]
      ; "s2", [ "b", [ "s2" ] ]
      ]
  and (* t *)
    (t : fsm) =
    make_fsm_from_lts "t0" [ "t0", [ "a", [ "t1" ] ]; "t1", [ "b", [ "t1" ] ] ]
  in
  exa "exa1" s t true
;;

(** [exa_2] is `Example 3.2.6` on page 107. *)
let exa_2 : example =
  (* s *)
  let (s : fsm) =
    make_fsm_from_lts
      "s0"
      [ "s0", [ "a", [ "s1"; "s2" ] ]
      ; "s1", [ "a", [ "s3" ]; "b", [ "s4" ] ]
      ; "s2", [ "a", [ "s4" ] ]
      ; "s3", [ "a", [ "s0" ] ]
      ; "s4", [ "a", [ "s0" ] ]
      ]
  and (* t *)
    (t : fsm) =
    make_fsm_from_lts
      "t0"
      [ "t0", [ "a", [ "t1"; "t2" ] ]
      ; "t1", [ "a", [ "t5" ]; "b", [ "t4"; "t5" ] ]
      ; "t2", [ "a", [ "t0" ] ]
      ; "t3", [ "a", [ "t4" ] ]
      ; "t4", [ "a", [ "t0" ] ]
      ; "t5", [ "a", [ "t0"; "t4" ] ]
      ]
  in
  exa "exa2" s t false
;;

(** [exa_mc] ...
    I was wondering about how to encode mixed-states using either mixed-choice or silent transitions -- Jonah *)
let exa_mc : example =
  (* [s] has a mixed-choice *)
  let (s : fsm) =
    make_fsm_from_lts
      "s0"
      [ "s0", [ "send", [ "s1" ]; "recv", [ "s2" ]; "silent", [ "s0" ] ]
      ; "s1", [ "recv", [ "s3" ] ]
      ; "s2", [ "send", [ "s3" ] ]
      ]
  and (* [t] has silent transitions *)
    (t : fsm) =
    make_fsm_from_lts
      "t0"
      [ "t0", [ "send", [ "t1" ]; "silent", [ "t2" ] ]
      ; "t1", [ "recv", [ "t4" ] ]
      ; "t2", [ "recv", [ "t3" ]; "silent", [ "t0" ] ]
      ; "t3", [ "send", [ "t4" ] ]
      ]
  in
  exa "exa_mc" s t false
;;

(** [exa_self_rec_nondet] ... *)
let exa_self_rec_nondet : example =
  (* s *)
  let (s : fsm) =
    make_fsm_from_lts
      "s0"
      [ "s0", [ "a", [ "s1" ] ]; "s1", [ "a", [ "s1"; "s2" ] ] ]
  and (* t *)
    (t : fsm) =
    make_fsm_from_lts
      "t0"
      [ "t0", [ "a", [ "t1" ] ]; "t1", [ "a", [ "t1"; "t2" ] ] ]
  in
  exa "exa_self_rec_nondet" s t true
;;

(** [exa_self_rec_nondet_inf] ... *)
let exa_self_rec_nondet_inf : example =
  (* s *)
  let (s : fsm) =
    make_fsm_from_lts
      "s0"
      [ "s0", [ "a", [ "s1" ] ]
      ; "s1", [ "a", [ "s1"; "s2" ] ]
      ; "s2", [ "a", [ "s0" ] ]
      ]
  and (* t *)
    (t : fsm) =
    make_fsm_from_lts
      "t0"
      [ "t0", [ "a", [ "t1" ] ]
      ; "t1", [ "a", [ "t1"; "t2" ] ]
      ; "t2", [ "a", [ "t0" ] ]
      ]
  in
  exa "exa_self_rec_nondet_inf" s t true
;;

(** [exa_self_rec_det] ... *)
let exa_self_rec_det : example =
  (* s *)
  let (s : fsm) =
    make_fsm_from_lts
      "s0"
      [ "s0", [ "a", [ "s1" ] ]; "s1", [ "a", [ "s1" ]; "b", [ "s2" ] ] ]
  and (* t *)
    (t : fsm) =
    make_fsm_from_lts
      "t0"
      [ "t0", [ "a", [ "t1" ] ]; "t1", [ "a", [ "t1" ]; "b", [ "t2" ] ] ]
  in
  exa "exa_self_rec_det" s t true
;;

(** [exa_self_rec_det_inf] ... *)
let exa_self_rec_det_inf : example =
  (* s *)
  let (s : fsm) =
    make_fsm_from_lts
      "s0"
      [ "s0", [ "a", [ "s1" ] ]
      ; "s1", [ "a", [ "s1"; "s2" ] ]
      ; "s2", [ "a", [ "s2" ] ]
      ]
  and (* t *)
    (t : fsm) =
    make_fsm_from_lts
      "t0"
      [ "t0", [ "a", [ "t1" ] ]
      ; "t1", [ "a", [ "t1"; "t2" ] ]
      ; "t2", [ "a", [ "t2" ] ]
      ]
  in
  exa "exa_self_rec_det_inf" s t true
;;

(** [exa_rec_1] ... *)
let exa_rec_1 : example =
  (* s *)
  let (s : fsm) =
    make_fsm_from_lts "s0" [ "s0", [ "a", [ "s1" ] ]; "s1", [ "b", [ "s0" ] ] ]
  and (* t *)
    (t : fsm) =
    make_fsm_from_lts
      "t0"
      [ "t0", [ "a", [ "t1" ] ]
      ; "t1", [ "b", [ "t2" ] ]
      ; "t2", [ "a", [ "t3" ] ]
      ; "t3", [ "b", [ "t0" ] ]
      ]
  in
  exa "exa_rec_1" s t true
;;

(** [exa_rec_2] ... *)
let exa_rec_2 : example =
  (* s *)
  let (s : fsm) =
    make_fsm_from_lts "s0" [ "s0", [ "a", [ "s1" ] ]; "s1", [ "b", [ "s0" ] ] ]
  and (* t *)
    (t : fsm) =
    make_fsm_from_lts
      "t0"
      [ "t0", [ "a", [ "t1" ] ]
      ; "t1", [ "b", [ "t2" ] ]
      ; "t2", [ "a", [ "t1" ] ]
      ]
  in
  exa "exa_rec_2" s t true
;;

(** [exa_par_1] ... *)
let exa_par_1 : example =
  (* s *)
  let (s : fsm) =
    make_fsm_from_lts
      "s0"
      [ "s0", [ "a", [ "s1" ]; "b", [ "s2" ] ]
      ; "s1", [ "b", [ "s3" ] ]
      ; "s2", [ "a", [ "s3" ] ]
      ]
  and (* t *)
    (t : fsm) =
    make_fsm_from_lts
      "t0"
      [ "t0", [ "a", [ "t1" ]; "b", [ "t1" ] ]
      ; "t1", [ "a", [ "t2" ]; "b", [ "t2" ] ]
      ]
  in
  exa "exa_par_1" s t false
;;

(** [exa_self_act1] ... *)
let exa_self_act1 : example =
  (* s *)
  let (s : fsm) = make_fsm_from_lts "s0" [ "s0", [ "a", [ "s1" ] ] ]
  and (* t *)
    (t : fsm) = make_fsm_from_lts "t0" [ "t0", [ "a", [ "t1" ] ] ] in
  exa "exa_self_act1" s t true
;;
