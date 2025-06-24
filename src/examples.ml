(** [Examples] contains examples to be used by either [KS90] or [PT87]. *)

type bisim_exa =
  { s : Fsm.t
  ; t : Fsm.t
  ; are_bisimilar : bool
  }

type minim_exa = { the_fsm : Fsm.t }

type exa_kind =
  | Bisim of bisim_exa
  | Minim of minim_exa
  | Weak of bisim_exa
  | Saturate of Fsm.t

(** [example] is a type for denoting pairs of fsms that we check are bisimilar.
*)
type example =
  { name : string
  ; kind : exa_kind
  }

let exa (name : string) (kind : exa_kind) : example = { name; kind }

(** [exa_1] is `Example 3.2.5` on page 106. *)
let exa_1 : example =

  let s : Lts.t = 

  let (s : Fsm.t) =
    Fsm.create_from (LTS ( ))
      (Lts.Create.lts
         ~init:"s0"
         (Nested
            ( [ "s0", [ "a", [ "s1"; "s2" ] ]
              ; "s1", [ "b", [ "s2" ] ]
              ; "s2", [ "b", [ "s2" ] ]
              ]
            , None )))
  and
    (* t *)
      (t : fsm)
    =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"t0"
         (Nested ([ "t0", [ "a", [ "t1" ] ]; "t1", [ "b", [ "t1" ] ] ], None)))
  in
  exa "exa1" (Bisim { s; t; are_bisimilar = true })
;;

(** [exa_2] is `Example 3.2.6` on page 107. *)
let exa_2 : example =
  (* s *)
  let (s : fsm) =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"s0"
         (Nested
            ( [ "s0", [ "a", [ "s1"; "s2" ] ]
              ; "s1", [ "a", [ "s3" ]; "b", [ "s4" ] ]
              ; "s2", [ "a", [ "s4" ] ]
              ; "s3", [ "a", [ "s0" ] ]
              ; "s4", [ "a", [ "s0" ] ]
              ]
            , None )))
  and
    (* t *)
      (t : fsm)
    =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"t0"
         (Nested
            ( [ "t0", [ "a", [ "t1"; "t2" ] ]
              ; "t1", [ "a", [ "t5" ]; "b", [ "t4"; "t5" ] ]
              ; "t2", [ "a", [ "t0" ] ]
              ; "t3", [ "a", [ "t4" ] ]
              ; "t4", [ "a", [ "t0" ] ]
              ; "t5", [ "a", [ "t0"; "t4" ] ]
              ]
            , None )))
  in
  exa "exa2" (Bisim { s; t; are_bisimilar = false })
;;

(** [exa_mc] ...
    I was wondering about how to encode mixed-states using either mixed-choice or silent transitions -- Jonah
*)
let exa_mc : example =
  (* [s] has a mixed-choice *)
  let (s : fsm) =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"s0"
         (Nested
            ( [ "s0", [ "send", [ "s1" ]; "recv", [ "s2" ]; "silent", [ "s0" ] ]
              ; "s1", [ "recv", [ "s3" ] ]
              ; "s2", [ "send", [ "s3" ] ]
              ]
            , None )))
  and
    (* [t] has silent transitions *)
      (t : fsm)
    =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"t0"
         (Nested
            ( [ "t0", [ "send", [ "t1" ]; "silent", [ "t2" ] ]
              ; "t1", [ "recv", [ "t4" ] ]
              ; "t2", [ "recv", [ "t3" ]; "silent", [ "t0" ] ]
              ; "t3", [ "send", [ "t4" ] ]
              ]
            , None )))
  in
  exa "exa_mc" (Bisim { s; t; are_bisimilar = false })
;;

(** [exa_self_rec_nondet] ... *)
let exa_self_rec_nondet : example =
  (* s *)
  let (s : fsm) =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"s0"
         (Nested
            ([ "s0", [ "a", [ "s1" ] ]; "s1", [ "a", [ "s1"; "s2" ] ] ], None)))
  and
    (* t *)
      (t : fsm)
    =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"t0"
         (Nested
            ([ "t0", [ "a", [ "t1" ] ]; "t1", [ "a", [ "t1"; "t2" ] ] ], None)))
  in
  exa "exa_self_rec_nondet" (Bisim { s; t; are_bisimilar = true })
;;

(** [exa_self_rec_nondet_inf] ... *)
let exa_self_rec_nondet_inf : example =
  (* s *)
  let (s : fsm) =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"s0"
         (Nested
            ( [ "s0", [ "a", [ "s1" ] ]
              ; "s1", [ "a", [ "s1"; "s2" ] ]
              ; "s2", [ "a", [ "s0" ] ]
              ]
            , None )))
  and
    (* t *)
      (t : fsm)
    =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"t0"
         (Nested
            ( [ "t0", [ "a", [ "t1" ] ]
              ; "t1", [ "a", [ "t1"; "t2" ] ]
              ; "t2", [ "a", [ "t0" ] ]
              ]
            , None )))
  in
  exa "exa_self_rec_nondet_inf" (Bisim { s; t; are_bisimilar = true })
;;

(** [exa_self_rec_det] ... *)
let exa_self_rec_det : example =
  (* s *)
  let (s : fsm) =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"s0"
         (Nested
            ( [ "s0", [ "a", [ "s1" ] ]; "s1", [ "a", [ "s1" ]; "b", [ "s2" ] ] ]
            , None )))
  and
    (* t *)
      (t : fsm)
    =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"t0"
         (Nested
            ( [ "t0", [ "a", [ "t1" ] ]; "t1", [ "a", [ "t1" ]; "b", [ "t2" ] ] ]
            , None )))
  in
  exa "exa_self_rec_det" (Bisim { s; t; are_bisimilar = true })
;;

(** [exa_self_rec_det_inf] ... *)
let exa_self_rec_det_inf : example =
  (* s *)
  let (s : fsm) =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"s0"
         (Nested
            ( [ "s0", [ "a", [ "s1" ] ]
              ; "s1", [ "a", [ "s1"; "s2" ] ]
              ; "s2", [ "a", [ "s2" ] ]
              ]
            , None )))
  and
    (* t *)
      (t : fsm)
    =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"t0"
         (Nested
            ( [ "t0", [ "a", [ "t1" ] ]
              ; "t1", [ "a", [ "t1"; "t2" ] ]
              ; "t2", [ "a", [ "t2" ] ]
              ]
            , None )))
  in
  exa "exa_self_rec_det_inf" (Bisim { s; t; are_bisimilar = true })
;;

(** [exa_rec_1] ... *)
let exa_rec_1 : example =
  (* s *)
  let (s : fsm) =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"s0"
         (Nested ([ "s0", [ "a", [ "s1" ] ]; "s1", [ "b", [ "s0" ] ] ], None)))
  and
    (* t *)
      (t : fsm)
    =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"t0"
         (Nested
            ( [ "t0", [ "a", [ "t1" ] ]
              ; "t1", [ "b", [ "t2" ] ]
              ; "t2", [ "a", [ "t3" ] ]
              ; "t3", [ "b", [ "t0" ] ]
              ]
            , None )))
  in
  exa "exa_rec_1" (Bisim { s; t; are_bisimilar = true })
;;

(** [exa_rec_2] ... *)
let exa_rec_2 : example =
  (* s *)
  let (s : fsm) =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"s0"
         (Nested ([ "s0", [ "a", [ "s1" ] ]; "s1", [ "b", [ "s0" ] ] ], None)))
  and
    (* t *)
      (t : fsm)
    =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"t0"
         (Nested
            ( [ "t0", [ "a", [ "t1" ] ]
              ; "t1", [ "b", [ "t2" ] ]
              ; "t2", [ "a", [ "t1" ] ]
              ]
            , None )))
  in
  exa "exa_rec_2" (Bisim { s; t; are_bisimilar = true })
;;

(** [exa_par_1] ... *)
let exa_par_1 : example =
  (* s *)
  let (s : fsm) =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"s0"
         (Nested
            ( [ "s0", [ "a", [ "s1" ]; "b", [ "s2" ] ]
              ; "s1", [ "b", [ "s3" ] ]
              ; "s2", [ "a", [ "s3" ] ]
              ]
            , None )))
  and
    (* t *)
      (t : fsm)
    =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"t0"
         (Nested
            ( [ "t0", [ "a", [ "t1" ]; "b", [ "t1" ] ]
              ; "t1", [ "a", [ "t2" ]; "b", [ "t2" ] ]
              ]
            , None )))
  in
  exa "exa_par_1" (Bisim { s; t; are_bisimilar = false })
;;

(** [exa_self_act1] ... *)
let exa_self_act1 : example =
  (* s *)
  let (s : fsm) =
    Translate.to_fsm
      (Lts.Create.lts ~init:"s0" (Nested ([ "s0", [ "a", [ "s1" ] ] ], None)))
  and
    (* t *)
      (t : fsm)
    =
    Translate.to_fsm
      (Lts.Create.lts ~init:"t0" (Nested ([ "t0", [ "a", [ "t1" ] ] ], None)))
  in
  exa "exa_self_act1" (Bisim { s; t; are_bisimilar = true })
;;

(** [exa_saturated1] ... *)
let exa_saturated1 : example =
  (* s *)
  let (s : fsm) =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"1"
         (Nested
            ( [ "1", [ "a", [ "3" ]; "~", [ "2" ] ]
              ; "2", [ "b", [ "4" ]; "~", [ "1" ] ]
              ; "3", [ "c", [ "5" ] ]
              ; "4", [ "d", [ "6" ] ]
              ]
            , None )))
  in
  exa "exa_saturated1" (Saturate s)
;;

(** [exa_saturated2] ... *)
let exa_saturated2 : example =
  (* s *)
  let (s : fsm) =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"1"
         (Nested
            ( [ "1", [ "a", [ "2" ]; "b", [ "3" ]; "~", [ "4" ] ]
              ; "2", [ "e", [ "6" ] ]
              ; "3", [ "~", [ "2"; "4" ]; "d", [ "5" ] ]
              ; "4", [ "c", [ "5" ]; "~", [ "2" ] ]
              ; "5", [ "g", [ "7" ] ]
              ; "6", [ "~", [ "2" ]; "f", [ "7" ] ]
              ]
            , None )))
  in
  exa "exa_saturated2" (Saturate s)
;;

(** [exa_weak1] ... *)
let exa_weak1 : example =
  (* s *)
  let (s : fsm) =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"s0"
         (Nested
            ( [ "s0", [ "a", [ "s1" ]; "~", [ "s2" ] ]
              ; "s1", [ "b", [ "s3" ] ]
              ; "s2", [ "c", [ "s4" ]; "~", [ "s0" ] ]
              ; "s4", [ "d", [ "s5" ] ]
              ]
            , None )))
  and
    (* t *)
      (t : fsm)
    =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"t0"
         (Nested
            ( [ "t0", [ "a", [ "t1" ]; "c", [ "t2" ] ]
              ; "t1", [ "b", [ "t3" ] ]
              ; "t2", [ "d", [ "t4" ] ]
              ]
            , None )))
  in
  exa "exa_weak1" (Weak { s; t; are_bisimilar = true })
;;

(** [exa_weak2] ... *)
let exa_weak2 : example =
  (* s *)
  let (s : fsm) =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"s1"
         (Nested
            ( [ "s1", [ "a", [ "s2" ]; "b", [ "s3" ]; "~", [ "s4" ] ]
              ; "s2", [ "e", [ "s6" ] ]
              ; "s3", [ "~", [ "s2" ]; "~", [ "s4" ]; "d", [ "s5" ] ]
              ; "s4", [ "c", [ "s5" ]; "~", [ "s2" ] ]
              ; "s5", [ "g", [ "s7" ] ]
              ; "s6", [ "~", [ "s2" ]; "f", [ "s7" ] ]
              ]
            , None )))
  and
    (* t *)
      (t : fsm)
    =
    Translate.to_fsm
      (Lts.Create.lts
         ~init:"t1"
         (Nested
            ( [ ( "t1"
                , [ "a", [ "t2" ]
                  ; "b", [ "t3" ]
                  ; "c", [ "t5" ]
                  ; "e", [ "t2"; "t6" ]
                  ] )
              ; "t2", [ "e", [ "t2"; "t6" ] ]
              ; "t3", [ "d", [ "t5" ]; "e", [ "t2"; "t6" ]; "c", [ "t5" ] ]
              ; "t5", [ "g", [ "t7" ] ]
              ; "t6", [ "f", [ "t7" ]; "e", [ "t6" ] ]
              ]
            , None )))
  in
  exa "exa_weak2" (Weak { s; t; are_bisimilar = true })
;;
