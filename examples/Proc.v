Require Import MEBI.loader.

Inductive action : Set := | ASend | ARecv | BSend | BRecv | CSend | CRecv.

Inductive term : Set :=
| trec : term
| tend : term
| tfix : term -> term
| tact : action -> term -> term
| tpar : term -> term -> term (* parallel *)
| tbra : term -> term -> term (* branching *)
.

Fixpoint tsubst (t1 : term) (t2 : term) :=
  match t2 with
  | trec => t1
  | tend => tend
  | tfix t => tfix t
  | tact a t => tact a (tsubst t1 t)
  | tpar tl tr => tpar (tsubst t1 tl) (tsubst t1 tr)
  | tbra tl tr => tbra (tsubst t1 tl) (tsubst t1 tr)
  end.

(* true: "comm", false: "silent" *)
Inductive termLTS : term -> bool -> term -> Prop :=
(* syncrhonous communication, i.e., send/recv in parallel *)

| do_senda : forall tl tr,                          (* 0*)
    termLTS (tpar (tact ASend tl) (tact ARecv tr))
      true  (tpar tl tr)

| do_sendb : forall tl tr,                          (* 1*)
    termLTS (tpar (tact BSend tl) (tact BRecv tr))
      true  (tpar tl tr)

| do_sendc : forall tl tr,                          (* 2*)
    termLTS (tpar (tact CSend tl) (tact CRecv tr))
      true  (tpar tl tr)

(* non-deterministic choice between tl and tr *)
| do_bral : forall a tl tr tl',                     (* 3*)
    termLTS tl a tl' ->
    termLTS (tbra tl tr) a tl'

| do_brar : forall a tl tr tr',                     (* 4*)
    termLTS tr a tr' ->
    termLTS (tbra tl tr) a tr'

(* non-deterministic parallel step *)
| do_parl : forall a tl tl' tr,                     (* 5*)
    termLTS tl a tl' ->
    termLTS (tpar tl tr) a (tpar tl' tr)

| do_parr : forall a tl tr tr',                     (* 6*)
    termLTS tr a tr' ->
    termLTS (tpar tl tr) a (tpar tl tr')

(* These below capture "structural congruence": using "silent" transitions *)
| do_fix : forall t,                                (* 7*)
    termLTS (tfix t) false (tsubst (tfix t) t)

| do_comm : forall tl tr,                           (* 8*)
    termLTS (tpar tl tr) false (tpar tr tl)

| do_assocl : forall t1 t2 t3,                      (* 9*)
    termLTS (tpar t1 (tpar t2 t3)) false (tpar (tpar t1 t2) t3)

| do_assocr : forall t1 t2 t3,                      (*10*)
    termLTS (tpar (tpar t1 t2) t3) false (tpar t1 (tpar t2 t3))

(* Below are attempts to "clean up" recursive terms. *)
(* | do_clean :                                        (*11*)
    termLTS (tpar tend tend) false tend

| do_clean_l : forall tr,                           (*12*)
    termLTS (tpar tend tr) false tr

| do_clean_r : forall tl,                           (*13*)
    termLTS (tpar tl tend) false tl *)
.


Inductive transitive_closure : term -> Prop :=
| trans_step : forall t a t', termLTS t a t' ->
                              transitive_closure t' ->
                              transitive_closure t

| no_step : forall t, transitive_closure t
.

(*************************************)
(** Basic: No recursion or branches **)
(*************************************)

Example proc0_send0 := tact ASend tend. (* empty lts *)
(* MeBi Show LTS Bounded 150 Of proc0_send0 Using termLTS. *)

Example proc0_send1a := tpar (tact ASend tend)
                             (tact ARecv tend).
(* MeBi Show LTS Bounded 150 Of proc0_send1a Using termLTS. *)

Goal termLTS proc0_send1a true (tpar tend tend).
  unfold proc0_send1a. eapply do_senda. Qed.

Goal transitive_closure proc0_send1a.
  unfold proc0_send1a.
  eapply trans_step. eapply do_senda.
  constructor.
Qed.

Example proc0_send1b := tpar (tact ARecv tend)
                             (tact ASend tend).
(* MeBi Show LTS Bounded 150 Of proc0_send1a Using termLTS. *)

Example proc0_send2 := tpar (tact ASend (tact ARecv tend))
                            (tact ARecv (tact ASend tend)).
(* MeBi Show LTS Bounded 150 Of proc0_send2 Using termLTS. *)

Goal transitive_closure proc0_send2.
  unfold proc0_send2.
  eapply trans_step. apply do_senda.
  eapply trans_step. apply do_comm.
  eapply trans_step. apply do_senda.
  constructor.
Qed.

Example proc0_send3 := tpar (tact ASend (tact BSend tend))
                            (tact ARecv (tact BRecv tend)).
(* MeBi Show LTS Bounded 150 Of proc0_send3 Using termLTS. *)

Example proc0_send4 := tpar (tact BSend (tact ARecv tend))
                            (tact BRecv (tact ASend tend)).
(* MeBi Show LTS Bounded 150 Of proc0_send4 Using termLTS. *)



(*************************************)
(** Simple Recursion, no Branching ***)
(*************************************)

Example proc1_rec1 := tfix trec.
(* MeBi Show LTS Bounded 150 Of proc1_rec1 Using termLTS. *)
(* MeBi Dump "proc1_rec1" LTS Bounded 150 Of proc1_rec1 Using termLTS. *)

Example proc1_rec2 := tpar (tfix (tact ASend trec))
                           (tfix (tact ARecv trec)).
(* MeBi Show LTS Bounded 150 Of proc1_rec2 Using termLTS. *)
(* MeBi Dump "proc1_rec2" LTS Bounded 150 Of proc1_rec2 Using termLTS. *)

Example proc1_rec3 := tpar (tfix (tact ASend (tact BSend trec)))
                           (tfix (tact ARecv (tact BRecv trec))).
(* MeBi Show LTS Bounded 150 Of proc1_rec3 Using termLTS. *)
(* MeBi Dump "proc1_rec3" LTS Bounded 150 Of proc1_rec3 Using termLTS. *)

Example proc1_rec4 := tpar (tact ASend (tfix (tact BSend trec)))
                           (tact ARecv (tfix (tact BRecv trec))).
(* MeBi Show LTS Bounded 150 Of proc1_rec4 Using termLTS. *)
(* MeBi Dump "proc1_rec4" LTS Bounded 150 Of proc1_rec4 Using termLTS. *)


(*************************************)
(** Branching, no Recursion **********)
(*************************************)

(** the example below does nothing since neither branch can occur *)
(* Example proc2_bra1 := tpar (tbra (tact ASend tend)
                                 (tact BSend tend))
                           (tbra (tact ARecv tend)
                                 (tact BRecv tend)).
(* MeBi Show LTS Bounded 150 Of proc2_bra1 Using termLTS. *)
MeBi Dump "proc2_bra1" LTS Bounded 150 Of proc2_bra1 Using termLTS. *)

Example proc2_bra2 := tbra (tpar (tact ASend tend)
                                 (tact ARecv tend))
                           (tpar (tact BSend tend)
                                 (tact BRecv tend)).
(* MeBi Show LTS Bounded 150 Of proc2_bra2 Using termLTS. *)
(* MeBi Dump "proc2_bra2" LTS Bounded 150 Of proc2_bra2 Using termLTS. *)

Example proc2_bra3 := tbra (tpar (tact ASend (tact BSend tend))
                                 (tact ARecv (tact BRecv tend)))
                           (tpar (tact BSend (tact ASend tend))
                                 (tact BRecv (tact ARecv tend))).
(* MeBi Show LTS Bounded 150 Of proc2_bra3 Using termLTS. *)
(* MeBi Dump "proc2_bra3" LTS Bounded 150 Of proc2_bra3 Using termLTS. *)

Example proc2_bra4 := tbra (tpar (tact ASend (tact BSend tend))
                                 (tact ARecv (tact BRecv tend)))
                           (tbra (tpar (tact BSend (tact CSend tend))
                                       (tact BRecv (tact CRecv tend)))
                                 (tpar (tact CSend (tact ASend tend))
                                       (tact CRecv (tact ARecv tend)))).
(* MeBi Show LTS Bounded 150 Of proc2_bra4 Using termLTS. *)
(* MeBi Dump "proc2_bra4" LTS Bounded 150 Of proc2_bra4 Using termLTS. *)

(** the example below does nothing since neither branch can occur *)
(* Example proc2_bra5 := tpar (tact ARecv (tact BRecv tend))
                           (tbra (tact ASend (tact BSend tend))
                                 (tact BSend (tact ASend tend))).
(* MeBi Show LTS Bounded 150 Of proc2_bra5 Using termLTS. *)
MeBi Dump "proc2_bra5" LTS Bounded 150 Of proc2_bra5 Using termLTS. *)


(*************************************)
(** Simple Branching and Recursion ***)
(*************************************)

(* NOTE: the examples below caused a state explosion. This is partially due *)
(* to their defintion, but also due to the [tend] and [tpar] not being      *)
(* cleaned up. *)

Example proc3_recbra1 := tfix (tbra (tpar (tact ASend trec)
                                          (tact ARecv trec))
                                    (tpar (tact BSend tend)
                                          (tact BRecv tend))).
(* MeBi Show LTS Bounded 1000 Of proc3_recbra1 Using termLTS. *)
MeBi Dump "proc3_recbra1" LTS Bounded 1000 Of proc3_recbra1 Using termLTS.

(* NOTE: duplicates start appearing at exactly 135 and onwards. *)
(* MeBi Dump "proc3_recbra1" LTS Bounded 135 Of proc3_recbra1 Using termLTS. *)


(* NOTE: with the [do_clean] additions to the semantics, duplicates start appearing at exactly 120 and onwards. *)
(* MeBi Dump "proc3_recbra1" LTS Bounded 120 Of proc3_recbra1 Using termLTS. *)

Example proc3_recbra2 := tfix (tbra (tpar (tact ASend trec)
                                          (tact ARecv trec))
                                    (tpar (tact BSend trec)
                                          (tact BRecv trec))).
(* MeBi Show LTS Bounded 1000 Of proc3_recbra2 Using termLTS. *)
(* MeBi Dump "proc3_recbra2" LTS Bounded 2000 Of proc3_recbra2 Using termLTS. *)

Example proc3_recbra3 := tbra (tfix (tpar (tact ASend trec)
                                          (tact ARecv trec)))
                              (tfix (tpar (tact BSend trec)
                                          (tact BRecv trec))).
(* MeBi Show LTS Bounded 1000 Of proc3_recbra3 Using termLTS. *)
(* MeBi Dump "proc3_recbra3" LTS Bounded 2000 Of proc3_recbra3 Using termLTS. *)

(*************************************)
(** Nested Branching and Recursion ***)
(*************************************)




Example proc1 := tpar (tfix (tact ASend trec)) (tfix (tact ARecv trec)).
(* MeBi Show LTS Bounded 150 Of proc1 Using termLTS. *)
(* MeBi Dump "proc1" LTS Bounded 1000 Of proc1 Using termLTS. *)

Example proc2 := tpar proc1 proc1.
(* MeBi Show LTS Bounded 500 Of proc2 Using termLTS. *)
(* MeBi Dump "proc2" LTS Bounded 500 Of proc2 Using termLTS. *)


(* TODO: would be cool to do first a "FSM minimisation". I believe there are
 many bisimilar states here, where the order (or assoc) of the processes is
 completely irrelevant *)
(* MeBi Show LTS Bounded 150 Of proc1 Using termLTS. *)
(* MeBi Dump "proc0" FSM Bounded 150 Of proc1 Using termLTS. *)


Inductive comp : Set :=
| cterm : term -> comp
| cpar : comp -> comp -> comp
.

(* second layer *)
Inductive compLTS : comp -> bool -> comp -> Prop :=
| do_t : forall a t t', termLTS t a t' -> compLTS (cterm t) a (cterm t')

| do_l : forall a l l' r, compLTS l a l' -> compLTS (cpar l r) a (cpar l' r)
| do_r : forall a l r r', compLTS r a r' -> compLTS (cpar l r) a (cpar l r')

| do_l_end : forall a r, compLTS (cpar (cterm tend) r) a r
| do_r_end : forall a l, compLTS (cpar l (cterm tend)) a l
.

Example comp0 := cpar (cterm proc1_rec1) (cterm proc1_rec1).
(* MeBi Show LTS Bounded 150 Of comp0 Using termLTS compLTS. *)
(* MeBi Dump "comp0" LTS Bounded 350 Of comp0 Using termLTS compLTS. *)

Example comp1a := cpar (cterm proc1) (cterm tend).
(* MeBi Show LTS Bounded 150 Of comp1a Using termLTS compLTS. *)
(* MeBi Dump "comp1a" LTS Bounded 350 Of comp1a Using termLTS compLTS. *)

Example comp1b := cpar (cterm proc1) (cterm proc1).
(* MeBi Show LTS Bounded 150 Of comp1b Using termLTS compLTS. *)
(* MeBi Dump "comp1b" LTS Bounded 5000 Of comp1b Using termLTS compLTS. *)

