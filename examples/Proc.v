Require Import MEBI.loader.

Inductive action : Set := | ASend | ARecv | BSend | BRecv | CSend | CRecv.

Module NoBranching.

  Inductive term : Set :=
  | trec : term
  | tend : term
  | tfix : term -> term
  | tact : action -> term -> term
  | tpar : term -> term -> term
  .

  Fixpoint tsubst (t1 : term) (t2 : term) :=
    match t2 with
    | trec => t1
    | tend => tend
    | tfix t => tfix t
    | tact a t => tact a (tsubst t1 t)
    | tpar tl tr => tpar (tsubst t1 tl) (tsubst t1 tr)
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

  (* non-deterministic parallel step *)
  | do_parl : forall a tl tl' tr,                     (* 3*)
      termLTS tl a tl' ->
      termLTS (tpar tl tr) a (tpar tl' tr)

  | do_parr : forall a tl tr tr',                     (* 4*)
      termLTS tr a tr' ->
      termLTS (tpar tl tr) a (tpar tl tr')

  (* These below capture "structural congruence": using "silent" transitions *)
  | do_fix : forall t,                                (* 5*)
      termLTS (tfix t) false (tsubst (tfix t) t)

  | do_comm : forall tl tr,                           (* 6*)
      termLTS (tpar tl tr) false (tpar tr tl)

  | do_assocl : forall t1 t2 t3,                      (* 7*)
      termLTS (tpar t1 (tpar t2 t3)) false (tpar (tpar t1 t2) t3)

  | do_assocr : forall t1 t2 t3,                      (* 8*)
      termLTS (tpar (tpar t1 t2) t3) false (tpar t1 (tpar t2 t3))
  .
End NoBranching.

Module WithBranching.
  Inductive term : Set :=
  | trec : term
  | tend : term
  | tfix : term -> term
  | tact : action -> term -> term
  | tpar : term -> term -> term

  (* active selection/branching *)
  (* | tbra : term -> term -> term  *)

  (* passive option selection/branching *)
  (* | topt : term -> term -> term  *)
  .

  Fixpoint tsubst (t1 : term) (t2 : term) :=
    match t2 with
    | trec => t1
    | tend => tend
    | tfix t => tfix t
    | tact a t => tact a (tsubst t1 t)
    | tpar tl tr => tpar (tsubst t1 tl) (tsubst t1 tr)
    (* | tbra tl tr => tbra (tsubst t1 tl) (tsubst t1 tr) *)
    (* | topt tl tr => topt (tsubst t1 tl) (tsubst t1 tr) *)
    end.

  (* "comm", false: "silent" *)
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

  (* non-deterministic parallel step *)
  | do_parl : forall a tl tl' tr,                     (* 3*)
      termLTS tl a tl' ->
      termLTS (tpar tl tr) a (tpar tl' tr)

  | do_parr : forall a tl tr tr',                     (* 4*)
      termLTS tr a tr' ->
      termLTS (tpar tl tr) a (tpar tl tr')

  (* These below capture "structural congruence": using "silent" transitions *)
  | do_fix : forall t,                                (* 5*)
      termLTS (tfix t) false (tsubst (tfix t) t)

  | do_comm : forall tl tr,                           (* 6*)
      termLTS (tpar tl tr) false (tpar tr tl)

  | do_assocl : forall t1 t2 t3,                      (* 7*)
      termLTS (tpar t1 (tpar t2 t3)) false (tpar (tpar t1 t2) t3)

  | do_assocr : forall t1 t2 t3,                      (* 8*)
      termLTS (tpar (tpar t1 t2) t3) false (tpar t1 (tpar t2 t3))

  (* non-deterministic selection between tl and tr *)
  (* | do_bral : forall a tl tr tl',                     (* 9*)
      termLTS tl a tl' ->
      termLTS (tbra tl tr) a tl'

  | do_brar : forall a tl tr tr',                     (*10*)
      termLTS tr a tr' ->
      termLTS (tbra tl tr) a tr' *)

  (* non-deterministic branch in reaction to ready party p *)
  (* | do_optl : forall tl tr p c,                       (*11*)
      termLTS (tpar p tl) true c ->
      termLTS (tpar p (topt tl tr)) true c

  | do_optr : forall tl tr p c,                       (*12*)
      termLTS (tpar p tr) true c ->
      termLTS (tpar p (topt tl tr)) true c *)
  .
End WithBranching.

Import NoBranching.
(* Import WithBranching. *)

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
(* MeBi Dump "proc0_send0" LTS Bounded 150 Of proc0_send0 Using termLTS. *)


Example proc0_send1a := tpar (tact ASend tend)
                             (tact ARecv tend).
(* MeBi Show LTS Bounded 150 Of proc0_send1a Using termLTS. *)
(* MeBi Dump "proc0_send1a" LTS Bounded 150 Of proc0_send1a Using termLTS. *)

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
(* MeBi Dump "proc0_send1b" LTS Bounded 150 Of proc0_send1b Using termLTS. *)


Example proc0_send2 := tpar (tact ASend (tact ARecv tend))
                            (tact ARecv (tact ASend tend)).
(* MeBi Show LTS Bounded 150 Of proc0_send2 Using termLTS. *)
(* MeBi Dump "proc0_send2" LTS Bounded 150 Of proc0_send2 Using termLTS. *)

(* Goal transitive_closure proc0_send2.
  unfold proc0_send2.
  eapply trans_step. apply do_senda.
  eapply trans_step. apply do_comm.
  eapply trans_step. apply do_senda.
  constructor.
Qed. *)


Example proc0_send3 := tpar (tact ASend (tact BSend tend))
                            (tact ARecv (tact BRecv tend)).
(* MeBi Show LTS Bounded 150 Of proc0_send3 Using termLTS. *)
(* MeBi Dump "proc0_send3" LTS Bounded 150 Of proc0_send3 Using termLTS. *)


Example proc0_send4 := tpar (tact BSend (tact ARecv tend))
                            (tact BRecv (tact ASend tend)).
(* MeBi Show LTS Bounded 150 Of proc0_send4 Using termLTS. *)
(* MeBi Dump "proc0_send4" LTS Bounded 150 Of proc0_send4 Using termLTS. *)



(*************************************)
(** Recursion ************************)
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


Example proc1_rec5 := tpar (tfix (tact ASend (tact ASend trec)))
                           (tfix (tact ARecv trec)).
(* MeBi Show LTS Bounded 150 Of proc1_rec5 Using termLTS. *)
(* MeBi Dump "proc1_rec5" LTS Bounded 150 Of proc1_rec5 Using termLTS. *)


Example proc1_rec6 := tpar (tact ASend (tact BSend (tfix (tact ASend trec))))
                           (tpar (tfix (tact ARecv trec))
                                 (tact BSend tend)).
(* MeBi Show LTS Bounded 150 Of proc1_rec6 Using termLTS. *)
(* MeBi Dump "proc1_rec6" LTS Bounded 150 Of proc1_rec6 Using termLTS. *)


(* FIXME: grows forever by accumulating [tend]. (need structural equiv.) *)
Example proc1_rec7 := tfix (tpar (tact ASend (tact BSend trec))
                                 (tact ARecv (tact BRecv tend))).
(* MeBi Show LTS Bounded 150 Of proc1_rec7 Using termLTS. *)
(* MeBi Dump "proc1_rec7" LTS Bounded 150 Of proc1_rec7 Using termLTS. *)






(****************************************************************************)
(** Layered Semantics *******************************************************)
(****************************************************************************)

Inductive comp : Set :=
| cterm : term -> comp
| cpar : comp -> comp -> comp
.

Inductive compLTS : comp -> bool -> comp -> Prop :=
| do_t : forall a t t',
    termLTS t a t' ->
    compLTS (cterm t) a (cterm t')

| do_l : forall a l l' r,
    compLTS l a l' ->
    compLTS (cpar l r) a (cpar l' r)

| do_r : forall a l r r',
    compLTS r a r' ->
    compLTS (cpar l r) a (cpar l r')

| do_l_end : forall a r,
    compLTS (cpar (cterm tend) r) a r

| do_r_end : forall a l,
    compLTS (cpar l (cterm tend)) a l
.

(*************************************)
(** Basic: No recursion or branches **)
(*************************************)

Example comp1_rec1 := cpar (cterm proc1_rec1) (cterm proc1_rec1).
(* MeBi Show LTS Bounded 150 Of comp0_send1 Using termLTS compLTS. *)
(* MeBi Dump "comp1_rec1" LTS Bounded 350 Of comp1_rec1 Using termLTS compLTS. *)

(*************************************)
(** Recursion ************************)
(*************************************)

Example comp0_send1 := cpar (cterm proc0_send1a) (cterm proc0_send1b).
(* MeBi Show LTS Bounded 150 Of comp1_rec1 Using termLTS compLTS. *)
(* MeBi Dump "comp0_send1" LTS Bounded 350 Of comp0_send1 Using termLTS compLTS. *)






(****************************************************************************)
(** Problematic Terms (Branching) *******************************************)
(** (Requires [tbra] and [topt] ) *******************************************)
(****************************************************************************)
(** Issues arise with branching AND recursion *******************************)
(****************************************************************************)

(* FIXME: these examples grow infinitely without some structural congruence. *)

Inductive bad_term : Set :=
| bad_trec : bad_term
| bad_tend : bad_term
| bad_tfix : bad_term -> bad_term
| bad_tact : action -> bad_term -> bad_term

(* parallel *)
| bad_tpar : bad_term -> bad_term -> bad_term

(* active selection/branching *)
| bad_tbra : bad_term -> bad_term -> bad_term

(* passive option selection/branching *)
| bad_topt : bad_term -> bad_term -> bad_term
.

Fixpoint bad_tsubst (t1 : bad_term) (t2 : bad_term) :=
  match t2 with
  | bad_trec => t1
  | bad_tend => bad_tend
  | bad_tfix t => bad_tfix t
  | bad_tact a t => bad_tact a (bad_tsubst t1 t)
  | bad_tpar tl tr => bad_tpar (bad_tsubst t1 tl) (bad_tsubst t1 tr)
  | bad_tbra tl tr => bad_tbra (bad_tsubst t1 tl) (bad_tsubst t1 tr)
  | bad_topt tl tr => bad_topt (bad_tsubst t1 tl) (bad_tsubst t1 tr)
  end.

(* true: "comm", false: "silent" *)
Inductive bad_termLTS : bad_term -> bool -> bad_term -> Prop :=
(* syncrhonous communication, i.e., send/recv in parallel *)

| bad_do_senda : forall tl tr,                          (* 0*)
    bad_termLTS (bad_tpar (bad_tact ASend tl) (bad_tact ARecv tr))
      true  (bad_tpar tl tr)

| bad_do_sendb : forall tl tr,                          (* 1*)
    bad_termLTS (bad_tpar (bad_tact BSend tl) (bad_tact BRecv tr))
      true  (bad_tpar tl tr)

| bad_do_sendc : forall tl tr,                          (* 2*)
    bad_termLTS (bad_tpar (bad_tact CSend tl) (bad_tact CRecv tr))
      true  (bad_tpar tl tr)

(* non-deterministic parallel step *)
| bad_do_parl : forall a tl tl' tr,                     (* 3*)
    bad_termLTS tl a tl' ->
    bad_termLTS (bad_tpar tl tr) a (bad_tpar tl' tr)

| bad_do_parr : forall a tl tr tr',                     (* 4*)
    bad_termLTS tr a tr' ->
    bad_termLTS (bad_tpar tl tr) a (bad_tpar tl tr')

(* These below capture "structural congruence": using "silent" transitions *)
| bad_do_fix : forall t,                                (* 5*)
    bad_termLTS (bad_tfix t) false (bad_tsubst (bad_tfix t) t)

| bad_do_comm : forall tl tr,                           (* 6*)
    bad_termLTS (bad_tpar tl tr) false (bad_tpar tr tl)

| bad_do_assocl : forall t1 t2 t3,                      (* 7*)
    bad_termLTS (bad_tpar t1 (bad_tpar t2 t3)) false (bad_tpar (bad_tpar t1 t2) t3)

| bad_do_assocr : forall t1 t2 t3,                      (* 8*)
    bad_termLTS (bad_tpar (bad_tpar t1 t2) t3) false (bad_tpar t1 (bad_tpar t2 t3))

(* non-deterministic selection between tl and tr *)
| bad_do_bral : forall a tl tr tl',                     (* 9*)
    bad_termLTS tl a tl' ->
    bad_termLTS (bad_tbra tl tr) a tl'

| bad_do_brar : forall a tl tr tr',                     (*10*)
    bad_termLTS tr a tr' ->
    bad_termLTS (bad_tbra tl tr) a tr'

(* non-deterministic branch in reaction to ready party p *)
| bad_do_optl : forall tl tr p c,                       (*11*)
    bad_termLTS (bad_tpar p tl) true c ->
    bad_termLTS (bad_tpar p (bad_topt tl tr)) true c

| bad_do_optr : forall tl tr p c,                       (*12*)
    bad_termLTS (bad_tpar p tr) true c ->
    bad_termLTS (bad_tpar p (bad_topt tl tr)) true c


(* Below are attempts to "clean up" recursive terms. *)
(* | bad_do_clean :                                        (*13*)
    bad_termLTS (bad_tpar bad_tend bad_tend) false bad_tend

| bad_do_clean_l : forall tr,                           (*14*)
    bad_termLTS (bad_tpar bad_tend tr) false tr

| bad_do_clean_r : forall tl,                           (*15*)
    bad_termLTS (bad_tpar tl bad_tend) false tl *)
.


(*************************************)
(** Branching, no Recursion **********)
(*************************************)

Example bad_proc2_bra1 := bad_tbra (bad_tpar (bad_tact ASend bad_tend)
                                         (bad_tact ARecv bad_tend))
                               (bad_tpar (bad_tact BSend bad_tend)
                                         (bad_tact BRecv bad_tend)).
(* MeBi Show LTS Bounded 150 Of bad_proc2_bra1 Using bad_termLTS. *)
(* MeBi Dump "bad_proc2_bra1" LTS Bounded 150 Of bad_proc2_bra1 Using bad_termLTS. *)

Example bad_proc2_bra2 :=
    bad_tbra (bad_tpar (bad_tact ASend (bad_tact BSend bad_tend))
                       (bad_tact ARecv (bad_tact BRecv bad_tend)))
             (bad_tpar (bad_tact BSend (bad_tact ASend bad_tend))
                       (bad_tact BRecv (bad_tact ARecv bad_tend))).
(* MeBi Show LTS Bounded 150 Of bad_proc2_bra2 Using bad_termLTS. *)
(* MeBi Dump "bad_bad_proc2_bra2" LTS Bounded 150 Of bad_proc2_bra2 Using bad_termLTS. *)

Example bad_proc2_bra3 :=
    bad_tbra (bad_tpar (bad_tact ASend (bad_tact BSend bad_tend))
                       (bad_tact ARecv (bad_tact BRecv bad_tend)))
             (bad_tbra (bad_tpar (bad_tact BSend (bad_tact CSend bad_tend))
                                 (bad_tact BRecv (bad_tact CRecv bad_tend)))
                       (bad_tpar (bad_tact CSend (bad_tact ASend bad_tend))
                                 (bad_tact CRecv (bad_tact ARecv bad_tend)))).
(* MeBi Show LTS Bounded 150 Of bad_proc2_bra3 Using bad_termLTS. *)
(* MeBi Dump "bad_bad_proc2_bra3" LTS Bounded 150 Of bad_proc2_bra3 Using bad_termLTS. *)


(*************************************)
(** Simple Branching and Recursion ***)
(*************************************)

Example bad_proc3_recbra1 :=
    bad_tbra (bad_tfix (bad_tpar (bad_tact ASend bad_trec)
                                 (bad_tact ARecv bad_trec)))
             (bad_tpar (bad_tact BSend bad_tend)
                       (bad_tact BRecv bad_tend)).
(* MeBi Show LTS Bounded 1000 Of bad_proc3_recbra1 Using bad_termLTS. *)
(* MeBi Dump "bad_proc3_recbra1" LTS Bounded 50 Of bad_proc3_recbra1 Using bad_termLTS. *)


Example bad_proc3_recbra2 :=
    bad_tfix (bad_tbra (bad_tpar (bad_tact ASend bad_trec)
                                 (bad_tact ARecv bad_trec))
                       (bad_tpar (bad_tact BSend bad_tend)
                                 (bad_tact BRecv bad_tend))).
(* MeBi Show LTS Bounded 1000 Of bad_proc3_recbra2 Using bad_termLTS. *)
(* MeBi Dump "bad_proc3_recbra2" LTS Bounded 5000 Of bad_proc3_recbra2 Using bad_termLTS. *)


Example bad_proc3_recbra3 :=
    bad_tfix (bad_tbra (bad_tpar (bad_tact ASend bad_trec)
                                 (bad_tact ARecv bad_trec))
                       (bad_tpar (bad_tact BSend bad_trec)
                                 (bad_tact BRecv bad_trec))).
(* MeBi Show LTS Bounded 1000 Of bad_proc3_recbra3 Using bad_termLTS. *)
(* MeBi Dump "bad_proc3_recbra3" LTS Bounded 2000 Of bad_proc3_recbra3 Using bad_termLTS. *)

Example bad_proc3_recbra4 :=
    bad_tbra (bad_tfix (bad_tpar (bad_tact ASend bad_trec)
                                 (bad_tact ARecv bad_trec)))
             (bad_tfix (bad_tpar (bad_tact BSend bad_trec)
                                 (bad_tact BRecv bad_trec))).
(* MeBi Show LTS Bounded 1000 Of bad_proc3_recbra4 Using bad_termLTS. *)
(* MeBi Dump "bad_proc3_recbra4" LTS Bounded 2000 Of bad_proc3_recbra4 Using bad_termLTS. *)

(*************************************)
(** Nested Branching and Recursion ***)
(*************************************)


(* the below example sends [ASend] and then [BSend]. *)
Example bad_proc1_nestedrec1 :=
    bad_tfix (bad_tpar ((bad_tact ASend (bad_tact BSend bad_trec)))
                       (bad_tfix (bad_topt (bad_tact ARecv bad_trec)
                                           (bad_tact BRecv bad_tend)))).

(* Goal transitive_closure bad_proc1_nestedrec1.
  unfold bad_proc1_nestedrec1.
  eapply trans_step. eapply do_fix. simpl.
  eapply trans_step. eapply do_parr. eapply do_fix. simpl.
  eapply trans_step. eapply do_optl. eapply do_senda.
  eapply trans_step. eapply do_parr. eapply do_fix. simpl.
  eapply trans_step. eapply do_optr. eapply do_sendb.
  eapply trans_step. eapply do_clean_r.
  constructor.
Qed. *)

(* MeBi Show LTS Bounded 150 Of bad_proc1_nestedrec1 Using bad_termLTS. *)
(* MeBi Dump "bad_proc1_nestedrec1" LTS Bounded 50 Of bad_proc1_nestedrec1 Using bad_termLTS. *)









(* TODO: would be cool to do first a "FSM minimisation". I believe there are
 many bisimilar states here, where the order (or assoc) of the processes is
 completely irrelevant *)
(* MeBi Show LTS Bounded 150 Of proc1 Using termLTS. *)
(* MeBi Dump "proc0" FSM Bounded 150 Of proc1 Using termLTS. *)

