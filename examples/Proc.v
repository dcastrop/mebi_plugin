Require Import MEBI.loader.

Inductive label : Set := | A | B | C.

Inductive action : Set := | ASend | ARecv | BSend | BRecv | CSend | CRecv.

Inductive term : Set :=
| trec : term
| tend : term
| tfix : term -> term
| tact : action -> term -> term
| tpar : term -> term -> term 
| tseq : term -> term -> term 
.

Fixpoint tsubst (t1 : term) (t2 : term) :=
  match t2 with
  | trec => t1
  | tend => tend
  | tfix t => tfix t
  | tact a t => tact a (tsubst t1 t)
  | tpar tl tr => tpar (tsubst t1 tl) (tsubst t1 tr)
  | tseq t s => tseq t (tsubst t1 s)
  end.

(* true: "comm", false: "silent" *)
Inductive termLTS : term -> option label -> term -> Prop :=

(* syncrhonous communication, i.e., send/recv in parallel *)
| do_senda : forall tl tr,                          (* 0*)
    termLTS (tpar (tact ASend tl) (tact ARecv tr))
      (Some A)  (tpar tl tr)

| do_sendb : forall tl tr,                          (* 1*)
    termLTS (tpar (tact BSend tl) (tact BRecv tr))
      (Some B)  (tpar tl tr)

| do_sendc : forall tl tr,                          (* 2*)
    termLTS (tpar (tact CSend tl) (tact CRecv tr))
      (Some C)  (tpar tl tr)

(* asyncrhonous communication, i.e., send/recv in parallel *)
(* | do_senda : forall tl tr,                          (* 0*)
    termLTS (tpar (tact ASend tl) (tact ARecv tr))
      true  (tpar tl tr)

| do_sendb : forall tl tr,                          (* 1*)
    termLTS (tpar (tact BSend tl) (tact BRecv tr))
      true  (tpar tl tr)

| do_sendc : forall tl tr,                          (* 2*)
    termLTS (tpar (tact CSend tl) (tact CRecv tr))
      true  (tpar tl tr) *)

(* non-deterministic parallel step *)
(* | do_parl : forall a tl tl' tr,                     (* 3*)
    termLTS tl a tl' ->
    termLTS (tpar tl tr) a (tpar tl' tr)

| do_parr : forall a tl tr tr',                     (* 4*)
    termLTS tr a tr' ->
    termLTS (tpar tl tr) a (tpar tl tr') *)
| do_seq : forall t t' s a,
    termLTS t a t' -> termLTS (tseq t s) a (tseq t' s)

| do_seq_end : forall s, termLTS (tseq tend s) None s
| do_par_end : termLTS (tpar tend tend) None tend

(* These below capture "structural congruence": using "silent" transitions *)
| do_fix : forall t,                                (* 5*)
    termLTS (tfix t) None (tsubst (tfix t) t)

| do_comm : forall tl tr,                           (* 6*)
    termLTS (tpar tl tr) None (tpar tr tl)

| do_assocl : forall t1 t2 t3,                      (* 7*)
    termLTS (tpar t1 (tpar t2 t3)) None (tpar (tpar t1 t2) t3)

| do_assocr : forall t1 t2 t3,                      (* 8*)
    termLTS (tpar (tpar t1 t2) t3) None (tpar t1 (tpar t2 t3))
.


(* Module WithBranching.
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
End WithBranching. *)

(* Import WithBranching. *)

Inductive transitive_closure : term -> Prop :=
| trans_step : forall t a t', termLTS t a t' ->
                              transitive_closure t' ->
                              transitive_closure t

| no_step : forall t, transitive_closure t
.





(****************************************************************************)
(** Layered Semantics *******************************************************)
(****************************************************************************)

Inductive comp : Set :=
| cterm : term -> comp
| cpar : comp -> comp -> comp
.

Inductive compLTS : comp -> option label -> comp -> Prop :=
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

