Require Import MEBI.loader.

Inductive action : Set := | ASend | ARecv .

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
| do_send : forall tl tr, termLTS (tpar (tact ASend tl) (tact ARecv tr))
                            true  (tpar tl tr)

| do_parl : forall a tl tl' tr,
    termLTS tl a tl' ->
    termLTS (tpar tl tr) a (tpar tl' tr)

| do_parr : forall a tl tr tr',
    termLTS tr a tr' ->
    termLTS (tpar tl tr) a (tpar tl tr')

(* These below capture "structural congruence": using "silent" transitions *)
| do_fix : forall t,
    termLTS (tfix t) false (tsubst (tfix t) t)

| do_comm : forall tl tr,
    termLTS (tpar tl tr) false (tpar tr tl)

| do_assocl : forall t1 t2 t3,
    termLTS (tpar t1 (tpar t2 t3)) false (tpar (tpar t1 t2) t3)

| do_assocr : forall t1 t2 t3,
    termLTS (tpar (tpar t1 t2) t3) false (tpar t1 (tpar t2 t3))
.

Example proc0 := tfix trec.

(* MeBi Show LTS Bounded 150 Of proc0 Using termLTS. *)

Example proc1 := tpar (tfix (tact ASend trec)) (tfix (tact ARecv trec)).

(* TODO: would be cool to do first a "FSM minimisation". I believe there are
 many bisimilar states here, where the order (or assoc) of the processes is
 completely irrelevant *)
(* MeBi Show LTS Bounded 150 Of proc1 Using termLTS. *)
(* MeBi Dump "proc0" FSM Bounded 150 Of proc1 Using termLTS. *)
