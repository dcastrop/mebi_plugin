Require Import MEBI.loader.

Inductive label : Set := | A | B | C.

Definition label_eq : forall a b : label, {a = b} + {a <> b}.
  decide equality.
Defined.

Definition label_eqb (a b : label): bool := 
  match a, b with
  | A, A => true
  | B, B => true
  | C, C => true
  | _, _ => false
  end.

Inductive action : Set := 
| send : label -> action 
| recv : label -> action 
.

Definition action_label (a : action) : label :=
  match a with
  | send l => l
  | recv l => l
  end.

Definition action_eq : forall a b : action, {a = b} + {a <> b}.
  intros [[]|[]] [[]|[]]; decide equality; decide equality.
    (* (left; reflexivity) || (right; intros; discriminate H). *)
Defined.

Inductive term : Set :=
| trec : term
| tend : term
| tfix : term -> term
| tact : action -> term -> term
| tpar : term -> term -> term 
| tseq : term -> term -> term 
.

Definition term_eq : forall a b : term, {a = b} + {a <> b}.
  intros [] []; decide equality; decide equality; decide equality.
Defined.

Fixpoint tsubst (t1 : term) (t2 : term) :=
  match t2 with
  | trec => t1
  | tend => tend
  | tfix t => tfix t
  | tact a t => tact a (tsubst t1 t)
  | tpar tl tr => tpar (tsubst t1 tl) (tsubst t1 tr)
  | tseq t s => tseq t (tsubst t1 s)
  end.

Inductive termLTS : term -> option label -> term -> Prop :=

| do_handshake : 
  forall a l r, 
  termLTS (tpar (tact (send a) l) (tact (recv a) r)) (Some a) (tpar l r)

| do_seq : 
  forall t t' s a, termLTS t a t' -> termLTS (tseq t s) a (tseq t' s)

| do_seq_end : forall s, termLTS (tseq tend s) None s

| do_par_end : termLTS (tpar tend tend) None tend

| do_end : termLTS tend None tend

(* These below capture "structural congruence": using "silent" transitions *)
| do_fix : 
  forall t t', t' = tsubst (tfix t) t -> termLTS (tfix t) None t'

| do_comm : 
  forall tl tr, termLTS (tpar tl tr) None (tpar tr tl)

| do_assocl : forall t1 t2 t3, 
    termLTS (tpar t1 (tpar t2 t3)) None (tpar (tpar t1 t2) t3)

| do_assocr : forall t1 t2 t3, 
    termLTS (tpar (tpar t1 t2) t3) None (tpar t1 (tpar t2 t3))
.

Inductive termLTS_tc : term -> Prop :=
| termLTS_ts : forall t a t', termLTS t a t' -> termLTS_tc t' -> termLTS_tc t
| termLTS_ns : forall t, termLTS_tc t
.

(****************************************************************************)
(** Layered Semantics *******************************************************)
(****************************************************************************)

Inductive comp : Set :=
| cprc : term -> comp
| cpar : comp -> comp -> comp
.

Inductive compLTS : comp -> option label -> comp -> Prop :=
| do_t : forall a t1 t2, termLTS t1 a t2 -> compLTS (cprc t1) a (cprc t2)

| do_parl : forall a l1 l2 r,
    compLTS l1 a l2 -> compLTS (cpar l1 r) a (cpar l2 r)

| do_parr : forall a l r1 r2,
    compLTS r1 a r2 -> compLTS (cpar l r1) a (cpar l r2)

| do_l_end : forall a r, compLTS (cpar (cprc tend) r) a r
| do_r_end : forall a l, compLTS (cpar l (cprc tend)) a l
.

Inductive compLTS_tc : comp -> Prop :=
| compLTS_ts : forall t a t', compLTS t a t' -> compLTS_tc t' -> compLTS_tc t
| compLTS_ns : forall t, compLTS_tc t
.
