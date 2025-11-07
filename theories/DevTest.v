Require Import MEBI.loader.
MeBi Divider "Theories.DevTest".

Inductive label : Set := | A | B | C.

Definition label_eq (a b:label) : bool :=
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

Definition action_eq (a b:action) : bool :=
  match a, b with
  | send a, send b => label_eq a b
  | recv a, recv b => label_eq a b
  | _, _ => false
  end.

Inductive term : Set :=
| trec : term
| tend : term
| tfix : term -> term
| tact : action -> term -> term
| tpar : term -> term -> term 
| tseq : term -> term -> term 
.

Fixpoint term_eq (a b:term) : bool :=
  match a, b with
  | trec, trec => true
  | tend, tend => true
  | tfix a, tfix b => term_eq a b
  | tact a ac, tact b bc => action_eq a b && term_eq ac bc
  | tpar al ar, tpar bl br => term_eq al bl && term_eq ar br
  | tseq al ar, tseq bl br => term_eq al bl && term_eq ar br
  | _, _ => false
  end.

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

(* These below capture "structural congruence": using "silent" transitions *)
| do_fix : 
  (* forall t t', t' = tsubst (tfix t) t -> termLTS (tfix t) None t' *)
  forall t , termLTS (tfix t) None (tsubst (tfix t) t)

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

MeBi Set ShowAny      True.
MeBi Set ShowNotices  True.
MeBi Set ShowDebug    True.
MeBi Set WeakMode     True.

MeBi Set Weak Option label.

(* MeBi Divider "Theories.DevTest.0".
MeBi FSM (tend) Using termLTS. *)

MeBi Divider "Theories.DevTest.0.1".
(* MeBi FSM (tseq tend tend) Using termLTS. *)

(* MeBi Divider "Theories.DevTest.0.2". *)
(* MeBi FSM (tpar tend tend) Using termLTS. *)

(* MeBi Divider "Theories.DevTest.0.3". *)
MeBi FSM (tseq (tpar tend tend) tend) Using termLTS.

(* MeBi Divider "Theories.DevTest.1".
MeBi FSM (tseq (tpar (tact (send A) tend) (tact (recv A) tend)) tend) Using termLTS. *)

(* MeBi Divider "Theories.DevTest.2".
MeBi FSM (tseq (tpar (tact (send A) tend) (tact (recv A) tend)) (tfix (tseq (tpar (tact (send A) tend) (tact (recv A) tend)) trec))) Using termLTS. *)


