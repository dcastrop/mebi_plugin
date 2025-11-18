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

(* | do_assocl : forall t1 t2 t3,  *)
    (* termLTS (tpar t1 (tpar t2 t3)) None (tpar (tpar t1 t2) t3) *)

(* | do_assocr : forall t1 t2 t3,  *)
    (* termLTS (tpar (tpar t1 t2) t3) None (tpar t1 (tpar t2 t3)) *)
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

(* MeBi Divider "Theories.DevTest.0.1". *)
(* MeBi FSM (tseq tend tend) Using termLTS. *)

(* MeBi Divider "Theories.DevTest.0.2". *)
(* MeBi FSM (tpar tend tend) Using termLTS. *)

(* MeBi Divider "Theories.DevTest.0.3". *)
(* MeBi FSM (tseq (tpar tend tend) tend) Using termLTS. *)
(* NOTE:        (tseq (tpar tend tend) tend)
  [ 1 -> [ 3 => (tseq tend tend)
         ; 5 => (tseq (tpar tend tend) tend)
         ]
  ] *)
(* NOTE:        (tseq tend tend)
  [ 1 =>        (tend)
  ] *)

(* MeBi Divider "Theories.DevTest.0.4". *)
(* MeBi FSM (tseq (tseq (tpar tend tend) (tpar tend tend)) (tpar tend tend)) Using termLTS. *)

(* MeBi Divider "Theories.DevTest.1". *)
(* MeBi FSM (tpar (tact (send A) tend) (tact (recv A) tend)) Using termLTS. *)

(* MeBi Divider "Theories.DevTest.1.1". *)
(* MeBi FSM (tseq (tpar (tact (send A) tend) (tact (recv A) tend)) tend) Using termLTS. *)

(* MeBi FSM (tseq (tpar (tact (send A) tend) (tact (recv A) tend)) (tfix (tseq (tpar (tact (send A) tend) (tact (recv A) tend)) trec))) Using termLTS. *)

(* testing for propagation *)
MeBi Divider "Theories.DevTest.2".
Inductive termLTS2 : term -> option label -> term -> Prop :=

| do2_send : 
  forall a t,
  termLTS2 (tact (send a) t) (Some a) t

| do2_recv : 
  forall a t,
  termLTS2 (tact (recv a) t) (Some a) t

| do2_handshake : 
  forall a l r, 
  termLTS2 (tact (send a) l) (Some a) l ->
  termLTS2 (tact (recv a) r) (Some a) r ->
  termLTS2 (tpar (tact (send a) l) (tact (recv a) r)) (Some a) (tpar l r)

| do2_seq : 
  forall t t' s a, termLTS2 t a t' -> termLTS2 (tseq t s) a (tseq t' s)

| do2_seq_end : forall s, termLTS2 (tseq tend s) None s

| do2_par_end : termLTS2 (tpar tend tend) None tend

(* These below capture "structural congruence": using "silent" transitions *)
| do2_fix : 
  (* forall t t', t' = tsubst (tfix t) t -> termLTS2 (tfix t) None t' *)
  forall t , termLTS2 (tfix t) None (tsubst (tfix t) t)

| do2_comm : 
  forall tl tr, termLTS2 (tpar tl tr) None (tpar tr tl)

(* | do_assocl : forall t1 t2 t3,  *)
    (* termLTS (tpar t1 (tpar t2 t3)) None (tpar (tpar t1 t2) t3) *)

(* | do_assocr : forall t1 t2 t3,  *)
    (* termLTS (tpar (tpar t1 t2) t3) None (tpar t1 (tpar t2 t3)) *)
.

(* MeBi LTS (tseq (tpar (tact (send A) tend) (tact (recv A) tend)) tend) Using termLTS2. *)
(* MeBi LTS (tseq (tpar (tact (send A) tend) (tact (recv A) tend)) (tfix (tseq (tpar (tact (send A) tend) (tact (recv A) tend)) trec))) Using termLTS2. *)




(* testing for separation *)
(* MeBi Divider "Theories.DevTest.3". *)
Inductive termLTS3 : term -> option label -> term -> Prop :=
(* 0 *)
| do3_send : 
  forall a t,
  termLTS3 (tact (send a) t) (Some a) t
(* 1 *)
| do3_recv : 
  forall a t,
  termLTS3 (tact (recv a) t) (Some a) t
(* 2 *)
| do3_handshake : 
  forall a l r, 
  termLTS3 (tact (send a) l) (Some a) l ->
  termLTS3 (tact (recv a) r) (Some a) r ->
  termLTS3 (tpar (tact (send a) l) (tact (recv a) r)) (Some a) (tpar l r)
(* 3 *)
| do3_reorder : 
  forall a b l r, 
  termLTS3 (tact (send a) l) (Some a) l ->
  termLTS3 (tact (send b) (tact (recv a) r)) (Some b) (tact (recv a) r) ->
  termLTS3 (tact (recv a) r) (Some a) r ->
  termLTS3 (tpar (tact (send a) l) (tact (send b) (tact (recv a) r))) 
            None 
            (tpar l (tact (send b) r))
(* 4 *)
| do3_seq : 
  forall t t' s a, termLTS3 t a t' -> termLTS3 (tseq t s) a (tseq t' s)
(* 5 *)
| do3_seq_end : forall s, termLTS3 (tseq tend s) None s
(* 6 *)
| do3_par_end : termLTS3 (tpar tend tend) None tend
(* 7 *)
(* These below capture "structural congruence": using "silent" transitions *)
| do3_fix : 
  (* forall t t', t' = tsubst (tfix t) t -> termLTS3 (tfix t) None t' *)
  forall t , termLTS3 (tfix t) None (tsubst (tfix t) t)
(* 8 *)
| do3_comm : 
  forall tl tr, termLTS3 (tpar tl tr) None (tpar tr tl)

(* | do_assocl : forall t1 t2 t3,  *)
    (* termLTS (tpar t1 (tpar t2 t3)) None (tpar (tpar t1 t2) t3) *)

(* | do_assocr : forall t1 t2 t3,  *)
    (* termLTS (tpar (tpar t1 t2) t3) None (tpar t1 (tpar t2 t3)) *)
.

(* MeBi LTS (tseq (tpar (tact (send A) (tact (recv B) tend)) (tact (send B) (tact (recv A) tend))) tend) Using termLTS3. *)
(* MeBi LTS (tfix (tseq (tpar (tact (send A) (tact (recv B) tend)) (tact (send B) (tact (recv A) tend))) trec)) Using termLTS3. *)

(* MeBi Set Bound 50.
MeBi LTS (tseq (tseq (tpar (tact (send A) (tact (recv B) tend)) (tact (send B) (tact (recv A) tend))) tend) (tfix (tseq (tpar (tact (send A) (tact (recv B) tend)) (tact (send B) (tact (recv A) tend))) trec))) Using termLTS3. *)
