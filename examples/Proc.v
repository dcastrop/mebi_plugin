Require Import MEBI.loader.

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

Module Flat.
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

  Module Simple.
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
  End Simple.

  Module Complex.
    Inductive termLTS : term -> option label -> term -> Prop :=

    | do_handshake : 
      forall a l r, 
      termLTS (tpar (tact (send a) l) (tact (recv a) r)) (Some a) (tpar l r)

    | do_seq : 
      forall t t' s a, termLTS t a t' -> termLTS (tseq t s) a (tseq t' s)

    | do_seq_end : forall s, termLTS (tseq tend s) None s

    | do_parl : 
      forall l1 l2 r a, termLTS l1 a l2 -> termLTS (tpar l1 r) a (tpar l2 r)

    | do_parr : 
      forall l r1 r2 a, termLTS r1 a r2 -> termLTS (tpar l r1) a (tpar l r2)

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
    | termLTS_ts : forall t t' a, termLTS t a t' -> termLTS_tc t' -> termLTS_tc t
    | termLTS_ns : forall t, termLTS_tc t
    .
  End Complex.
End Flat.

Module Layered.
  Inductive term : Set :=
  | trec : term
  | tend : term
  | tfix : term -> term
  | tact : action -> term -> term
  | tseq : term -> term -> term 
  .

  Fixpoint term_eq (a b:term) : bool :=
    match a, b with
    | trec, trec => true
    | tend, tend => true
    | tfix a, tfix b => term_eq a b
    | tact a ac, tact b bc => action_eq a b && term_eq ac bc
    | tseq al ar, tseq bl br => term_eq al bl && term_eq ar br
    | _, _ => false
    end.

  Fixpoint tsubst (t1 : term) (t2 : term) :=
    match t2 with
    | trec => t1
    | tend => tend
    | tfix t => tfix t
    | tact a t => tact a (tsubst t1 t)
    | tseq t s => tseq t (tsubst t1 s)
    end.

  Inductive termLTS : term -> option label -> term -> Prop :=

  | do_send :
    forall t a, termLTS (tact (send a) t) (Some a) t

  | do_recv :
    forall t a, termLTS (tact (recv a) t) (Some a) t

  | do_seq : 
    forall t t' s a, termLTS t a t' -> termLTS (tseq t s) a (tseq t' s)

  | do_seq_end : forall s, termLTS (tseq tend s) None s

  (* These below capture "structural congruence": using "silent" transitions *)
  | do_fix : 
    (* forall t t', t' = tsubst (tfix t) t -> termLTS (tfix t) None t' *)
    forall t , termLTS (tfix t) None (tsubst (tfix t) t)
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

  | do_t : forall t t' a, termLTS t a t' -> compLTS (cprc t) a (cprc t')

  | do_parl : 
    forall l l' r a, compLTS l a l' -> compLTS (cpar l r) a (cpar l' r)

  | do_parr : 
    forall l r r' a, compLTS r a r' -> compLTS (cpar l r) a (cpar l r')

  | do_par_end : compLTS (cpar (cprc tend) (cprc tend)) None (cprc tend)

  | do_comm : forall l r, compLTS (cpar l r) None (cpar r l)

  | do_assocl : 
    forall x y z, compLTS (cpar x (cpar y z)) None (cpar (cpar x y) z)

  | do_assocr : 
    forall x y z, compLTS (cpar (cpar x y) z) None (cpar x (cpar y z))
  .

  Inductive compLTS_tc : comp -> Prop :=
  | compLTS_ts : forall t a t', compLTS t a t' -> compLTS_tc t' -> compLTS_tc t
  | compLTS_ns : forall t, compLTS_tc t
  .
End Layered.