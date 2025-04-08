Require Import MEBI.loader.

Require Export String.
Require Import PeanoNat.
Require Import Notations.
Require Export Bool.
Require Import List.
Import ListNotations.

Fixpoint app {X:Type} (l1 l2 : list X) : list X :=
  match l1 with
  | nil    => l2
  | h :: t => h :: (app t l2)
  end.



Inductive act : Type :=
  | ENTER : act
  | LEAVE : act
  | INC_N : act
  | DEC_N : act
  | SET_V : nat -> act
  | DEC_V : act
  .


Inductive ntm : Type :=
  | N_IS_ZERO : ntm
  | V_IS_ZERO : ntm
  .


Inductive tm : Type :=
  | OK  : tm (* no-op *)
  | ACT : act -> tm
  | SEQ : tm -> tm -> tm

  | IF  : ntm -> tm -> tm -> tm

  | DEF : nat -> tm -> tm
  | REC : nat -> tm
  .

Fixpoint unfold (i:nat) (b:tm) (t:tm) : tm :=
  match t with
  | OK  => OK
  | ACT a => ACT a
  | SEQ l r => SEQ l (unfold i b r)

  | IF c t1 t2 => IF c (unfold i b t1) (unfold i b t2)

  | DEF j d => if Nat.eqb i j then DEF j d else DEF j (unfold i b d)
  | REC j => if Nat.eqb i j then DEF i b else REC j
  end.

Inductive label : Type :=
  | SILENT : label
  | LABEL  : act -> label
  .

Definition get_label (a:act) : label :=
  match a with
  | ENTER => LABEL ENTER
  | LEAVE => LABEL LEAVE
  | _ => SILENT
  end.

Definition do_act (a:act) (n:nat) (v:nat) : tm * nat * nat :=
  match a with
  | ENTER => (OK, n, v)
  | LEAVE => (OK, n, v)
  | INC_N => (OK, S n, v)
  | DEC_N => (OK, Nat.sub n 1, v)
  | SET_V v => (OK, n, v)
  | DEC_V => (OK, n, Nat.sub v 1)
  end.

Definition do_if (c:ntm) (t1:tm) (t2:tm) (n:nat) (v:nat) : tm * nat * nat :=
  match c with
  | N_IS_ZERO => (if Nat.eqb n 0 then t1 else t2, n, v)
  | V_IS_ZERO => (if Nat.eqb v 0 then t1 else t2, n, v)
  end.


Inductive step :
  tm * nat * nat -> label -> tm * nat * nat -> Prop
  :=
  | STEP_ACT : forall a n v, step (ACT a, n, v) (get_label a) (do_act a n v)

  | STEP_DEF : forall i t n v,
    step (DEF i t, n, v) SILENT (unfold i t t, n, v)

  | STEP_IFF : forall c t1 t2 n v,
    step (IF c t1 t2, n, v) SILENT (do_if c t1 t2 n v)

  | STEP_SOK : forall t n v, step (SEQ OK t, n, v) SILENT (t, n, v)
  | STEP_SEQ : forall a r l1 l2 n1 n2 v1 v2,
    step (l1, n1, v1) a (l2, n2, v2) ->
    step (SEQ l1 r, n1, v1) a (SEQ l2 r, n2, v2)
  .

(** [tm1] will keep waiting until [n] in state is 0, *)
Example tm1 : tm :=
  DEF 0 (
    SEQ (
      IF N_IS_ZERO (
        ACT INC_N (* inc n by one, continue to enter *)
      ) (
        DEF 1 (
          IF N_IS_ZERO OK (REC 1)
        )
      )
    ) (
      SEQ (ACT ENTER) (
        SEQ (ACT (SET_V 5)) (
          DEF 2 (
            IF V_IS_ZERO (
              SEQ (ACT LEAVE) (SEQ (ACT DEC_N) OK)
            ) (
              SEQ (ACT DEC_V) (REC 2)
            )
          )
        )
      )
    )
  ).

Example e1 : tm * nat * nat := (tm1, 0, 0).

MeBi Dump "e1" FSM Bounded 150 Of e1 Using step.

Inductive sys : Type :=
  | PRC : tm -> nat -> nat -> sys
  | PAR : sys -> sys -> sys
  .


Inductive lts : sys -> label -> sys -> Prop :=

  | LTS_PRC : forall a t1 t2 n1 n2 v1 v2,
    step (t1, n1, v1) a (t2, n2, v2) ->
    lts (PRC t1 n1 v1) a (PRC t2 n2 v2)

  | LTS_PAR_L : forall a l1 l2 r,
    lts l1 a l2 ->
    lts (PAR l1 r) a (PAR l2 r)

  | LTS_PAR_R : forall a l r1 r2,
    lts r1 a r2 ->
    lts (PAR l r1) a (PAR l r2)

  | LTS_OK_L : forall r n v, lts (PAR (PRC OK n v) r) SILENT (r)

  | LTS_OK_R : forall l n v, lts (PAR l (PRC OK n v)) SILENT (l)
  .

Example e2 : sys :=
  PAR (PRC tm1 0 0) (
    (* (PRC tm1 0 0) *)
    PRC OK 0 0
  ).

MeBi Dump "e2" FSM Bounded 350 Of e2 Using lts step.
