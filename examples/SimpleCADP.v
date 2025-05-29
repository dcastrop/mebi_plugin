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

Definition act_equiv (t1:act) (t2:act) : Prop :=
  match t1, t2 with
  | ENTER, ENTER => True
  | LEAVE, LEAVE => True
  | INC_N, INC_N => True
  | DEC_N, DEC_N => True
  | SET_V i, SET_V j => if Nat.eqb i j then True else False
  | DEC_V, DEC_V => True
  | _, _ => False
  end.


Inductive ntm : Type :=
  | N_IS_ZERO : ntm
  | V_IS_ZERO : ntm
  .

Definition ntm_equiv (t1:ntm) (t2:ntm) : Prop :=
  match t1, t2 with
  | N_IS_ZERO, N_IS_ZERO => True
  | V_IS_ZERO, V_IS_ZERO => True
  | _, _ => False
  end.


Inductive tm : Type :=
  | OK  : tm (* no-op *)
  | ACT : act -> tm
  | SEQ : tm -> tm -> tm

  | IF  : ntm -> tm -> tm -> tm

  | DEF : nat -> tm -> tm
  | REC : nat -> tm
  .

Fixpoint tm_equiv (t1:tm) (t2:tm) : Prop :=
  match t1, t2 with
  | OK, OK => True

  | ACT a,
    ACT b => act_equiv a b

  | SEQ l1 r1,
    SEQ l2 r2 => tm_equiv l1 l2 /\ tm_equiv r1 r2

  | IF c1 a1 b1,
    IF c2 a2 b2 => ntm_equiv c1 c2 /\ tm_equiv a1 a2 /\ tm_equiv b1 b2

  | DEF i b1,
    DEF j b2 => if Nat.eqb i j then tm_equiv b1 b2 else False

  | REC i, REC j => if Nat.eqb i j then True else False

  | _, _ => False

  end.


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
  SEQ (
    DEF 0 (
      IF N_IS_ZERO (
        ACT INC_N (* inc n by one, continue to enter *)
      ) (
        REC 0 (* go back to beginning, wait until can enter *)
      )
    )
  ) (
    SEQ (ACT ENTER) (
      SEQ (ACT (SET_V 5)) (
        DEF 1 (
          IF V_IS_ZERO (
            SEQ (ACT LEAVE) (ACT DEC_N) (* leave and dec n by one *)
          ) (
            SEQ (ACT DEC_V) (REC 1) (* dec v by one, loop *)
          )
        )
      )
    )
  ).

Inductive sys : Type :=
  | PRC : (tm * nat * nat) -> sys
  | PAR : sys -> sys -> sys
  .

Fixpoint sys_equiv (t1:sys) (t2:sys) : Prop :=
  match t1, t2 with

  | PAR t1a t1b,
    PAR t2a t2b => (sys_equiv t1a t2a /\ sys_equiv t1b t2b) \/
                   (sys_equiv t1a t2b /\ sys_equiv t1b t2a)

  | PRC t1, PRC t2 =>
    match t1, t2 with
    | (t1, n1, v1),
      (t2, n2, v2) =>
      match Nat.eqb n1 n2, Nat.eqb v1 v2 with
      | true, true => tm_equiv t1 t2
      | _, _ => False
      end
    end

  | _, _ => False

  end.

Inductive lts : sys -> label -> sys -> Prop :=

  | LTS_PRC : forall a t1 t2 n1 n2 v1 v2,
    step (t1, n1, v1) a (t2, n2, v2) ->
    lts (PRC (t1, n1, v1)) a (PRC (t2, n2, v2))

  | LTS_PAR_L : forall a l1 l2 r,
    lts l1 a l2 ->
    lts (PAR l1 r) a (PAR l2 r)

  | LTS_PAR_R : forall a l r1 r2,
    lts r1 a r2 ->
    lts (PAR l r1) a (PAR l r2)

  (* | LTS_OK_L : forall r n v, lts (PAR (PRC (OK, n, v)) r) SILENT (r)

  | LTS_OK_R : forall l n v, lts (PAR l (PRC (OK, n, v))) SILENT (l) *)
  .


Example e1 : tm * nat * nat := (tm1, 0, 0).
(* MeBi Show LTS Bounded 35 Of e1 Using step. *)
(* MeBi Dump "e1" LTS Bounded 35 Of e1 Using step. *)


Example e2 : sys :=
  PAR (PRC (tm1, 0, 0)) (
    PRC (OK, 0, 0)
  ).
(* MeBi Show LTS Bounded 35 Of e2 Using lts step. *)
(* MeBi Dump "e2" LTS Bounded 35 Of e2 Using lts step. *)


Example e3 : sys :=
  PAR (PRC (tm1, 0, 0)) (
    (PRC (tm1, 0, 0))
  ).
(* MeBi Show LTS Bounded 1200 Of e3 Using lts step. *)
MeBi Dump "e3" LTS Bounded 1156 Of e3 Using lts step.


Example e4 : sys :=
  PAR (PRC (tm1, 0, 0)) (
    PAR (PRC (tm1, 0, 0)) (
      (PRC (OK, 0, 0))
    )
  ).
(* MeBi Show LTS Bounded 2040 Of e4 Using lts step. *)
MeBi Dump "e4" LTS Bounded 1156 Of e4 Using lts step.


Example e5 : sys :=
  PAR (PRC (tm1, 0, 0)) (
    PAR (PRC (tm1, 0, 0)) (
      (PRC (tm1, 0, 0))
    )
  ).
(* MeBi Show LTS Bounded 5000 Of e5 Using lts step. *)
MeBi Dump "e5" LTS Bounded 2278 Of e5 Using lts step.


Example e6 : sys :=
  PAR
    (PRC (DEF 0 (SEQ (OK) (REC 0)), 0, 0))
    (PRC (DEF 0 (SEQ (OK) (REC 0)), 0, 0)).
(* MeBi Show LTS Bounded 5 Of e6 Using lts step. *)
MeBi Dump "e6" LTS Bounded 5 Of e6 Using lts step.


Example e7 : sys :=
  PAR
    (PRC (DEF 0 (SEQ (OK) (REC 0)), 0, 0))
    (PAR
      (PRC (DEF 0 (SEQ (OK) (REC 0)), 0, 0))
      (PRC (DEF 0 (SEQ (OK) (REC 0)), 0, 0))).
(* MeBi Show LTS Bounded 10 Of e7 Using lts step. *)
MeBi Dump "e7" LTS Bounded 10 Of e7 Using lts step.


(* MeBi Dump "e2" LTS sys_equiv Bounded 350 Of e2 Using lts step. *)
(* MeBi Dump "e2" FSM sys_equiv Bounded 350 Of e2 Using lts step. *)