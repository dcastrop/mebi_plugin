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


(*************************************************************************)
(**** Layer 1 ************************************************************)
(*************************************************************************)

(** [tm1] is a [nat] that can change via increments and decrements. *)
Inductive tm1 : Type :=
  | NUM : nat -> tm1        (* any nat                 *)
  | ADD : tm1 -> tm1        (* add the state increment *)
  | SUB : tm1 -> tm1        (* sub the state decrement *)
  | DEF : nat -> tm1 -> tm1 (* recursive def           *)
  | REC : nat -> tm1        (* recursive call          *)
  .

(** [unfold1 (i, b) t] unfolds all recursive calls within [t] corresponding to [i] with [b]. Note: if another recursive definition is encountered corresponding to [i], then that body is skipped. *)
Fixpoint unfold1 (i:nat) (b:tm1) (t:tm1) : tm1 :=
  match t with
  | NUM n => NUM n
  | ADD t => unfold1 i b t
  | SUB t => unfold1 i b t
  | DEF j d => if Nat.eqb i j then DEF j d else DEF j (unfold1 i b d)
  | REC j => if Nat.eqb i j then DEF i b else REC j
  end.


(** [s1] is some state local to [tm1], specifying the values used when incrementing and decrementing. *)
Definition s1 : Type := (nat * nat). (* (incr, decr) *)
Definition init_s1 : s1 := (0, 0).
Definition get_inc1 (s:s1) : nat := match s with | (inc, _) => inc end.
Definition get_dec1 (s:s1) : nat := match s with | (_, dec) => dec end.


(** [cfg1] is the configuration for level 1 of this system. I.e., the lowest level. *)
Definition cfg1 : Type := (tm1 * s1).

(** [act1] is the kind of action taken by a [cfg1]. *)
Inductive act1 : Type :=
  | SILENT1 : act1
  | DO_ADD1 : nat -> nat -> act1
  | DO_SUB1 : nat -> nat -> act1
  .

(** [lts1] *)
Inductive lts1 :
  cfg1 -> act1 -> cfg1 -> Prop
  :=
  (* add the value of the state incrementor to [n]. *)
  | LTS1_ADDN : forall n s,
    lts1 (ADD (NUM n), s) (DO_ADD1 n (get_inc1 s)) (NUM (n + (get_inc1 s)), s)
  (* advance the body of [ADD _] if not yet [NUM _]. *)
  | LTS1_ADDT : forall a t1 t2 s1 s2,
    lts1 (t1, s1) a (t2, s2) -> lts1 (ADD t1, s1) a (ADD t2, s2)
  (* minus the value of the state decrementor to [n]. *)
  | LTS1_SUBN : forall n s,
    lts1 (SUB (NUM n), s) (DO_SUB1 n (get_dec1 s)) (NUM (n - (get_dec1 s)), s)
  (* advance the body of [SUB _] if not yet [NUM _]. *)
  | LTS1_SUBT : forall a t1 t2 s1 s2,
    lts1 (t1, s1) a (t2, s2) -> lts1 (SUB t1, s1) a (SUB t2, s2)
  (* unfold recursive definition. *)
  | LTS1_DEFN : forall n t s, lts1 (DEF n t, s) SILENT1 (unfold1 n t t, s)
  .

(** [lts1_tc] is the transitive closure over [lts1]. *)
Inductive lts1_tc : cfg1 -> Prop :=
  (* given an [lts1] transition, both the original and result [cfg1] must either have been able to make a step, or not. *)
  | LTS1_TRANS : forall a t1 t2 s1 s2,
    lts1 (t1, s1) a (t2, s2) -> lts1_tc (t2, s2) -> lts1_tc (t1, s1)
  (* no transition via [lts1]. *)
  | LTS1_NONE : forall t s, lts1_tc (t, s)
  .

(********************************)
(**** Tests *********************)
(********************************)




(*************************************************************************)
(**** Layer 2 ************************************************************)
(*************************************************************************)

(** [tm2] controls the values of increment and decrement in the state. *)
Inductive tm2 : Type :=
  | NOOP : tm2                      (* no-op                      *)
  | INCR : tm1 -> tm2        (* add [tm1] to the state increment *)
  | DECR : tm1 -> tm2        (* add [tm1] to the state decrement *)
  | ZERO : tm1 -> tm2 -> tm2 -> tm2 (* select if zero                   *)
  | EVEN : tm1 -> tm2 -> tm2 -> tm2 (* select if even                   *)
  | SEQ2 : tm2 -> tm2 -> tm2        (* sequence                         *)
  | DEF2 : nat -> tm2 -> tm2        (* recursive def                    *)
  | REC2 : nat -> tm2               (* recursive call                   *)
  .

(** [unfold2 (i, b) t] unfolds all recursive calls within [t] corresponding to [i] with [b]. Note: if another recursive definition is encountered corresponding to [i], then that body is skipped. *)
Fixpoint unfold2 (i:nat) (b:tm2) (t:tm2) : tm2 :=
  match t with
  | NOOP => NOOP
  | INCR t => INCR t
  | DECR t => DECR t
  | SEQ2 l r => SEQ2 l (unfold2 i b r)
  | DEF2 j d => if Nat.eqb i j then DEF2 j d else DEF2 j (unfold2 i b d)
  | REC2 j => if Nat.eqb i j then DEF2 i b else REC2 j
  | ZERO n l r => ZERO n (unfold2 i b l) (unfold2 i b r)
  | EVEN n l r => EVEN n (unfold2 i b l) (unfold2 i b r)
  end.

(** [s2] is a [s1] with an accumulator of the result. *)
Definition s2 : Type := (nat * nat * nat). (* (incr, decr, result) *)
Definition init_s2 : s2 := (0, 0, 0).
Definition get_inc2 (s:s2) : nat := match s with | (inc, _, _) => inc end.
Definition get_dec2 (s:s2) : nat := match s with | (_, dec, _) => dec end.
Definition get_acc2 (s:s2) : nat := match s with | (_, _, acc) => acc end.

Definition add_inc2 (n:nat) (s:s2) : s2 :=
  (Nat.add n (get_inc2 s), get_dec2 s, get_acc2 s).

Definition add_dec2 (n:nat) (s:s2) : s2 :=
  (get_inc2 s, Nat.add n (get_dec2 s), get_acc2 s).

Definition add_acc2 (n:nat) (s:s2) : s2 :=
  (get_inc2 s, get_dec2 s, Nat.add n (get_acc2 s)).

Definition s2_to_s1 (s:s2) : s1 :=
  (get_inc2 s, get_dec2 s).

Definition s2_of_s1 (s:s1) (t:s2) : s2 :=
  (get_inc1 s, get_inc1 s, get_acc2 t).

(** [cfg2] is the configuration for level 2 of this system, above [cfg1]. *)
Definition cfg2 : Type := (tm2 * s2).

(** [act2] is the kind of action taken by a [cfg2]. *)
Inductive act2 : Type :=
  | SILENT2 : act2
  | DO_INC2 : nat -> act2
  | DO_DEC2 : nat -> act2
  (* | RESULT2 : nat -> act2 *)
  | SCOPED2 : act1 -> act2
  .

(** *)
Definition branch_even (n:nat) (l:tm2) (r:tm2) : tm2 := if Nat.even n then l else r.
Definition branch_zero (n:nat) (l:tm2) (r:tm2) : tm2 := if Nat.eqb n 0 then l else r.

(** *)
Inductive lts2 :
  cfg2 -> act2 -> cfg2 -> Prop
  :=
  (* increase the value of the state incrementor by one. *)
  | LTS2_INCR : forall n s,
    lts2 (INCR (NUM n), s) SILENT2 (NOOP, add_inc2 n s)
  (* increase the value of the state decrementor by one. *)
  | LTS2_DECR : forall n s,
    lts2 (DECR (NUM n), s) SILENT2 (NOOP, add_dec2 n s)
  (* if zero then lhs else rhs. *)
  | LTS2_ZERO : forall n l r s,
    lts2 (ZERO (NUM n) l r, s) SILENT2 (branch_zero n l r, s)
  (* advance body of zero. *)
  | LTS2_ZERB : forall a t1 t2 l r s1 s2,
    lts1 (t1, s2_to_s1 s2) a (t2, s1) ->
    lts2 (ZERO t1 l r, s2) (SCOPED2 a) (ZERO t2 l r, s2_of_s1 s1 s2)
  (* if even then lhs else rhs. *)
  | LTS2_EVEN : forall n l r s,
    lts2 (EVEN (NUM n) l r, s) SILENT2 (branch_even n l r, s)
  (* advance body of even. *)
  | LTS2_EVEB : forall a t1 t2 l r s1 s2,
    lts1 (t1, s2_to_s1 s2) a (t2, s1) ->
    lts2 (EVEN t1 l r, s2) (SCOPED2 a) (EVEN t2 l r, s2_of_s1 s1 s2)
  (* advance the lhs of [SEQ l r] until just [NUM _] remains. *)
  (* | LTS2_SEQN : forall n r s,
    lts2 (SEQ2 (NUM n) r, s) (RESULT2 n) (r, add_acc2 n s) *)
  (* advance the lhs of [SEQ l r] until just [NUM _] remains. *)
  | LTS2_SEQN : forall r s,
    lts2 (SEQ2 NOOP r, s) SILENT2 (r, s)
  (* advance the lhs of [SEQ l r] until just [NUM _] remains. *)
  | LTS2_SEQL : forall a l1 l2 r s1 s2,
    (* lts1 (l1, s2_to_s1 s2) a (l2, s1) -> *)
    lts2 (SEQ2 l1 r, s2) a (SEQ2 l2 r, s2_of_s1 s1 s2)
  (* unfold recursive definition. *)
  | LTS2_DEFN : forall n t s, lts2 (DEF2 n t, s) SILENT2 (unfold2 n t t, s)
  .

(** [lts2_tc] is the transitive closure over [lts2]. *)
Inductive lts2_tc : cfg2 -> Prop :=
  (* given an [lts2] transition, both the original and result [cfg2] must either have been able to make a step, or not. *)
  | LTS2_TRANS : forall a t1 t2 s1 s2,
    lts2 (t1, s1) a (t2, s2) -> lts2_tc (t2, s2) -> lts2_tc (t1, s1)
  (* no transition via [lts2]. *)
  | LTS2_NONE : forall t s, lts2_tc (t, s)
  .


(********************************)
(**** Tests *********************)
(********************************)

(** [cfg2_a] will loop forever, since 1 is not even. *)
Example cfg2_a : cfg2 := (DEF2 0 (EVEN (NUM 1) NOOP (REC2 0)), init_s2).

Goal lts2_tc cfg2_a.
  eapply LTS2_TRANS.
  eapply LTS2_DEFN.
  simpl.
  eapply LTS2_TRANS.
  eapply LTS2_EVEN.
  unfold branch_even.
  simpl.
  eapply LTS2_TRANS.
  eapply LTS2_DEFN.
  simpl.

  constructor.
Qed.


(** [cfg2_b] will  *)
Example cfg2_b : cfg2 := (
  DEF2 0 (
    ZERO (
      SUB (NUM 1)
    ) NOOP (
      SEQ2 (
        INCR (ADD (NUM 0))
      ) (REC2 0)
    )
  )
, init_s2).

Goal lts2_tc cfg2_a.
  eapply LTS2_TRANS.
  eapply LTS2_DEFN.
  simpl.
  eapply LTS2_TRANS.
  eapply LTS2_EVEN.
  unfold branch_even.
  simpl.
  eapply LTS2_TRANS.
  eapply LTS2_DEFN.
  simpl.

  constructor.
Qed.



MeBi Dump "cfg2_b"  FSM Bounded 500 Of cfg2_b Using lts2 lts1.




(* Example cfg1_a : cfg1 := () *)