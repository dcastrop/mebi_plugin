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

(** [tm1] is a [nat] that can change before it terminates. *)
Inductive tm1 : Type :=
  | END : tm1               (* termination         *)
  | NUM : nat -> tm1        (* any nat             *)
  | INC : tm1 -> tm1        (* increment the state *)
  | DEC : tm1 -> tm1        (* decrement the state *)
  | ADD : tm1 -> tm1        (* add the increment   *)
  | SUB : tm1 -> tm1        (* sub the decrement   *)
  | SEQ : tm1 -> tm1 -> tm1 (* sequence            *)
  | DEF : nat -> tm1 -> tm1 (* recursive def       *)
  | REC : nat -> tm1        (* recursive call      *)
  | PTY : tm1 -> tm1 -> tm1 -> tm1 (* parity (even l, odd r) *)
  .

(** [unfold (i, b) t] unfolds all recursive calls within [t] corresponding to [i] with [b]. Note: if another recursive definition is encountered corresponding to [i], then that body is skipped. *)
Fixpoint unfold (i:nat) (b:tm1) (t:tm1) : tm1 :=
  match t with
  | END => END
  | NUM n => NUM n
  | INC t => unfold i b t
  | DEC t => unfold i b t
  | ADD t => unfold i b t
  | SUB t => unfold i b t
  | SEQ l r => SEQ l (unfold i b r)
  | DEF j d => if Nat.eqb i j then DEF j d else DEF j (unfold i b d)
  | REC j => if Nat.eqb i j then DEF i b else REC j
  | PTY n l r => PTY n (unfold i b l) (unfold i b r)
  end.

(** [parity n l r] returns [l] if [n] is even, else [r]. *)
Definition parity (n:nat) (l:tm1) (r:tm1) : tm1 := if Nat.even n then l else r.


(** [s1] is some state local to [tm1], specifying the values used when incrementing and decrementing. *)
Definition s1 : Type := (nat * nat). (* (incr, decr) *)
Definition init_s1 : s1 := (0, 0).
Definition get_inc (s:s1) : nat := match s with | (inc, _) => inc end.
Definition get_dec (s:s1) : nat := match s with | (_, dec) => dec end.


(** [cfg1] is the configuration for level 1 of this system. I.e., the lowest level. *)
Definition cfg1 : Type := (tm1 * s1).

(** [act1] is the kind of action taken by a [cfg1]. *)
Inductive act1 : Type :=
  | SILENT : act1
  | DO_NUM : nat -> act1
  | DO_ADD : nat -> nat -> act1
  | DO_SUB : nat -> nat -> act1
  .

(** [lts1] *)
Inductive lts1 :
  cfg1 -> act1 -> cfg1 -> Prop
  :=
  (* any [tm1] that reaches [NUM 0] must [END]. *)
  | LTS1_ZERO : forall s, lts1 (NUM 0, s) SILENT (END, s)
  (* advance the lhs of [SEQ l r] until just [NUM _] remains. *)
  | LTS1_SEQN : forall n r s, lts1 (SEQ (NUM n) r, s) (DO_NUM n) (r, s)
  (* advance the lhs of [SEQ l r] until just [NUM _] remains. *)
  | LTS1_SEQL : forall a l1 l2 r s1 s2,
    lts1 (l1, s1) a (l2, s2) -> lts1 (SEQ l1 r, s1) a (SEQ l2 r, s2)
  (* increase the value of the state incrementor by one. *)
  | LTS1_INCR : forall t s,
    lts1 (INC t, s) SILENT (t, (S (get_inc s), get_dec s))
  (* increase the value of the state decrementor by one. *)
  | LTS1_DECR : forall t s,
    lts1 (DEC t, s) SILENT (t, (S (get_inc s), get_dec s))
  (* add the value of the state incrementor to [n]. *)
  | LTS1_ADDN : forall n s,
    lts1 (ADD (NUM n), s) (DO_ADD n (get_inc s)) (NUM (n + (get_inc s)), s)
  (* advance the body of [ADD _] if not yet [NUM _]. *)
  | LTS1_ADDT : forall a t1 t2 s1 s2,
    lts1 (t1, s1) a (t2, s2) -> lts1 (ADD t1, s1) a (ADD t2, s2)
  (* minus the value of the state decrementor to [n]. *)
  | LTS1_SUBN : forall n s,
    lts1 (SUB (NUM n), s) (DO_SUB n (get_dec s)) (NUM (n - (get_dec s)), s)
  (* advance the body of [SUB _] if not yet [NUM _]. *)
  | LTS1_SUBT : forall a t1 t2 s1 s2,
    lts1 (t1, s1) a (t2, s2) -> lts1 (SUB t1, s1) a (SUB t2, s2)
  (* unfold recursive definition. *)
  | LTS1_DEFN : forall n t s, lts1 (DEF n t, s) SILENT (unfold n t t, s)
  (* parity, if even then lhs else rhs. *)
  | LTS1_PTYN : forall n l r s,
    lts1 (PTY (NUM n) l r, s) SILENT (parity n l r, s)
  (* advance body of parity. *)
  | LTS1_PTYB : forall a t1 t2 l r s1 s2,
    lts1 (t1, s1) a (t2, s2) -> lts1 (PTY t1 l r, s1) a (PTY t2 l r, s2)
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

(** [cfg1_a] will loop forever, since 1 is not even. *)
Example cfg1_a : cfg1 := (DEF 0 (PTY (NUM 1) END (REC 0)), init_s1).

Goal lts1_tc cfg1_a.
  eapply LTS1_TRANS.
  eapply LTS1_DEFN.
  simpl.
  eapply LTS1_TRANS.
  eapply LTS1_PTYN.
  unfold parity.
  simpl.

  constructor.
Qed.



(* Example cfg1_a : cfg1 := () *)