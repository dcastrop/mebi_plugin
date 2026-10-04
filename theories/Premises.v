(* Lemmas the plugin uses to prove constructor premises that its bounded
   proof search cannot prove by applying constructors alone.

   Bounded universals ([forall k, k < n -> P k], [forall k, k <= n -> P k],
   with [n] a numeral once normalized): the search decides each [P i] in
   turn, and the proof of the universal is built from those proofs, one
   lemma application per value of [k]. Added 2026-10-04 (notes/14, item 1).

   [k < n] is [S k <= n] by definition, so both forms are over Peano's [le];
   the [_lt_] lemmas peel the [S] off [n] once and hand over to the [_le_]
   ones. *)

Lemma bounded_le_0 (P : nat -> Prop) : P 0 -> forall k, k <= 0 -> P k.
Proof. intros H k Hk; inversion Hk; exact H. Qed.

Lemma bounded_le_S (P : nat -> Prop) (m : nat) :
  (forall k, k <= m -> P k) -> P (S m) -> forall k, k <= S m -> P k.
Proof.
  intros H Hs k Hk; inversion Hk as [E | m' Hk' E].
  - exact Hs.
  - exact (H k Hk').
Qed.

Lemma bounded_lt_0 (P : nat -> Prop) : forall k, S k <= 0 -> P k.
Proof. intros k Hk; inversion Hk. Qed.

Lemma bounded_lt_S (P : nat -> Prop) (m : nat) :
  (forall k, k <= m -> P k) -> forall k, S k <= S m -> P k.
Proof. intros H k Hk; exact (H k (le_S_n k m Hk)). Qed.
