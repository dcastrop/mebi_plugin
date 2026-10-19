(* Moving weak (bi)similarity results between two semantics of one system
   (added 2026-10-11).

   A model often has a faithful semantics, and a smaller one on which the
   plugin can decide and prove things: congruent states identified
   ([examples/ProcCongruence.v]), or dead data erased ([examples/CADP/Norm.v],
   [examples/CADP/Obs.v]). Each example used to prove by hand, with its own
   coinduction, that the two are weakly bisimilar, and then derive transfer
   theorems from it. This file holds the generic parts, so an example only
   discharges its model-specific obligations:

   - [stutter_bisimilar]: a relation under which every step is matched by
     one step with the same label, or, for a silent step, by not moving,
     is contained in [weak_bisimilar];
   - [normalised_bisimilar]: a semantics, and the same semantics with
     every target normalised by a function [f], are weakly bisimilar at
     any two states with the same normal form (given an invariant);
   - [transfer_wsim_l] .. [transfer_wbis_r]: a [weak_sim] or
     [weak_bisimilar] result over one semantics holds over another weakly
     bisimilar to it.

   The plugin cannot explore an LTS with parameters, so nothing here
   defines a relation for it to run on. An example states its normalised
   relation as its own inductive, and links it to these theorems by a
   characterisation lemma (the hypothesis [norm_spec] below), a couple of
   lines by [inversion] and [constructor]. *)

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.

Require Import MEBI.Bisimilarity.

(******************************************************************************)
(** Bisimulations matched step for step, up to silent stuttering *)

Section Stutter.
  Context {M N A : Type} (ltsM : LTS M A) (ltsN : LTS N A).
  Context (R : M -> N -> Prop).

  (* Each step of [ltsM] from a state related to [n] is matched by a step of
     [ltsN] from [n] with the same label, or is silent and stays related to
     [n]. *)
  Hypothesis step_l : forall m n m' a, R m n -> ltsM m a m' ->
    (exists n', ltsN n a n' /\ R m' n') \/ (a = None /\ R m' n).

  (* And the same the other way round. *)
  Hypothesis step_r : forall m n n' a, R m n -> ltsN n a n' ->
    (exists m', ltsM m a m' /\ R m' n') \/ (a = None /\ R m n').

  Theorem stutter_bisimilar : forall m n, R m n -> weak_bisimilar ltsM ltsN m n.
  Proof.
    cofix CH; intros m n H; constructor; constructor.
    - intros m' a T.
      destruct (step_l m n m' a H T) as [[n' [T' H']] | [Ha H']].
      + exists n'; split; [apply inject_weak; exact T' | exact (CH _ _ H')].
      + subst a; exists n; split; [apply wk_none, rt1n_refl | exact (CH _ _ H')].
    - intros n' a T.
      destruct (step_r m n n' a H T) as [[m' [T' H']] | [Ha H']].
      + exists m'; split; [apply inject_weak; exact T' | exact (CH _ _ H')].
      + subst a; exists m; split; [apply wk_none, rt1n_refl | exact (CH _ _ H')].
  Qed.
End Stutter.

(******************************************************************************)
(** A semantics against itself with every target normalised *)

(* [eq_sym] with a universe of its own. [eq_sym]'s type mentions [eq]'s
   template universe, so using it in the sections below would constrain that
   universe globally (Rocq's [bad-template-constraint] warning). *)
Lemma eq_flip {M : Type} {x y : M} : x = y -> y = x.
Proof. intros E; destruct E; reflexivity. Qed.

Section Normalise.
  Context {M A : Type} (lts norm : LTS M A) (f : M -> M) (W : M -> Prop).

  (* [norm] is [lts] with every target normalised by [f]. *)
  Hypothesis norm_spec : forall c a d,
    norm c a d <-> exists c', lts c a c' /\ d = f c'.

  (* Normalising is idempotent. *)
  Hypothesis f_idem : forall c, f (f c) = f c.

  (* The invariant [W] holds of every state with the normal form of one it
     holds of, and is kept by every step. *)
  Hypothesis W_f : forall c d, f c = f d -> W c -> W d.
  Hypothesis step_W : forall c a c', lts c a c' -> W c -> W c'.

  (* States with the same normal form step alike, up to normal form: the
     model-specific obligation. *)
  Hypothesis step_f : forall c a c', lts c a c' -> W c ->
    forall d, f d = f c -> exists d', lts d a d' /\ f d' = f c'.

  Theorem normalised_bisimilar : forall c d,
    W c -> f c = f d -> weak_bisimilar lts norm c d.
  Proof.
    intros c d Wc E.
    refine (stutter_bisimilar lts norm (fun c d => W c /\ f c = f d) _ _
              c d (conj Wc E)); clear c d Wc E.
    - intros c d c' a [Wc E] T; left.
      destruct (step_f c a c' T Wc d (eq_flip E)) as [d' [T' E']].
      exists (f d'); split.
      + apply (proj2 (norm_spec d a (f d'))); exists d'; split; [exact T' | reflexivity].
      + split; [exact (step_W c a c' T Wc) | rewrite f_idem; exact (eq_flip E')].
    - intros c d e a [Wc E] T; left.
      destruct (proj1 (norm_spec d a e) T) as [d' [T' Ee]]; subst e.
      destruct (step_f d a d' T' (W_f c d E Wc) c E) as [c' [Tc Ec]].
      exists c'; split; [exact Tc|].
      split; [exact (step_W c a c' Tc Wc) | rewrite f_idem; exact Ec].
  Qed.
End Normalise.

(******************************************************************************)
(** Transfer: results over a stand-in semantics hold over the original *)

(* [lts'] stands in for [lts] at [c] when [weak_bisimilar lts lts' c c], as
   [normalised_bisimilar] gives for a normalised semantics. The [_l]
   theorems move a result with [c] on the left, the [_r] ones with [c] on
   the right; applying one of each moves a result about two states of
   [lts']. *)
Section Transfer.
  Context {M N A : Type} (lts lts' : LTS M A) (ltsN : LTS N A).

  Theorem transfer_wsim_l : forall c n,
    weak_bisimilar lts lts' c c -> weak_sim lts' ltsN c n -> weak_sim lts ltsN c n.
  Proof.
    intros c n L H.
    apply (weak_sim_trans lts lts' ltsN c c n); [apply weak_bisimilar_sim, L | exact H].
  Qed.

  Theorem transfer_wsim_r : forall n c,
    weak_bisimilar lts lts' c c -> weak_sim ltsN lts' n c -> weak_sim ltsN lts n c.
  Proof.
    intros n c L H.
    apply (weak_sim_trans ltsN lts' lts n c c H).
    apply weak_bisimilar_sim, weak_bisimilar_sym, L.
  Qed.

  Theorem transfer_wbis_l : forall c n,
    weak_bisimilar lts lts' c c -> weak_bisimilar lts' ltsN c n ->
    weak_bisimilar lts ltsN c n.
  Proof.
    intros c n L H; exact (weak_bisimilar_trans lts lts' ltsN c c n L H).
  Qed.

  Theorem transfer_wbis_r : forall n c,
    weak_bisimilar lts lts' c c -> weak_bisimilar ltsN lts' n c ->
    weak_bisimilar ltsN lts n c.
  Proof.
    intros n c L H.
    exact (weak_bisimilar_trans ltsN lts' lts n c c H (weak_bisimilar_sym _ _ L)).
  Qed.
End Transfer.
