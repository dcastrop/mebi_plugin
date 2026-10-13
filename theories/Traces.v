(* What carries over a weak simulation or bisimilarity: traces, and what
   can happen next (added 2026-10-13).

   A weak simulation or bisimilarity between two systems is a statement
   about the systems. The properties a user asks about are statements
   about their behaviour: which sequences of visible actions they can do
   (their weak traces, [weak_trace]), and which visible action each can do
   next once silent steps are ignored ([weak_can]). This file says which
   of them each relation preserves, so a result proved for one semantics
   (glued, normalised) can be read off for another (layered).

   - [weak_sim m n]: every weak trace of [m] is one of [n]
     ([weak_sim_traces]). So a safety property, one every trace of [n]
     satisfies, holds of [m] ([weak_sim_safe]).
   - [mutual_sim m n]: the same weak traces ([mutual_sim_traces]).
   - [weak_bisimilar m n]: the same weak traces too, and more. After any
     trace, each state [m] reaches is matched by one [n] reaches with the
     same next actions ([weak_bisimilar_ready]), so in particular the same
     deadlocks ([weak_bisimilar_deadlock]). Mutual similarity does not give
     this: [a.b + a] and [a.b] have the same traces, but only the first can
     be stuck after [a] (Test.v, [WeakBisimilarVsMutualSim]).

   What does NOT carry over: divergence (weak bisimilarity ignores silent
   loops, so "eventually" properties are not preserved; [examples/README.md],
   "No starvation"), internal states and labels hidden as silent, and the
   number of steps taken.

   As for [MEBI.Transfer] and [MEBI.Hiding], these are generic theorems.
   An example links them to its own property by a characterisation lemma:
   [examples/CADP/SpecMutex.v]'s [spec_lts_mutex] says every trace of the
   lock server is mutually exclusive, and [examples/CADP/LayeredTransfer.v]
   carries that to the layered model. *)

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Import List.
Import ListNotations.

Require Import MEBI.Bisimilarity.

Section Traces.
  Context {M A : Type} (lts : LTS M A).

  (* [weak_path x t y]: from [x], [lts] can do the visible actions [t] in
     order, with any silent steps before, between and after, ending in
     [y]. *)
  Inductive weak_path : M -> list A -> M -> Prop :=
  | wp_nil : forall x y, silent lts x y -> weak_path x [] y
  | wp_cons : forall x y z a t,
      weak lts x y (Some a) -> weak_path y t z -> weak_path x (a :: t) z.

  (* [t] is a weak trace of [x]. *)
  Definition weak_trace (x : M) (t : list A) : Prop := exists y, weak_path x t y.

  (* [x] can do [a] next, after silent steps. *)
  Definition weak_can (x : M) (a : A) : Prop := exists y, weak lts x y (Some a).
End Traces.
Arguments wp_nil {M A lts x y} _.
Arguments wp_cons {M A lts x y z a t} _ _.

(* A weak simulation answers a weak path with one on the same trace,
   ending in a pair it still relates. *)
Lemma weak_sim_path {M N A} {ltsM : LTS M A} {ltsN : LTS N A} :
  forall m t m', weak_path ltsM m t m' ->
  forall n, weak_sim ltsM ltsN m n ->
  exists n', weak_path ltsN n t n' /\ weak_sim ltsM ltsN m' n'.
Proof.
  intros m t m' P; induction P as [x y S | x y z a t W P IH]; intros n H.
  - destruct (weak_sim_silent_clos H S) as [n' [S' H']].
    exists n'; split; [exact (wp_nil S') | exact H'].
  - destruct (weak_sim_act_clos H W) as [n2 [W' H2]].
    destruct (IH n2 H2) as [n' [P' H']].
    exists n'; split; [exact (wp_cons W' P') | exact H'].
Qed.

Lemma weak_sim_traces {M N A} {ltsM : LTS M A} {ltsN : LTS N A} :
  forall m n, weak_sim ltsM ltsN m n ->
  forall t, weak_trace ltsM m t -> weak_trace ltsN n t.
Proof.
  intros m n H t [m' P].
  destruct (weak_sim_path m t m' P n H) as [n' [P' _]]; exists n'; exact P'.
Qed.

(* A safety property, one that every trace of [n] has, holds of [m]. *)
Lemma weak_sim_safe {M N A} {ltsM : LTS M A} {ltsN : LTS N A}
  (P : list A -> Prop) :
  forall m n, weak_sim ltsM ltsN m n ->
  (forall t, weak_trace ltsN n t -> P t) ->
  forall t, weak_trace ltsM m t -> P t.
Proof. intros m n H Safe t T; exact (Safe t (weak_sim_traces m n H t T)). Qed.

Lemma mutual_sim_traces {M N A} {ltsM : LTS M A} {ltsN : LTS N A} :
  forall m n, mutual_sim ltsM ltsN m n ->
  forall t, weak_trace ltsM m t <-> weak_trace ltsN n t.
Proof.
  intros m n [H1 H2] t; split;
    [exact (weak_sim_traces m n H1 t) | exact (weak_sim_traces n m H2 t)].
Qed.

Lemma weak_bisimilar_path {M N A} {ltsM : LTS M A} {ltsN : LTS N A} :
  forall m t m', weak_path ltsM m t m' ->
  forall n, weak_bisimilar ltsM ltsN m n ->
  exists n', weak_path ltsN n t n' /\ weak_bisimilar ltsM ltsN m' n'.
Proof.
  intros m t m' P; induction P as [x y S | x y z a t W P IH]; intros n H.
  - destruct (weak_bisimilar_silent_clos H S) as [n' [S' H']].
    exists n'; split; [exact (wp_nil S') | exact H'].
  - destruct (weak_bisimilar_act_clos H W) as [n2 [W' H2]].
    destruct (IH n2 H2) as [n' [P' H']].
    exists n'; split; [exact (wp_cons W' P') | exact H'].
Qed.

Lemma weak_bisimilar_traces {M N A} {ltsM : LTS M A} {ltsN : LTS N A} :
  forall m n, weak_bisimilar ltsM ltsN m n ->
  forall t, weak_trace ltsM m t <-> weak_trace ltsN n t.
Proof. intros m n H; exact (mutual_sim_traces m n (weak_bisimilar_mutual_sim m n H)). Qed.

(* Bisimilar states can do the same visible actions next. *)
Lemma weak_bisimilar_can {M N A} {ltsM : LTS M A} {ltsN : LTS N A} :
  forall m n, weak_bisimilar ltsM ltsN m n ->
  forall a, weak_can ltsM m a <-> weak_can ltsN n a.
Proof.
  intros m n H a; split; intros [y W].
  - destruct (weak_bisimilar_act_clos H W) as [y' [W' _]]; exists y'; exact W'.
  - destruct (weak_bisimilar_act_clos (weak_bisimilar_sym _ _ H) W)
      as [y' [W' _]]; exists y'; exact W'.
Qed.

(* After any trace, each state [m] reaches is matched by one [n] reaches by
   the same trace, which can do the same visible actions next. (By
   [weak_bisimilar_sym], the same holds from [n]'s side.) *)
Lemma weak_bisimilar_ready {M N A} {ltsM : LTS M A} {ltsN : LTS N A} :
  forall m n, weak_bisimilar ltsM ltsN m n ->
  forall t m', weak_path ltsM m t m' ->
  exists n', weak_path ltsN n t n' /\
             forall a, weak_can ltsM m' a <-> weak_can ltsN n' a.
Proof.
  intros m n H t m' P.
  destruct (weak_bisimilar_path m t m' P n H) as [n' [P' H']].
  exists n'; split; [exact P' | exact (weak_bisimilar_can m' n' H')].
Qed.

(* In particular, a deadlock (a state that can do nothing visible, even
   after silent steps) reached by [m] after [t] is matched by one reached
   by [n] after [t]. *)
Lemma weak_bisimilar_deadlock {M N A} {ltsM : LTS M A} {ltsN : LTS N A} :
  forall m n, weak_bisimilar ltsM ltsN m n ->
  forall t m', weak_path ltsM m t m' -> (forall a, ~ weak_can ltsM m' a) ->
  exists n', weak_path ltsN n t n' /\ forall a, ~ weak_can ltsN n' a.
Proof.
  intros m n H t m' P D.
  destruct (weak_bisimilar_ready m n H t m' P) as [n' [P' C]].
  exists n'; split; [exact P' | intros a Can; exact (D a (proj2 (C a) Can))].
Qed.
