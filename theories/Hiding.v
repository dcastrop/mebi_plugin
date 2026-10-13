(* Hiding and relabelling (added 2026-10-11).

   [relabelled h lts lts'] says [lts'] is [lts] with every label [b]
   replaced by [h b]. When [h] keeps silent steps silent ([h None = None]),
   relabelling preserves weak steps, so it preserves [weak_sim] and
   [weak_bisimilar]: two systems related before relabelling still are after
   ([relabel_sim], [relabel_bisimilar]). Hiding is the case where [h] maps
   some visible labels to [None].

   The use it was written for: [examples/CADP/Obs.v] makes joining the
   lock's queue visible, to state FIFO order. Plain [bigstep] is that
   semantics with joining hidden, so a result about the observing
   semantics gives one about [bigstep]
   ([examples/CADP/Hidden.v]).

   As in [MEBI.Transfer], the plugin cannot explore a relation with
   parameters, so nothing here defines a relabelled LTS. An example states
   its relabelled relation as its own inductive, and proves it
   [relabelled] by induction. *)

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.

Require Import MEBI.Bisimilarity.

(* [relabelled h lts lts'] holds when the steps of [lts'] are those of
   [lts], each label [b] replaced by [h b]. *)
Definition relabelled {M A : Type} (h : option A -> option A) (lts lts' : LTS M A)
  : Prop :=
  forall c a d, lts' c a d <-> exists b, lts c b d /\ a = h b.

(* A silent path of [lts] is one of [lts'], when [h] keeps silence. *)
Lemma relabel_silent {M A : Type} {h : option A -> option A} {lts lts' : LTS M A} :
  h None = None -> relabelled h lts lts' ->
  forall x y, silent lts x y -> silent lts' x y.
Proof.
  intros H0 R x y S; induction S as [x | x z y T S IH].
  - apply rt1n_refl.
  - apply (rt1n_trans _ _ x z y); [|exact IH].
    apply (proj2 (R x None z)); exists None; split; [exact T|].
    (* [eq_sym H0], without [eq_sym]: see [MEBI.Transfer.eq_flip]. *)
    exact (match H0 in _ = y return y = h None with eq_refl => eq_refl end).
Qed.

(* A weak [b] step of [lts] is a weak [h b] step of [lts']. A visible step
   that [h] hides joins the silent paths around it. *)
Lemma relabel_weak {M A : Type} {h : option A -> option A} {lts lts' : LTS M A} :
  h None = None -> relabelled h lts lts' ->
  forall x y b, weak lts x y b -> weak lts' x y (h b).
Proof.
  intros H0 R x y b W; destruct W as [a z t S1 T S2 | S].
  - assert (T' : lts' z (h (Some a)) t)
      by (apply (proj2 (R z (h (Some a)) t)); exists (Some a); split; [exact T | reflexivity]).
    pose proof (relabel_silent H0 R x z S1) as S1'.
    pose proof (relabel_silent H0 R t y S2) as S2'.
    destruct (h (Some a)) as [a'|] eqn:E.
    + exact (wk_some lts' x y a' z t S1' T' S2').
    + apply wk_none.
      exact (clos_rt_trans S1' (rt1n_trans _ _ z t y T' S2')).
  - rewrite H0; apply wk_none, (relabel_silent H0 R x y S).
Qed.

(* Relabelling both sides by [h] preserves weak similarity. *)
Theorem relabel_sim {M N A : Type} (h : option A -> option A)
  (ltsM ltsM' : LTS M A) (ltsN ltsN' : LTS N A) :
  h None = None -> relabelled h ltsM ltsM' -> relabelled h ltsN ltsN' ->
  forall m n, weak_sim ltsM ltsN m n -> weak_sim ltsM' ltsN' m n.
Proof.
  intros H0 RM RN; cofix CH; intros m n H; constructor; constructor.
  intros m' a T.
  destruct (proj1 (RM m a m') T) as [b [Tb Ea]]; subst a.
  destruct (sim_weak (out_sim H) Tb) as [n' [W H']].
  exists n'; split; [exact (relabel_weak H0 RN n n' b W) | exact (CH _ _ H')].
Qed.

(* Relabelling both sides by [h] preserves weak bisimilarity. *)
Theorem relabel_bisimilar {M N A : Type} (h : option A -> option A)
  (ltsM ltsM' : LTS M A) (ltsN ltsN' : LTS N A) :
  h None = None -> relabelled h ltsM ltsM' -> relabelled h ltsN ltsN' ->
  forall m n, weak_bisimilar ltsM ltsN m n -> weak_bisimilar ltsM' ltsN' m n.
Proof.
  intros H0 RM RN; cofix CH; intros m n H; constructor; constructor.
  - intros m' a T.
    destruct (proj1 (RM m a m') T) as [b [Tb Ea]]; subst a.
    destruct (bisim_l (out_bisim H) Tb) as [n' [W H']].
    exists n'; split; [exact (relabel_weak H0 RN n n' b W) | exact (CH _ _ H')].
  - intros n' a T.
    destruct (proj1 (RN n a n') T) as [b [Tb Ea]]; subst a.
    destruct (bisim_r (out_bisim H) Tb) as [m' [W H']].
    exists m'; split; [exact (relabel_weak H0 RM m m' b W) | exact (CH _ _ H')].
Qed.
