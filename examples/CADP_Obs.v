(* The glued CADP semantics with joining the queue made visible
   (2026-10-11), for stating FIFO order against [Properties/Fifo.v].

   [bigstep_obs] is [CADP_Glued.bigstep] rule for rule, except that
   [DO_ACQUIRE] -- one atomic step that swaps the process into the lock's
   tail and links it behind its predecessor -- is labelled
   [(FETCH_AND_STORE, pid)] rather than silent. From that point the process
   is in the queue. [bigstep] and every proof over it are unchanged.

   [bigstep_obs_norm] is [bigstep_obs] with dead data erased, as in
   [CADP_Norm.v], whose per-block lemmas it reuses. The link between the
   two is [MEBI.Transfer.normalised_bisimilar]'s, as in [CADP_Norm.v]. *)

From Stdlib Require Import List Lia.
Import ListNotations.
Require Import MEBI.Bisimilarity.
Require Import MEBI.Transfer.
Require Import MEBI.Examples.CADP.
Require Import MEBI.Examples.CADP_Glued.
Require Import MEBI.Examples.CADP_Norm.

Import Protocol.

Inductive bigstep_obs : composition -> option label -> composition -> Prop :=
| OBS_MAIN_LOOP : forall b s r,
  bigstep_obs (PRC (REC_DEF PMainLoopDef b, s), r) None (do_main_loop b s r)
| OBS_ACQUIRE : forall c s r,
  bigstep_obs (PRC (SEQ Acquire c, s), r) (Some (FETCH_AND_STORE, get_pid s))
    (do_acquire c s r)
| OBS_RELEASE : forall c s r,
  bigstep_obs (PRC (SEQ Release c, s), r) None (do_release c s r)
| OBS_ACQUIRE_INNER : forall c s r,
  bigstep_obs (PRC (SEQ AcquireInnerLoop c, s), r) None (do_acquire_inner c s r)
| OBS_RELEASE_INNER : forall c s r,
  bigstep_obs (PRC (SEQ ReleaseInnerLoop c, s), r) None (do_release_inner c s r)
| OBS_SEQ_ACT_ENTER : forall y s r,
  bigstep_obs (PRC (SEQ (ACT ENTER) y, s), r) (Some (ENTER, get_pid s)) (PRC (y, s), r)
| OBS_SEQ_ACT_LEAVE : forall y s r,
  bigstep_obs (PRC (SEQ (ACT LEAVE) y, s), r) (Some (LEAVE, get_pid s)) (PRC (y, s), r)
| OBS_PAR_L : forall a l1 l2 r gr1 gr2,
  bigstep_obs (l1, gr1) a (l2, gr2) ->
  bigstep_obs (PAR l1 r, gr1) a (PAR l2 r, gr2)
| OBS_PAR_R : forall a l r1 r2 gr1 gr2,
  bigstep_obs (r1, gr1) a (r2, gr2) ->
  bigstep_obs (PAR l r1, gr1) a (PAR l r2, gr2).

Inductive bigstep_obs_norm : composition -> option label -> composition -> Prop :=
| obs_norm_step : forall c a c', bigstep_obs c a c' -> bigstep_obs_norm c a (tidy c').

(******************************************************************************)
(** The [CADP_Norm] argument again, for [bigstep_obs]: it differs from
    [bigstep] only in [OBS_ACQUIRE]'s label, so the per-block lemmas carry
    over as they are. *)

Lemma step_wf_obs : forall c a c', bigstep_obs c a c' -> wf c ->
  wf c' /\ qlen c' = qlen c.
Proof.
  induction 1 as [b s r | c0 s r | c0 s r | c0 s r | c0 s r | y s r | y s r
                 | a l1 l2 r gr1 gr2 T IH | a l r1 r2 gr1 gr2 T IH];
    intros W; try (split; [exact W | reflexivity]).
  - destruct W as [Hr Hp].
    apply do_acquire_wf; [apply Hp; left; reflexivity | exact Hr].
  - destruct W as [Hr Hp].
    apply do_release_wf; [apply Hp; left; reflexivity | exact Hr].
  - destruct W as [Hr Hp].
    apply do_acquire_inner_wf; [apply Hp; left; reflexivity | exact Hr].
  - destruct W as [Hr Hp].
    apply do_release_inner_wf; [apply Hp; left; reflexivity | exact Hr].
  - destruct (IH (wf_par_l _ _ _ W)) as [[Hr Hp] Q].
    destruct W as [_ Hp0].
    split; [split; [exact Hr|] | exact Q].
    intros p I; cbn in I; apply in_app_or in I as [I | I]; [apply Hp, I|].
    change (p < qlen (l2, gr2)); rewrite Q; apply Hp0.
    cbn; apply in_or_app; right; exact I.
  - destruct (IH (wf_par_r _ _ _ W)) as [[Hr Hp] Q].
    destruct W as [_ Hp0].
    split; [split; [exact Hr|] | exact Q].
    intros p I; cbn in I; apply in_app_or in I as [I | I]; [|apply Hp, I].
    change (p < qlen (r2, gr2)); rewrite Q; apply Hp0.
    cbn; apply in_or_app; left; exact I.
Qed.

Lemma step_tidy_obs : forall c a c', bigstep_obs c a c' -> wf c ->
  forall d, tidy d = tidy c -> exists d', bigstep_obs d a d' /\ tidy d' = tidy c'.
Proof.
  induction 1 as [b s r | c0 s r | c0 s r | c0 s r | c0 s r | y s r | y s r
                 | a l1 l2 r gr1 gr2 T IH | a l r1 r2 gr1 gr2 T IH]; intros W.
  - intros [[[t s'] | xl xr] g] E; cbn [tidy tidy_sys fst snd] in E;
      [|discriminate]; injection E as Et Es Er; subst t.
    eexists; split; [constructor|].
    rewrite (do_main_loop_tidy b s' g), (do_main_loop_tidy b s r), Es, Er.
    reflexivity.
  - (* the labelled block: [d]'s pid is [c]'s *)
    intros [[[t s'] | xl xr] g] E; cbn [tidy tidy_sys fst snd] in E;
      [|discriminate]; injection E as Et Es Er; subst t.
    destruct W as [Hr Hp].
    assert (Hs : get_pid s < List.length (mem_qnodes (fst r)))
      by (apply Hp; left; reflexivity).
    destruct (ok_tidy _ _ _ _ Hs Hr Es Er) as [Hs' Hr'].
    exists (do_acquire c0 s' g); split.
    + rewrite <- (tidy_state_pid _ _ Es); constructor.
    + rewrite (do_acquire_tidy _ _ _ Hs' Hr'), (do_acquire_tidy _ _ _ Hs Hr),
        Es, Er; reflexivity.
  - block_case do_release_tidy.
  - block_case do_acquire_inner_tidy.
  - block_case do_release_inner_tidy.
  - intros [[[t s'] | xl xr] g] E; cbn [tidy tidy_sys fst snd] in E;
      [|discriminate]; injection E as Et Es Er; subst t.
    exists (PRC (y, s'), g); split.
    + rewrite <- (tidy_state_pid _ _ Es); constructor.
    + unfold tidy; cbn [tidy_sys fst snd]; rewrite Es, Er; reflexivity.
  - intros [[[t s'] | xl xr] g] E; cbn [tidy tidy_sys fst snd] in E;
      [|discriminate]; injection E as Et Es Er; subst t.
    exists (PRC (y, s'), g); split.
    + rewrite <- (tidy_state_pid _ _ Es); constructor.
    + unfold tidy; cbn [tidy_sys fst snd]; rewrite Es, Er; reflexivity.
  - intros [[[t s'] | xl xr] g] E; cbn [tidy tidy_sys fst snd] in E;
      [discriminate|]; injection E as El Er Eg.
    destruct (IH (wf_par_l _ _ _ W) (xl, g)) as [[xl' g'] [T' E']].
    { unfold tidy; cbn [fst snd]; rewrite El, Eg; reflexivity. }
    exists (PAR xl' xr, g'); split; [constructor; exact T'|].
    unfold tidy in *; cbn [tidy_sys fst snd] in *.
    injection E' as E1 E2; rewrite E1, E2, Er; reflexivity.
  - intros [[[t s'] | xl xr] g] E; cbn [tidy tidy_sys fst snd] in E;
      [discriminate|]; injection E as El Er Eg.
    destruct (IH (wf_par_r _ _ _ W) (xr, g)) as [[xr' g'] [T' E']].
    { unfold tidy; cbn [fst snd]; rewrite Er, Eg; reflexivity. }
    exists (PAR xl xr', g'); split; [constructor; exact T'|].
    unfold tidy in *; cbn [tidy_sys fst snd] in *.
    injection E' as E1 E2; rewrite E1, E2, El; reflexivity.
Qed.

Lemma norm_spec_obs : forall c a d,
  bigstep_obs_norm c a d <-> exists c', bigstep_obs c a c' /\ d = tidy c'.
Proof.
  split; [intros T; inversion T; subst; eauto | intros [c' [T ->]]; constructor; exact T].
Qed.

Lemma link_obs : forall c d, wf c -> tidy c = tidy d ->
  weak_bisimilar bigstep_obs bigstep_obs_norm c d.
Proof.
  exact (normalised_bisimilar bigstep_obs bigstep_obs_norm tidy wf norm_spec_obs
           tidy_idem wf_tidy (fun c a c' T W => proj1 (step_wf_obs c a c' T W))
           step_tidy_obs).
Qed.

(* What the plugin proves over [bigstep_obs_norm] holds over [bigstep_obs],
   from any well-formed state (every [composition_create] is one:
   [composition_create_wf]). *)
Theorem wbis_transfer_obs {N : Type} (ltsN : LTS N label) : forall c n,
  wf c -> weak_bisimilar bigstep_obs_norm ltsN c n ->
  weak_bisimilar bigstep_obs ltsN c n.
Proof.
  intros c n W.
  apply (transfer_wbis_l bigstep_obs bigstep_obs_norm ltsN c n), link_obs;
    [exact W | reflexivity].
Qed.
