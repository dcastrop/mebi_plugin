(* The glued CADP semantics ([CADP_Glued.bigstep]) with its dead data erased,
   made explicit as a normalised semantics, after the recipe of
   [ProcCongruence.v] and [doc/structural-congruence.md] (2026-10-11).

   Most of [bigstep]'s states differ only in data no step will read:
   - each process's local variables ([predecessor], [locked], [next],
     [swap]). Every glued block ([do_acquire], [do_release], and their
     inner loops) writes a local before it reads it, within the one step,
     so between steps every local is dead;
   - the resource's [mem_next] and [mem_locked], which [read_next] and
     [read_locked] write and nothing reads.

   [tidy] resets all of them, and [bigstep_norm] takes [tidy] of every
   [bigstep] target. [link] proves the two semantics weakly bisimilar on
   states equal up to [tidy], so a [weak_sim] or [weak_bisimilar] the plugin
   proves over [bigstep_norm] transfers to [bigstep] ([wsim_transfer],
   [wsim_transfer_r], [wbis_transfer]), for every size
   ([composition_create_wf]).

   One caveat makes this hold only for well-formed states ([wf]): every
   pid, the lock and every qnode's [next] index a qnode. Outside them a
   block's memory access fails, and the error path goes on to read a stale
   local (e.g. [do_acquire_inner] after a failed [read_locked]). Every
   state reachable from [composition_create] is well formed ([step_wf]).

   Unlike [ProcCongruence.v] there is no congruence to quotient by: CADP's
   [PAR] tree is fixed. What is erased is data, not arrangement. *)

From Stdlib Require Import List Lia.
Import ListNotations.
Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.
Require Import MEBI.Examples.CADP_Glued.

Import Protocol.

(******************************************************************************)
(** Erasing dead data *)

Definition tidy_state (s : state) : state :=
  match s with (p, _, e) => (p, Vars.initial, e) end.

Definition tidy_res (r : resource) : resource :=
  match r with (m, l) => (Build_mem None false (mem_qnodes m), l) end.

Fixpoint tidy_sys (x : sys) : sys :=
  match x with
  | PRC (t, s) => PRC (t, tidy_state s)
  | PAR l r => PAR (tidy_sys l) (tidy_sys r)
  end.

Definition tidy (c : composition) : composition :=
  (tidy_sys (fst c), tidy_res (snd c)).

(******************************************************************************)
(** The normalised semantics: [bigstep] with every target tidied *)

Inductive bigstep_norm : composition -> option label -> composition -> Prop :=
| norm_step : forall c a c', bigstep c a c' -> bigstep_norm c a (tidy c').

(******************************************************************************)
(** Well-formed states: every index names a qnode *)

Definition in_range (n : nat) (i : index) : Prop :=
  match i with None => True | Some j => j < n end.

Definition nexts_in (n : nat) (qs : qnodes) : Prop :=
  forall j q, List.nth_error qs j = Some q -> in_range n (next q).

Definition res_ok (r : resource) : Prop :=
  in_range (List.length (mem_qnodes (fst r))) (snd r)
  /\ nexts_in (List.length (mem_qnodes (fst r))) (mem_qnodes (fst r)).

Fixpoint pids (x : sys) : list pid :=
  match x with PRC (_, s) => [get_pid s] | PAR l r => pids l ++ pids r end.

Definition qlen (c : composition) : nat := List.length (mem_qnodes (fst (snd c))).

Definition wf (c : composition) : Prop :=
  res_ok (snd c) /\ forall p, In p (pids (fst c)) -> p < qlen c.

(******************************************************************************)
(** Lemmas: [tidy] *)

Lemma tidy_state_idem : forall s, tidy_state (tidy_state s) = tidy_state s.
Proof. intros [[p v] e]; reflexivity. Qed.

Lemma tidy_res_idem : forall r, tidy_res (tidy_res r) = tidy_res r.
Proof. intros [[mn ml qs] l]; reflexivity. Qed.

Lemma tidy_sys_idem : forall x, tidy_sys (tidy_sys x) = tidy_sys x.
Proof.
  induction x as [[t s] | l IHl r IHr]; simpl;
    [rewrite tidy_state_idem | rewrite IHl, IHr]; reflexivity.
Qed.

Lemma tidy_idem : forall c, tidy (tidy c) = tidy c.
Proof.
  intros [x r]; unfold tidy; simpl; rewrite tidy_sys_idem, tidy_res_idem.
  reflexivity.
Qed.

Lemma tidy_state_pid : forall s s', tidy_state s = tidy_state s' ->
  get_pid s = get_pid s'.
Proof. intros [[p v] e] [[p' v'] e'] E; inversion E; reflexivity. Qed.

Lemma tidy_res_parts : forall r r', tidy_res r = tidy_res r' ->
  mem_qnodes (fst r) = mem_qnodes (fst r') /\ snd r = snd r'.
Proof. intros [[mn ml qs] l] [[mn' ml' qs'] l'] E; inversion E; auto. Qed.

Lemma pids_tidy : forall x, pids (tidy_sys x) = pids x.
Proof.
  induction x as [[t [[p v] e]] | l IHl r IHr]; simpl;
    [reflexivity | rewrite IHl, IHr; reflexivity].
Qed.

Lemma wf_tidy : forall c d, tidy c = tidy d -> wf c -> wf d.
Proof.
  intros [x r] [y r'] E [[Hl Hn] Hp]; unfold tidy in E; simpl in *.
  inversion E as [[Ex Er]].
  destruct (tidy_res_parts _ _ Er) as [Eq El].
  unfold wf, res_ok, qlen in *; simpl in *.
  rewrite <- Eq, <- El; split; [split; assumption|].
  intros p I; apply Hp.
  rewrite <- pids_tidy, <- Ex, pids_tidy in I; exact I.
Qed.

(******************************************************************************)
(** Lemmas: qnode lists *)

Lemma replace_spec : forall r i qs qs',
  Memory.qnodes_nth_replace r i qs = Some qs' ->
  List.length qs' = List.length qs
  /\ (forall n, nexts_in n qs -> in_range n (next r) -> nexts_in n qs').
Proof.
  intros r i; induction i as [|i IH]; intros [|q qs] qs' H; cbn in H;
    try discriminate.
  - inversion H; subst; split; [reflexivity|].
    intros n N R [|j] q' E; cbn in E; [inversion E; subst; exact R|].
    apply (N (S j)); exact E.
  - destruct (Memory.qnodes_nth_replace r i qs) as [t'|] eqn:E; [|discriminate].
    inversion H; subst.
    destruct (IH _ _ E) as [L N']; split; [cbn; rewrite L; reflexivity|].
    intros n N R [|j] q' E'; cbn in E'.
    + inversion E'; subst; apply (N 0); reflexivity.
    + refine (N' n _ R j q' E').
      intros j' q'' E''; apply (N (S j')); exact E''.
Qed.

Lemma replace_none : forall r i qs,
  Memory.qnodes_nth_replace r i qs = None -> List.length qs <= i.
Proof.
  intros r i; induction i as [|i IH]; intros [|q qs] H; cbn in *;
    try discriminate; try lia.
  destruct (Memory.qnodes_nth_replace r i qs) eqn:E; [discriminate|].
  apply IH in E; lia.
Qed.

(******************************************************************************)
(** Tactics for the per-block lemmas *)

(* Unfold [tidy] and everything a glued block calls, down to the qnode list,
   and reduce. [cbv] with an explicit list never folds a definition back, so
   both sides of a [*_tidy] lemma end up branching on the same terms. *)
Ltac norm_goal :=
  cbv beta iota zeta delta [tidy tidy_sys tidy_state tidy_res fst snd
    do_main_loop do_acquire do_acquire_inner do_release do_release_inner
    return_error' write_next write_locked read_next read_locked
    fetch_and_store compare_and_swap return_error handle_value
    local_var_to_pid_opt local_var_to_index_opt Memory.nth_error
    Memory.nth_replace qnode_set_next qnode_set_locked mem_set_next
    mem_set_locked mem_set_qnodes set_predecessor
    set_locked set_next set_swap resource_set_mem resource_set_lock
    resource_get_mem resource_get_lock get_pid get_vars get_errors
    get_predecessor get_locked get_next get_swap handle_index_value
    get_local_var_value value_to_index_opt value_to_bool_opt set_vars
    add_error get_state get_resource Index.eqb Index.of_pid Vars.initial
    var_predecessor var_locked var_next var_swap mem_next mem_locked
    mem_qnodes next locked eval Nil].

(* Destruct an innermost [match] scrutinee, reduce, and repeat. *)
Ltac destruct_matches :=
  repeat (match goal with
          | |- context [match ?x with _ => _ end] =>
              lazymatch x with
              | context [match _ with _ => _ end] => fail
              | _ => destruct x eqn:?
              end
          end; norm_goal).

(* A failed memory access contradicts the ranges in context. *)
Ltac close_range :=
  subst;
  repeat match goal with
         | H : Memory.qnodes_nth_replace _ _ _ = Some _ |- _ =>
             apply replace_spec in H; destruct H as [? _]
         | H : Memory.qnodes_nth_replace _ _ _ = None |- _ =>
             apply replace_none in H
         | H : List.nth_error _ _ = None |- _ => apply List.nth_error_None in H
         | H : List.nth_error ?qs _ = Some _, N : nexts_in _ ?qs |- _ =>
             apply N in H
         end;
  cbn [in_range next] in *; lia.

(******************************************************************************)
(** Each glued block, run from a tidied state, ends in the same tidied state *)

Lemma do_main_loop_tidy : forall b s r,
  tidy (do_main_loop b s r) = tidy (do_main_loop b (tidy_state s) (tidy_res r)).
Proof. intros b [[p v] e] [[mn ml qs] l]; reflexivity. Qed.

Lemma do_acquire_tidy : forall c s r,
  get_pid s < List.length (mem_qnodes (fst r)) -> res_ok r ->
  tidy (do_acquire c s r) = tidy (do_acquire c (tidy_state s) (tidy_res r)).
Proof.
  intros c [[p v] e] [[mn ml qs] l] Hp [Hl Hn]; cbn in Hp, Hl, Hn.
  norm_goal; destruct_matches; solve [reflexivity | exfalso; close_range].
Qed.

Lemma do_acquire_inner_tidy : forall c s r,
  get_pid s < List.length (mem_qnodes (fst r)) -> res_ok r ->
  tidy (do_acquire_inner c s r)
  = tidy (do_acquire_inner c (tidy_state s) (tidy_res r)).
Proof.
  intros c [[p v] e] [[mn ml qs] l] Hp [Hl Hn]; cbn in Hp, Hl, Hn.
  norm_goal; destruct_matches; solve [reflexivity | exfalso; close_range].
Qed.

Lemma do_release_tidy : forall c s r,
  get_pid s < List.length (mem_qnodes (fst r)) -> res_ok r ->
  tidy (do_release c s r) = tidy (do_release c (tidy_state s) (tidy_res r)).
Proof.
  intros c [[p v] e] [[mn ml qs] l] Hp [Hl Hn]; cbn in Hp, Hl, Hn.
  norm_goal; destruct_matches; solve [reflexivity | exfalso; close_range].
Qed.

Lemma do_release_inner_tidy : forall c s r,
  get_pid s < List.length (mem_qnodes (fst r)) -> res_ok r ->
  tidy (do_release_inner c s r)
  = tidy (do_release_inner c (tidy_state s) (tidy_res r)).
Proof.
  intros c [[p v] e] [[mn ml qs] l] Hp [Hl Hn]; cbn in Hp, Hl, Hn.
  norm_goal; destruct_matches; solve [reflexivity | exfalso; close_range].
Qed.

(******************************************************************************)
(** Each glued block keeps the state well formed, and the qnode count *)

(* Carry the range facts forward: each qnode read from a list whose [next]
   fields are in range has its [next] in range, and each replaced list keeps
   its [next] fields in range when the written qnode's is. *)
Ltac range_facts :=
  repeat match goal with
         | H : List.nth_error ?L _ = Some _, K : nexts_in _ ?L |- _ =>
             apply K in H; cbn [in_range next] in H
         | N : forall n, nexts_in n ?q -> _ -> nexts_in n ?q',
           K : nexts_in ?M ?q |- _ =>
             let K' := fresh "K" in
             assert (K' : nexts_in M q')
               by (apply N; [exact K | cbn [in_range next] in *;
                                       first [exact I | lia | assumption]]);
             clear N
         end.

(* The result of a block is well formed: rewrite every qnode list's length
   back to the original's, then discharge each part. *)
Ltac close_wf :=
  subst;
  repeat match goal with
         | H : Memory.qnodes_nth_replace _ _ _ = Some _ |- _ =>
             apply replace_spec in H; destruct H as [?L ?N]
         end;
  repeat match goal with
         | L : List.length ?a = List.length ?b |- context [List.length ?a] =>
             rewrite L
         end;
  cbn [in_range next pids In qlen fst snd mem_qnodes get_pid] in *;
  range_facts;
  repeat split;
  first [ assumption | exact I | lia | intros ? [<- | []]; lia ].

Lemma do_acquire_wf : forall c s r,
  get_pid s < List.length (mem_qnodes (fst r)) -> res_ok r ->
  wf (do_acquire c s r) /\ qlen (do_acquire c s r) = List.length (mem_qnodes (fst r)).
Proof.
  intros c [[p v] e] [[mn ml qs] l] Hp [Hl Hn]; cbn in Hp, Hl, Hn.
  unfold wf, res_ok, qlen.
  norm_goal; destruct_matches; solve [close_wf | exfalso; close_range].
Qed.

Lemma do_acquire_inner_wf : forall c s r,
  get_pid s < List.length (mem_qnodes (fst r)) -> res_ok r ->
  wf (do_acquire_inner c s r)
  /\ qlen (do_acquire_inner c s r) = List.length (mem_qnodes (fst r)).
Proof.
  intros c [[p v] e] [[mn ml qs] l] Hp [Hl Hn]; cbn in Hp, Hl, Hn.
  unfold wf, res_ok, qlen.
  norm_goal; destruct_matches; solve [close_wf | exfalso; close_range].
Qed.

Lemma do_release_wf : forall c s r,
  get_pid s < List.length (mem_qnodes (fst r)) -> res_ok r ->
  wf (do_release c s r) /\ qlen (do_release c s r) = List.length (mem_qnodes (fst r)).
Proof.
  intros c [[p v] e] [[mn ml qs] l] Hp [Hl Hn]; cbn in Hp, Hl, Hn.
  unfold wf, res_ok, qlen.
  norm_goal; destruct_matches; solve [close_wf | exfalso; close_range].
Qed.

Lemma do_release_inner_wf : forall c s r,
  get_pid s < List.length (mem_qnodes (fst r)) -> res_ok r ->
  wf (do_release_inner c s r)
  /\ qlen (do_release_inner c s r) = List.length (mem_qnodes (fst r)).
Proof.
  intros c [[p v] e] [[mn ml qs] l] Hp [Hl Hn]; cbn in Hp, Hl, Hn.
  unfold wf, res_ok, qlen.
  norm_goal; destruct_matches; solve [close_wf | exfalso; close_range].
Qed.

(******************************************************************************)
(** [bigstep] keeps states well formed *)

Lemma wf_par_l : forall l r g, wf (PAR l r, g) -> wf (l, g).
Proof.
  intros l r g [Hr Hp]; split; [exact Hr|].
  intros p I; apply Hp; cbn; apply in_or_app; left; exact I.
Qed.

Lemma wf_par_r : forall l r g, wf (PAR l r, g) -> wf (r, g).
Proof.
  intros l r g [Hr Hp]; split; [exact Hr|].
  intros p I; apply Hp; cbn; apply in_or_app; right; exact I.
Qed.

Lemma step_wf : forall c a c', bigstep c a c' -> wf c -> wf c' /\ qlen c' = qlen c.
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

(******************************************************************************)
(** Steps from states equal up to [tidy] match *)

Lemma ok_tidy : forall s r s' g,
  get_pid s < List.length (mem_qnodes (fst r)) -> res_ok r ->
  tidy_state s' = tidy_state s -> tidy_res g = tidy_res r ->
  get_pid s' < List.length (mem_qnodes (fst g)) /\ res_ok g.
Proof.
  intros s r s' g Hs [Hl Hn] Es Er.
  destruct (tidy_res_parts _ _ Er) as [Eq El].
  rewrite (tidy_state_pid _ _ Es), Eq; unfold res_ok; rewrite Eq, El.
  auto.
Qed.

(* One base case: [d] runs the same block as [c] from a state equal up to
   [tidy]; the [*_tidy] lemma [lem] shows the results agree. *)
Ltac block_case lem :=
  let s := match goal with W : wf (PRC (_, ?s), _) |- _ => s end in
  let r := match goal with W : wf (PRC (_, _), ?r) |- _ => r end in
  let W := match goal with W : wf (PRC (_, _), _) |- _ => W end in
  let t := fresh "t" in let s' := fresh "s'" in let g := fresh "g" in
  let E := fresh "E" in let Et := fresh "Et" in let Es := fresh "Es" in
  let Er := fresh "Er" in let Hr := fresh "Hr" in let Hp := fresh "Hp" in
  let Hs := fresh "Hs" in let Hs' := fresh "Hs'" in let Hr' := fresh "Hr'" in
  intros [[[t s'] | ? ?] g] E; cbn [tidy tidy_sys fst snd] in E;
  [|discriminate]; injection E as Et Es Er; subst t;
  destruct W as [Hr Hp];
  assert (Hs : get_pid s < List.length (mem_qnodes (fst r)))
    by (apply Hp; left; reflexivity);
  destruct (ok_tidy _ _ _ _ Hs Hr Es Er) as [Hs' Hr'];
  eexists; split; [constructor|];
  rewrite (lem _ _ _ Hs' Hr'), (lem _ _ _ Hs Hr), Es, Er; reflexivity.

Lemma step_tidy : forall c a c', bigstep c a c' -> wf c ->
  forall d, tidy d = tidy c -> exists d', bigstep d a d' /\ tidy d' = tidy c'.
Proof.
  induction 1 as [b s r | c0 s r | c0 s r | c0 s r | c0 s r | y s r | y s r
                 | a l1 l2 r gr1 gr2 T IH | a l r1 r2 gr1 gr2 T IH]; intros W.
  - intros [[[t s'] | xl xr] g] E; cbn [tidy tidy_sys fst snd] in E;
      [|discriminate]; injection E as Et Es Er; subst t.
    eexists; split; [constructor|].
    rewrite (do_main_loop_tidy b s' g), (do_main_loop_tidy b s r), Es, Er.
    reflexivity.
  - block_case do_acquire_tidy.
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

(******************************************************************************)
(** The two semantics agree, up to [tidy] *)

Lemma link : forall c d, wf c -> tidy c = tidy d ->
  weak_bisimilar bigstep bigstep_norm c d.
Proof.
  cofix CH; intros c d W E; constructor; constructor.
  - intros c' a T.
    destruct (step_tidy _ _ _ T W d (eq_sym E)) as [d' [T' E']].
    exists (tidy d'); split.
    + apply inject_weak; constructor; exact T'.
    + apply CH; [exact (proj1 (step_wf _ _ _ T W))|].
      rewrite tidy_idem; exact (eq_sym E').
  - intros e a T; inversion T as [d0 a0 d' T']; subst.
    destruct (step_tidy _ _ _ T' (wf_tidy _ _ E W) c E) as [c' [Tc Ec]].
    exists c'; split.
    + apply inject_weak; exact Tc.
    + apply CH; [exact (proj1 (step_wf _ _ _ Tc W))|].
      rewrite tidy_idem; exact Ec.
Qed.

(* What the plugin proves over [bigstep_norm] holds over [bigstep], from
   any well-formed state. *)
Theorem wsim_transfer {N : Type} (ltsN : LTS N label) : forall c n,
  wf c -> weak_sim bigstep_norm ltsN c n -> weak_sim bigstep ltsN c n.
Proof.
  intros c n W H.
  apply (weak_sim_trans bigstep bigstep_norm ltsN c c n); [|exact H].
  apply weak_bisimilar_sim, link; [exact W | reflexivity].
Qed.

Theorem wsim_transfer_r {N : Type} (ltsN : LTS N label) : forall n c,
  wf c -> weak_sim ltsN bigstep_norm n c -> weak_sim ltsN bigstep n c.
Proof.
  intros n c W H.
  apply (weak_sim_trans ltsN bigstep_norm bigstep n c c H).
  apply weak_bisimilar_sim, weak_bisimilar_sym, link; [exact W | reflexivity].
Qed.

Theorem wbis_transfer {N : Type} (ltsN : LTS N label) : forall c n,
  wf c -> weak_bisimilar bigstep_norm ltsN c n -> weak_bisimilar bigstep ltsN c n.
Proof.
  intros c n W H.
  apply (weak_bisimilar_trans bigstep bigstep_norm ltsN c c n); [|exact H].
  apply link; [exact W | reflexivity].
Qed.

(******************************************************************************)
(** Every initial composition is well formed *)

Lemma qnodes_create_length : forall n, List.length (qnodes_create n) = S n.
Proof. induction n as [|n IH]; cbn; [reflexivity | rewrite IH; reflexivity]. Qed.

Lemma qnodes_create_nexts : forall n m, nexts_in m (qnodes_create n).
Proof.
  induction n as [|n IH]; intros m j q E.
  - destruct j as [|[|j]]; cbn in E; inversion E; subst; exact I.
  - destruct j as [|j]; cbn in E; [inversion E; subst; exact I | exact (IH m j q E)].
Qed.

Lemma pids_create : forall n x p, In p (pids (sys_create n x)) -> p <= n.
Proof.
  induction n as [|n IH]; intros x p I; cbn in I.
  - destruct I as [<- | []]; lia.
  - destruct I as [<- | I]; [lia | apply IH in I; lia].
Qed.

Theorem composition_create_wf : forall n x, wf (composition_create n x).
Proof.
  intros n x; unfold wf, res_ok; split; [split|].
  - exact I.
  - apply qnodes_create_nexts.
  - intros p I; apply pids_create in I; unfold qlen; cbn.
    rewrite qnodes_create_length; lia.
Qed.
