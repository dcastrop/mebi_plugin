(** * The certificate's top level: from the logged check to a simulation

    Proved once (doc/certificate.md, "The theorem from the certificate").
    A goal's proof instantiates these theorems; nothing here is computed.

    The engine's pairs are vectors [p ++ q]: [p] the left system's state,
    [q] the right one's ([lib/learn/learn.ml]: the answering side first).
    The certificate's relations are lists of [(label, codes, diagram)]: a
    step [VStep rs l a b] is by one of them, with label [l]. The
    answerer's relations are logged backwards (each tuple's read and
    written values swapped): [Fw inv l w w'], one step forward from [w] to
    [w'], is a step of [inv] from [w'] to [w].

    Two parts. The {e game} ([VWSim], [VSSim]): every move from a pair of
    the relation [z] is answered into [z], on vectors; it follows from the
    logged facts by the lemmas of the toolkit ([All], [all_back],
    [move_ok], [vwsim_of], ...). The {e transfer} ([cert_weak_sim], ...):
    with an encoding of each system's states as vectors, complete for the
    mover (every step of an encoded state is a relation's step) and sound
    for the answerer (every relation's step is a step of the system), the
    game gives the relation of [theories/Bisimilarity.v] between the
    states encoded by a pair of [z]. *)

From Stdlib Require Import List NArith.
From Stdlib Require Import Relations.Relation_Operators.
From MEBI Require Import Bisimilarity DdTree.
Import ListNotations.

Section Vectors.
  Context {A : Type}.

  (** a relation of the certificate *)
  Definition rel A := (option A * list code * dd)%type.

  Definition VStep (rs : list (rel A)) (l : option A) (a b : list N) : Prop :=
    exists cs r, In (l, cs, r) rs /\ exists t, In_t r t /\ rstep cs t a b.

  (** one step forward of the answerer: its relations are logged
      backwards *)
  Definition Fw (inv : list (rel A)) (l : option A) (w w' : list N) : Prop :=
    VStep inv l w' w.

  Definition Taus (inv : list (rel A)) := clos_refl_trans_1n (list N) (Fw inv None).

  (** a weak answer: silent steps; or silent steps, the label, silent
      steps *)
  Definition WAns (inv : list (rel A)) (l : option A) (w w' : list N) : Prop :=
    match l with
    | None => Taus inv w w'
    | Some _ => exists w1 w2, Taus inv w w1 /\ Fw inv l w1 w2 /\ Taus inv w2 w'
    end.

  (** the game on vectors, weak and strong *)
  Definition VWSim (rs inv : list (rel A)) (z : dd) : Prop :=
    forall w l w', In_t z w -> VStep rs l w w' -> exists w'', WAns inv l w' w'' /\ In_t z w''.

  Definition VSSim (rs inv : list (rel A)) (z : dd) : Prop :=
    forall w l w', In_t z w -> VStep rs l w w' -> exists w'', Fw inv l w' w'' /\ In_t z w''.

  (** ** The toolkit: from the logged facts to the game *)

  (** every vector of a set has a property *)
  Definition All (x : dd) (P : list N -> Prop) : Prop := forall w, In_t x w -> P w.

  Lemma all_0 : forall P, All D0 P.
  Proof. intros P w []. Qed.

  Lemma all_u : forall a b c P, U a b c -> All a P -> All b P -> All c P.
  Proof. intros a b c P H Ha Hb w Hw; apply H in Hw as [K|K]; auto. Qed.

  Lemma all_mi : forall a b c P, Mi a b c -> All a P -> All c P.
  Proof. intros a b c P [H _] Ha w Hw; exact (Ha w (H w Hw)). Qed.

  (** an answerer's image backwards, sound: each of its vectors steps
      forward to one of the set it came from *)
  Lemma all_back : forall inv l cs r x R (P Q : list N -> Prop), In (l, cs, r) inv ->
    PostS cs x r R -> All x P -> (forall b a, Fw inv l b a -> P a -> Q b) -> All R Q.
  Proof.
    intros inv l cs r x R P Q Hin HS Hx HQ b Hb.
    destruct (HS b Hb) as [t [a [Ha [Hr Hs]]]].
    apply (HQ b a); [exists cs, r; split; [exact Hin | exists t; auto] | exact (Hx a Ha)].
  Qed.

  (** reaching a property by the answerer's silent steps *)
  Definition Reach (inv : list (rel A)) (P : list N -> Prop) (w : list N) : Prop :=
    exists w', Taus inv w w' /\ P w'.

  Lemma all_reach : forall inv x (P : list N -> Prop), All x P -> All x (Reach inv P).
  Proof. intros inv x P H w Hw; exists w; split; [constructor | exact (H w Hw)]. Qed.

  Lemma reach_back : forall inv P b a, Fw inv None b a -> Reach inv P a -> Reach inv P b.
  Proof. intros inv P b a F [w' [T H]]; exists w'; split; [econstructor; eauto | exact H]. Qed.

  (** membership, as a predicate *)
  Definition InZ (z : dd) (w : list N) : Prop := In_t z w.

  (** the pairs from which the answerer answers [l] into [z]: silent
      steps, [l], silent steps *)
  Definition OkL (inv : list (rel A)) (z : dd) (l : option A) : list N -> Prop :=
    Reach inv (fun w1 => exists w2, Fw inv l w1 w2 /\ Reach inv (InZ z) w2).

  Lemma okl_back : forall inv z l b a, Fw inv l b a -> Reach inv (InZ z) a ->
    OkL inv z l b.
  Proof. intros inv z l b a F H; exists b; split; [constructor | exists a; auto]. Qed.

  Lemma okl_wans : forall inv z a w, OkL inv z (Some a) w ->
    exists w'', WAns inv (Some a) w w'' /\ In_t z w''.
  Proof.
    intros inv z a w [w1 [T1 [w2 [F [w3 [T3 H]]]]]].
    exists w3; split; [exists w1, w2; auto | exact H].
  Qed.

  Lemma reach_wans : forall inv z w, Reach inv (InZ z) w ->
    exists w'', WAns inv None w w'' /\ In_t z w''.
  Proof. intros inv z w [w' [T H]]; exists w'; auto. Qed.

  (** the mover's image of [z], complete, within the pairs [ok]
      (their difference empty): every move from [z] lands in [ok] *)
  Lemma move_ok : forall cs z r I ok (P : list N -> Prop), Srt z -> Srt r ->
    PostC cs z r I -> Mi I ok D0 -> All ok P ->
    forall w t w', In_t z w -> In_t r t -> rstep cs t w w' -> P w'.
  Proof.
    intros cs z r I ok P Sz Sr HC HM Hok w t w' Hw Hr Hs.
    exact (Hok w' (mi_empty I ok HM w' (HC Sz Sr t w w' Hw Hr Hs))).
  Qed.

  (** the game from one fact per relation of the mover *)
  Definition MoveW (inv : list (rel A)) (z : dd) (e : rel A) : Prop :=
    let '(l, cs, r) := e in
    forall w t w', In_t z w -> In_t r t -> rstep cs t w w' ->
      exists w'', WAns inv l w' w'' /\ In_t z w''.

  Lemma vwsim_of : forall rs inv z, Forall (MoveW inv z) rs -> VWSim rs inv z.
  Proof.
    intros rs inv z H w l w' Hw [cs [r [Hin [t [Hr Hs]]]]].
    rewrite Forall_forall in H; exact (H (l, cs, r) Hin w t w' Hw Hr Hs).
  Qed.

  (** the steps of a proof, each one lemma: a set's own property, a
      silent round's images, the first images of a label, a move *)
  Lemma all_self : forall z, All z (InZ z).
  Proof. intros z w H; exact H. Qed.

  Lemma all_reach_self : forall inv z, All z (Reach inv (InZ z)).
  Proof. intros inv z; apply all_reach, all_self. Qed.

  Lemma all_back_reach : forall inv cs r x R P, In (None, cs, r) inv -> PostS cs x r R ->
    All x (Reach inv P) -> All R (Reach inv P).
  Proof. intros; eapply all_back; eauto; intros; eapply reach_back; eauto. Qed.

  Lemma all_back_okl : forall inv l cs r x R z, In (l, cs, r) inv -> PostS cs x r R ->
    All x (Reach inv (InZ z)) -> All R (OkL inv z l).
  Proof. intros; eapply all_back; eauto; intros; eapply okl_back; eauto. Qed.

  Lemma all_back_okl_tau : forall inv l cs r x R z, In (None, cs, r) inv -> PostS cs x r R ->
    All x (OkL inv z l) -> All R (OkL inv z l).
  Proof. intros; eapply all_back_reach; eauto. Qed.

  Definition OkS (inv : list (rel A)) (z : dd) (l : option A) (o : list N) : Prop :=
    exists w2, Fw inv l o w2 /\ In_t z w2.

  Lemma all_back_strong : forall inv l cs r R z, In (l, cs, r) inv -> PostS cs z r R ->
    All R (OkS inv z l).
  Proof.
    intros inv l cs r R z Hin HS; eapply all_back; [exact Hin | exact HS | apply all_self |].
    intros b a F H; exists a; auto.
  Qed.

  Lemma movew_l : forall inv z a cs r I ok, Srt z -> Srt r -> PostC cs z r I -> Mi I ok D0 ->
    All ok (OkL inv z (Some a)) -> MoveW inv z (Some a, cs, r).
  Proof.
    intros inv z a cs r I ok Sz Sr HC HM Hok w t w' Hw Hr Hs.
    apply okl_wans; exact (move_ok cs z r I ok _ Sz Sr HC HM Hok w t w' Hw Hr Hs).
  Qed.

  Lemma movew_tau : forall inv z cs r I ok, Srt z -> Srt r -> PostC cs z r I -> Mi I ok D0 ->
    All ok (Reach inv (InZ z)) -> MoveW inv z (None, cs, r).
  Proof.
    intros inv z cs r I ok Sz Sr HC HM Hok w t w' Hw Hr Hs.
    apply reach_wans; exact (move_ok cs z r I ok _ Sz Sr HC HM Hok w t w' Hw Hr Hs).
  Qed.

  Definition MoveS (inv : list (rel A)) (z : dd) (e : rel A) : Prop :=
    let '(l, cs, r) := e in
    forall w t w', In_t z w -> In_t r t -> rstep cs t w w' ->
      exists w'', Fw inv l w' w'' /\ In_t z w''.

  Lemma vssim_of : forall rs inv z, Forall (MoveS inv z) rs -> VSSim rs inv z.
  Proof.
    intros rs inv z H w l w' Hw [cs [r [Hin [t [Hr Hs]]]]].
    rewrite Forall_forall in H; exact (H (l, cs, r) Hin w t w' Hw Hr Hs).
  Qed.

  Lemma moves_of : forall inv z l cs r I ok, Srt z -> Srt r -> PostC cs z r I -> Mi I ok D0 ->
    All ok (OkS inv z l) -> MoveS inv z (l, cs, r).
  Proof.
    intros inv z l cs r I ok Sz Sr HC HM Hok w t w' Hw Hr Hs.
    exact (move_ok cs z r I ok _ Sz Sr HC HM Hok w t w' Hw Hr Hs).
  Qed.
End Vectors.

(** ** The transfer: from the game on vectors to the relation on states *)

Section Transfer.
  Context {ML MR A : Type} (ltsL : LTS ML A) (ltsR : LTS MR A).
  (** each system's encoding: a state, and its vector; the left system's
      vectors have [nL] positions *)
  Variables (EL : ML -> list N -> Prop) (ER : MR -> list N -> Prop) (z : dd) (nL : nat).

  Definition LenL : Prop := forall x p, EL x p -> length p = nL.

  (** The right system moves, the left answers. *)
  Variables (relR invL : list (rel A)).

  Definition CompleteR : Prop :=
    forall r q p l r', ER r q -> length p = nL -> In_t z (p ++ q) -> ltsR r l r' ->
      exists q', ER r' q' /\ VStep relR l (p ++ q) (p ++ q').

  Definition SoundL : Prop :=
    forall x p q l w1, EL x p -> Fw invL l (p ++ q) w1 ->
      exists x1 p1, w1 = p1 ++ q /\ EL x1 p1 /\ ltsL x l x1.

  Lemma taus_left : SoundL -> forall q w w', Taus invL w w' ->
    forall x p, w = p ++ q -> EL x p ->
    exists x' p', w' = p' ++ q /\ EL x' p' /\ silent ltsL x x'.
  Proof.
    intros S q w w' T; induction T as [w|w w1 w' F T IH]; intros x p -> H.
    - exists x, p; split; [reflexivity | split; [exact H | constructor]].
    - destruct (S x p q None w1 H F) as [x1 [p1 [-> [H1 St]]]].
      destruct (IH x1 p1 eq_refl H1) as [x' [p' [-> [H' Si]]]].
      exists x', p'; split; [reflexivity | split; [exact H' | econstructor; eauto]].
  Qed.

  Lemma answer_left : SoundL -> forall l x p q w'', EL x p -> WAns invL l (p ++ q) w'' ->
    exists x'' p'', w'' = p'' ++ q /\ EL x'' p'' /\ weak ltsL x x'' l.
  Proof.
    intros S [a|] x p q w'' H W.
    - destruct W as [w1 [w2 [T1 [F T2]]]].
      destruct (taus_left S q _ _ T1 x p eq_refl H) as [x1 [p1 [-> [H1 S1]]]].
      destruct (S x1 p1 q (Some a) w2 H1 F) as [x2 [p2 [-> [H2 St]]]].
      destruct (taus_left S q _ _ T2 x2 p2 eq_refl H2) as [x3 [p3 [-> [H3 S3]]]].
      exists x3, p3; split; [reflexivity | split; [exact H3 | econstructor; eauto]].
    - destruct (taus_left S q _ _ W x p eq_refl H) as [x' [p' [-> [H' Si]]]].
      exists x', p'; split; [reflexivity | split; [exact H' | constructor; exact Si]].
  Qed.

  (** the right system weakly simulated by the left *)
  Theorem cert_weak_sim : VWSim relR invL z -> LenL -> CompleteR -> SoundL ->
    forall x r p q, EL x p -> ER r q -> In_t z (p ++ q) -> weak_sim ltsR ltsL r x.
  Proof.
    intros G Ln C S x r p q HL HR Hz.
    apply (weak_sim_coind (fun r x => exists p q, EL x p /\ ER r q /\ In_t z (p ++ q)));
      [|exists p, q; auto].
    clear x r p q HL HR Hz; intros r x [p [q [HL [HR Hz]]]]; constructor; intros r' l T.
    destruct (C r q p l r' HR (Ln x p HL) Hz T) as [q' [HR' Hm]].
    destruct (G _ _ _ Hz Hm) as [w'' [W Hz'']].
    destruct (answer_left S l x p q' w'' HL W) as [x'' [p'' [-> [HL'' Wk]]]].
    exists x''; split; [exact Wk | exists p'', q'; auto].
  Qed.

  Theorem cert_strong_sim : VSSim relR invL z -> LenL -> CompleteR -> SoundL ->
    forall x r p q, EL x p -> ER r q -> In_t z (p ++ q) -> strong_sim ltsR ltsL r x.
  Proof.
    intros G Ln C S x r p q HL HR Hz.
    apply (strong_sim_coind (fun r x => exists p q, EL x p /\ ER r q /\ In_t z (p ++ q)));
      [|exists p, q; auto].
    clear x r p q HL HR Hz; intros r x [p [q [HL [HR Hz]]]]; constructor; intros r' l T.
    destruct (C r q p l r' HR (Ln x p HL) Hz T) as [q' [HR' Hm]].
    destruct (G _ _ _ Hz Hm) as [w'' [F Hz'']].
    destruct (S x p q' l w'' HL F) as [x1 [p1 [-> [H1 St]]]].
    exists x1; split; [exact St | exists p1, q'; auto].
  Qed.

  (** The left system moves, the right answers (for bisimilarity). *)
  Variables (relL invR : list (rel A)).

  Definition CompleteL : Prop :=
    forall x p q l x', EL x p -> In_t z (p ++ q) -> ltsL x l x' ->
      exists p', EL x' p' /\ VStep relL l (p ++ q) (p' ++ q).

  Definition SoundR : Prop :=
    forall r q p l w1, ER r q -> length p = nL -> Fw invR l (p ++ q) w1 ->
      exists r1 q1, w1 = p ++ q1 /\ ER r1 q1 /\ ltsR r l r1.

  Lemma taus_right : SoundR -> forall p, length p = nL -> forall w w', Taus invR w w' ->
    forall r q, w = p ++ q -> ER r q ->
    exists r' q', w' = p ++ q' /\ ER r' q' /\ silent ltsR r r'.
  Proof.
    intros S p Lp w w' T; induction T as [w|w w1 w' F T IH]; intros r q -> H.
    - exists r, q; split; [reflexivity | split; [exact H | constructor]].
    - destruct (S r q p None w1 H Lp F) as [r1 [q1 [-> [H1 St]]]].
      destruct (IH r1 q1 eq_refl H1) as [r' [q' [-> [H' Si]]]].
      exists r', q'; split; [reflexivity | split; [exact H' | econstructor; eauto]].
  Qed.

  Lemma answer_right : SoundR -> forall l r p q w'', length p = nL -> ER r q ->
    WAns invR l (p ++ q) w'' ->
    exists r'' q'', w'' = p ++ q'' /\ ER r'' q'' /\ weak ltsR r r'' l.
  Proof.
    intros S [a|] r p q w'' Lp H W.
    - destruct W as [w1 [w2 [T1 [F T2]]]].
      destruct (taus_right S p Lp _ _ T1 r q eq_refl H) as [r1 [q1 [-> [H1 S1]]]].
      destruct (S r1 q1 p (Some a) w2 H1 Lp F) as [r2 [q2 [-> [H2 St]]]].
      destruct (taus_right S p Lp _ _ T2 r2 q2 eq_refl H2) as [r3 [q3 [-> [H3 S3]]]].
      exists r3, q3; split; [reflexivity | split; [exact H3 | econstructor; eauto]].
    - destruct (taus_right S p Lp _ _ W r q eq_refl H) as [r' [q' [-> [H' Si]]]].
      exists r', q'; split; [reflexivity | split; [exact H' | constructor; exact Si]].
  Qed.

  (** both ways: weak bisimilarity of the left and the right systems *)
  Theorem cert_weak_bisim : LenL -> VWSim relR invL z -> CompleteR -> SoundL ->
    VWSim relL invR z -> CompleteL -> SoundR ->
    forall x r p q, EL x p -> ER r q -> In_t z (p ++ q) -> weak_bisimilar ltsL ltsR x r.
  Proof.
    intros Ln G1 C1 S1 G2 C2 S2 x r p q HL HR Hz.
    apply (weak_bisimilar_coind (fun x r => exists p q, EL x p /\ ER r q /\ In_t z (p ++ q)));
      [|exists p, q; auto].
    clear x r p q HL HR Hz; intros x r [p [q [HL [HR Hz]]]]; constructor.
    - intros x' l T.
      destruct (C2 x p q l x' HL Hz T) as [p' [HL' Hm]].
      destruct (G2 _ _ _ Hz Hm) as [w'' [W Hz'']].
      destruct (answer_right S2 l r p' q w'' (Ln x' p' HL') HR W) as [r'' [q'' [-> [HR'' Wk]]]].
      exists r''; split; [exact Wk | exists p', q''; auto].
    - intros r' l T.
      destruct (C1 r q p l r' HR (Ln x p HL) Hz T) as [q' [HR' Hm]].
      destruct (G1 _ _ _ Hz Hm) as [w'' [W Hz'']].
      destruct (answer_left S1 l x p q' w'' HL W) as [x'' [p'' [-> [HL'' Wk]]]].
      exists x''; split; [exact Wk | exists p'', q'; auto].
  Qed.

  Theorem cert_strong_bisim : LenL -> VSSim relR invL z -> CompleteR -> SoundL ->
    VSSim relL invR z -> CompleteL -> SoundR ->
    forall x r p q, EL x p -> ER r q -> In_t z (p ++ q) -> strong_bisimilar ltsL ltsR x r.
  Proof.
    intros Ln G1 C1 S1 G2 C2 S2 x r p q HL HR Hz.
    apply (strong_bisimilar_coind (fun x r => exists p q, EL x p /\ ER r q /\ In_t z (p ++ q)));
      [|exists p, q; auto].
    clear x r p q HL HR Hz; intros x r [p [q [HL [HR Hz]]]]; constructor.
    - intros x' l T.
      destruct (C2 x p q l x' HL Hz T) as [p' [HL' Hm]].
      destruct (G2 _ _ _ Hz Hm) as [w'' [F Hz'']].
      destruct (S2 r q p' l w'' HR (Ln x' p' HL') F) as [r1 [q1 [-> [H1 St]]]].
      exists r1; split; [exact St | exists p', q1; auto].
    - intros r' l T.
      destruct (C1 r q p l r' HR (Ln x p HL) Hz T) as [q' [HR' Hm]].
      destruct (G1 _ _ _ Hz Hm) as [w'' [F Hz'']].
      destruct (S1 x p q' l w'' HL F) as [x1 [p1 [-> [H1 St]]]].
      exists x1; split; [exact St | exists p1, q'; auto].
  Qed.
End Transfer.

(** a bisimilarity proved with the systems the other way round *)
Lemma strong_bisimilar_sym {M N A} {ltsM : LTS M A} {ltsN : LTS N A} :
  forall s t, strong_bisimilar ltsM ltsN s t -> strong_bisimilar ltsN ltsM t s.
Proof.
  cofix CH; intros s t H; constructor; constructor; intros x2 a T.
  - destruct (sbisim_r (out_sbisim H) T) as [m2 [W B]].
    exists m2; split; [exact W | exact (CH _ _ B)].
  - destruct (sbisim_l (out_sbisim H) T) as [n2 [W B]].
    exists n2; split; [exact W | exact (CH _ _ B)].
Qed.

(** For the plugin (src/game_proof.ml) *)
Register rel as mebi.dg.rel.
Register VStep as mebi.dg.VStep.
Register Fw as mebi.dg.Fw.
Register VWSim as mebi.dg.VWSim.
Register VSSim as mebi.dg.VSSim.
Register All as mebi.dg.All.
Register all_0 as mebi.dg.all_0.
Register all_u as mebi.dg.all_u.
Register all_mi as mebi.dg.all_mi.
Register InZ as mebi.dg.InZ.
Register Reach as mebi.dg.Reach.
Register OkL as mebi.dg.OkL.
Register OkS as mebi.dg.OkS.
Register all_self as mebi.dg.all_self.
Register all_reach_self as mebi.dg.all_reach_self.
Register all_back_reach as mebi.dg.all_back_reach.
Register all_back_okl as mebi.dg.all_back_okl.
Register all_back_okl_tau as mebi.dg.all_back_okl_tau.
Register all_back_strong as mebi.dg.all_back_strong.
Register MoveW as mebi.dg.MoveW.
Register MoveS as mebi.dg.MoveS.
Register movew_l as mebi.dg.movew_l.
Register movew_tau as mebi.dg.movew_tau.
Register moves_of as mebi.dg.moves_of.
Register vwsim_of as mebi.dg.vwsim_of.
Register vssim_of as mebi.dg.vssim_of.
Register cert_weak_sim as mebi.dg.cert_weak_sim.
Register cert_strong_sim as mebi.dg.cert_strong_sim.
Register cert_weak_bisim as mebi.dg.cert_weak_bisim.
Register cert_strong_bisim as mebi.dg.cert_strong_bisim.
Register Forall as mebi.dg.Forall.
Register Forall_nil as mebi.dg.Forall_nil.
Register Forall_cons as mebi.dg.Forall_cons.
Register In as mebi.dg.In.
Register conj as mebi.dg.conj.
Register or_introl as mebi.dg.or_introl.
Register or_intror as mebi.dg.or_intror.
Register LenL as mebi.dg.LenL.
Register strong_bisimilar_sym as mebi.dg.strong_bisimilar_sym.
Register weak_bisimilar_sym as mebi.dg.weak_bisimilar_sym.
