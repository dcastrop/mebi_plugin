(** * Proofs up to a strong bisimulation (doc/upto.md, route B)

    Proved once; a goal's proof instantiates these theorems; nothing here
    is computed.

    The engine plays the game on representatives: a state stands for its
    class, the states equal to it up to reordering ([Eq], written [≡] in
    doc/upto.md). Its relations are learned from representatives only, so
    an answer brings a pair back to representatives as it goes, by
    normalisation steps: relations of the certificate like the others
    ([nsL], [nsR], logged backwards as the answerer's relations are),
    silent steps of the answer that are not steps of a system. Two things
    make this sound:
    - [Eq] is a strong bisimulation of each system ([SimEq]: the rules
      commute with the reorderings), so a state moves as any state of its
      class does, up to [Eq] (weak bisimulation up to strong
      bisimilarity, Milner);
    - each normalisation step relates states equal up to [Eq]
      ([SoundNL], [SoundNR]).
    The game ([VWSimN]) is [DdGame]'s, with the answer's silent steps
    the answerer's or normalisation steps ([TausN]). The transfer
    ([cert_weak_sim_upto], [cert_weak_bisim_upto]) uses [DdGame]'s
    completeness and soundness of the encodings as they are; a real
    answer is carried along [Eq] at each normalisation step
    ([taus_left_upto]). *)

From Stdlib Require Import List NArith.
From Stdlib Require Import Relations.Relation_Operators.
From MEBI Require Import Bisimilarity DdTree DdGame.
Import ListNotations.

(** ** A strong bisimulation of one system *)
Section Congruence.
  Context {M A : Type} (lts : LTS M A).

  (** [Eq] is a strong simulation of [lts] *)
  Definition SimEq (Eq : M -> M -> Prop) : Prop :=
    forall x y, Eq x y -> forall a x', lts x a x' -> exists y', lts y a y' /\ Eq x' y'.

  Lemma silent_eq : forall Eq, SimEq Eq -> forall x x', silent lts x x' ->
    forall y, Eq x y -> exists y', silent lts y y' /\ Eq x' y'.
  Proof.
    intros Eq S x x' H; induction H as [x|x x1 x' T H IH]; intros y E.
    - exists y; split; [constructor | exact E].
    - destruct (S x y E None x1 T) as [y1 [T1 E1]].
      destruct (IH y1 E1) as [y' [H' E']].
      exists y'; split; [econstructor; [exact T1 | exact H'] | exact E'].
  Qed.

  Lemma weak_eq : forall Eq, SimEq Eq -> forall x x' a, weak lts x x' a ->
    forall y, Eq x y -> exists y', weak lts y y' a /\ Eq x' y'.
  Proof.
    intros Eq S x x' a W y E; destruct W as [b z t H1 T H2 | H].
    - destruct (silent_eq Eq S x z H1 y E) as [z' [H1' E1]].
      destruct (S z z' E1 (Some b) t T) as [t' [T' E2]].
      destruct (silent_eq Eq S t x' H2 t' E2) as [y' [H2' E3]].
      exists y'; split; [exact (wk_some lts y y' b z' t' H1' T' H2') | exact E3].
    - destruct (silent_eq Eq S x x' H y E) as [y' [H' E']].
      exists y'; split; [exact (wk_none lts y y' H') | exact E'].
  Qed.

  (** [Eq] from one step: the reflexive-transitive closure of [Ro], each
      step of which has its inverse as a path, and commutes with the
      steps of [lts] *)
  Definition Cong (Ro : M -> M -> Prop) := clos_refl_trans_1n M Ro.

  Lemma cong_refl : forall Ro x, Cong Ro x x.
  Proof. intros Ro x; constructor. Qed.

  Lemma cong_trans : forall Ro x y w, Cong Ro x y -> Cong Ro y w -> Cong Ro x w.
  Proof. intros Ro x y w H1 H2; exact (@clos_rt_trans M Ro x y w H1 H2). Qed.

  Lemma cong_step : forall (Ro : M -> M -> Prop) x y w, Ro x y -> Cong Ro y w -> Cong Ro x w.
  Proof. intros Ro x y w R H; econstructor; [exact R | exact H]. Qed.

  Lemma cong_sym : forall Ro, (forall x y, Ro x y -> Cong Ro y x) ->
    forall x y, Cong Ro x y -> Cong Ro y x.
  Proof.
    intros Ro I x y H; induction H as [x|x w y R H IH]; [constructor |].
    exact (cong_trans Ro y w x IH (I x w R)).
  Qed.

  Lemma cong_sim : forall Ro,
    (forall x y, Ro x y -> forall a x', lts x a x' -> exists y', lts y a y' /\ Cong Ro x' y') ->
    SimEq (Cong Ro).
  Proof.
    intros Ro S x y H; induction H as [x|x w y R H IH]; intros a x' T.
    - exists x'; split; [exact T | constructor].
    - destruct (S x w R a x' T) as [w' [T' E1]].
      destruct (IH a w' T') as [y' [T'' E2]].
      exists y'; split; [exact T'' | exact (cong_trans Ro x' w' y' E1 E2)].
  Qed.

  (** an equivalence that is a strong simulation: a strong bisimulation *)
  Definition Equiv (Eq : M -> M -> Prop) : Prop :=
    (forall x, Eq x x) /\ (forall x y, Eq x y -> Eq y x) /\
    (forall x y w, Eq x y -> Eq y w -> Eq x w) /\ SimEq Eq.

  Lemma cong_equiv : forall Ro, (forall x y, Ro x y -> Cong Ro y x) ->
    (forall x y, Ro x y -> forall a x', lts x a x' -> exists y', lts y a y' /\ Cong Ro x' y') ->
    Equiv (Cong Ro).
  Proof.
    intros Ro I S; split; [exact (cong_refl Ro) | split; [exact (cong_sym Ro I) |]].
    split; [exact (cong_trans Ro) | exact (cong_sim Ro S)].
  Qed.
End Congruence.

(** ** The game, normalisation steps among the answer's silent steps *)
Section GameN.
  Context {A : Type}.
  Variables (inv nsL nsR : list (rel A)).

  (** a normalisation step, of either side's part *)
  Definition NStep (w w' : list N) : Prop := Fw nsL None w w' \/ Fw nsR None w w'.

  (** a silent step of an answer: the answerer's, or a normalisation *)
  Definition TStep (w w' : list N) : Prop := Fw inv None w w' \/ NStep w w'.

  Definition TausN := clos_refl_trans_1n (list N) TStep.

  Definition WAnsN (l : option A) (w w' : list N) : Prop :=
    match l with
    | None => TausN w w'
    | Some _ => exists w1 w2, TausN w w1 /\ Fw inv l w1 w2 /\ TausN w2 w'
    end.

  Definition VWSimN (rs : list (rel A)) (z : dd) : Prop :=
    forall w l w', In_t z w -> VStep rs l w w' -> exists w'', WAnsN l w' w'' /\ In_t z w''.

  Definition ReachN (P : list N -> Prop) (w : list N) : Prop :=
    exists w', TausN w w' /\ P w'.

  Lemma all_reachN : forall x (P : list N -> Prop), All x P -> All x (ReachN P).
  Proof. intros x P H w Hw; exists w; split; [constructor | exact (H w Hw)]. Qed.

  Lemma all_reachN_self : forall z, All z (ReachN (InZ z)).
  Proof. intros z; apply all_reachN; intros w H; exact H. Qed.

  Lemma all_back_reachN : forall cs r x R P, In (None, cs, r) inv -> PostS cs x r R ->
    All x (ReachN P) -> All R (ReachN P).
  Proof.
    intros cs r x R P Hin HS Hx.
    apply (all_back inv None cs r x R _ _ Hin HS Hx).
    intros b a F [w' [T H]]; exists w'; split; [econstructor; [left; exact F | exact T] | exact H].
  Qed.

  Lemma all_back_reachN_nl : forall cs r x R P, In (None, cs, r) nsL -> PostS cs x r R ->
    All x (ReachN P) -> All R (ReachN P).
  Proof.
    intros cs r x R P Hin HS Hx.
    apply (all_back nsL None cs r x R _ _ Hin HS Hx).
    intros b a F [w' [T H]]; exists w'; split;
      [econstructor; [right; left; exact F | exact T] | exact H].
  Qed.

  Lemma all_back_reachN_nr : forall cs r x R P, In (None, cs, r) nsR -> PostS cs x r R ->
    All x (ReachN P) -> All R (ReachN P).
  Proof.
    intros cs r x R P Hin HS Hx.
    apply (all_back nsR None cs r x R _ _ Hin HS Hx).
    intros b a F [w' [T H]]; exists w'; split;
      [econstructor; [right; right; exact F | exact T] | exact H].
  Qed.

  (** the vectors from which the answerer answers [l] into [z] *)
  Definition OkLN (z : dd) (l : option A) : list N -> Prop :=
    ReachN (fun w1 => exists w2, Fw inv l w1 w2 /\ ReachN (InZ z) w2).

  Lemma all_back_oklN : forall l cs r x R z, In (l, cs, r) inv -> PostS cs x r R ->
    All x (ReachN (InZ z)) -> All R (OkLN z l).
  Proof.
    intros l cs r x R z Hin HS Hx.
    apply (all_back inv l cs r x R _ _ Hin HS Hx).
    intros b a F H; exists b; split; [constructor | exists a; split; [exact F | exact H]].
  Qed.

  Lemma all_back_oklN_tau : forall l cs r x R z, In (None, cs, r) inv -> PostS cs x r R ->
    All x (OkLN z l) -> All R (OkLN z l).
  Proof. intros l cs r x R z; exact (all_back_reachN cs r x R _). Qed.

  Lemma all_back_oklN_nl : forall l cs r x R z, In (None, cs, r) nsL -> PostS cs x r R ->
    All x (OkLN z l) -> All R (OkLN z l).
  Proof. intros l cs r x R z; exact (all_back_reachN_nl cs r x R _). Qed.

  Lemma all_back_oklN_nr : forall l cs r x R z, In (None, cs, r) nsR -> PostS cs x r R ->
    All x (OkLN z l) -> All R (OkLN z l).
  Proof. intros l cs r x R z; exact (all_back_reachN_nr cs r x R _). Qed.

  Lemma oklN_wans : forall z a w, OkLN z (Some a) w ->
    exists w'', WAnsN (Some a) w w'' /\ In_t z w''.
  Proof.
    intros z a w [w1 [T1 [w2 [F [w3 [T3 H]]]]]].
    exists w3; split; [exists w1, w2; split; [exact T1 | split; [exact F | exact T3]] | exact H].
  Qed.

  Lemma reachN_wans : forall z w, ReachN (InZ z) w -> exists w'', WAnsN None w w'' /\ In_t z w''.
  Proof. intros z w [w' [T H]]; exists w'; split; [exact T | exact H]. Qed.

  Definition MoveWN (z : dd) (e : rel A) : Prop :=
    let '(l, cs, r) := e in
    forall w t w', In_t z w -> In_t r t -> rstep cs t w w' ->
      exists w'', WAnsN l w' w'' /\ In_t z w''.

  Lemma vwsimN_of : forall rs z, Forall (MoveWN z) rs -> VWSimN rs z.
  Proof.
    intros rs z H w l w' Hw [cs [r [Hin [t [Hr Hs]]]]].
    rewrite Forall_forall in H; exact (H (l, cs, r) Hin w t w' Hw Hr Hs).
  Qed.

  Lemma movewN_l : forall z a cs r I ok, Srt z -> Srt r -> PostC cs z r I -> Mi I ok D0 ->
    All ok (OkLN z (Some a)) -> MoveWN z (Some a, cs, r).
  Proof.
    intros z a cs r I ok Sz Sr HC HM Hok w t w' Hw Hr Hs.
    apply oklN_wans; exact (move_ok cs z r I ok _ Sz Sr HC HM Hok w t w' Hw Hr Hs).
  Qed.

  Lemma movewN_tau : forall z cs r I ok, Srt z -> Srt r -> PostC cs z r I -> Mi I ok D0 ->
    All ok (ReachN (InZ z)) -> MoveWN z (None, cs, r).
  Proof.
    intros z cs r I ok Sz Sr HC HM Hok w t w' Hw Hr Hs.
    apply reachN_wans; exact (move_ok cs z r I ok _ Sz Sr HC HM Hok w t w' Hw Hr Hs).
  Qed.

  (** the start: normalisation steps only, into [z] *)
  Definition NormZ (z : dd) (w : list N) : Prop :=
    exists w', clos_refl_trans_1n (list N) NStep w w' /\ In_t z w'.
End GameN.

(** ** The transfer up to [EqL], [EqR] *)
Section TransferUpto.
  Context {ML MR A : Type} (ltsL : LTS ML A) (ltsR : LTS MR A).
  Variables (EL : ML -> list N -> Prop) (ER : MR -> list N -> Prop) (z : dd) (nL : nat).
  Variables (EqL : ML -> ML -> Prop) (EqR : MR -> MR -> Prop) (nsL nsR : list (rel A)).

  (** each normalisation step: a state, and one [Eq] to it *)
  Definition SoundNL : Prop :=
    forall x p q w1, EL x p -> Fw nsL None (p ++ q) w1 ->
      exists x1 p1, w1 = p1 ++ q /\ EL x1 p1 /\ EqL x x1.

  Definition SoundNR : Prop :=
    forall r q p w1, ER r q -> length p = nL -> Fw nsR None (p ++ q) w1 ->
      exists r1 q1, w1 = p ++ q1 /\ ER r1 q1 /\ EqR r r1.

  Hypotheses (EqvL : Equiv ltsL EqL) (EqvR : Equiv ltsR EqR) (Ln : LenL EL nL)
    (SNL : SoundNL) (SNR : SoundNR).

  Let RfL := proj1 EqvL.
  Let SyL := proj1 (proj2 EqvL).
  Let TrL := proj1 (proj2 (proj2 EqvL)).
  Let SiL := proj2 (proj2 (proj2 EqvL)).
  Let RfR := proj1 EqvR.
  Let SyR := proj1 (proj2 EqvR).
  Let TrR := proj1 (proj2 (proj2 EqvR)).
  Let SiR := proj2 (proj2 (proj2 EqvR)).

  Lemma norm_sound : forall w w3, clos_refl_trans_1n (list N) (NStep nsL nsR) w w3 ->
    forall x r p q, w = p ++ q -> EL x p -> ER r q ->
    exists x3 r3 p3 q3, w3 = p3 ++ q3 /\ EL x3 p3 /\ ER r3 q3 /\ EqL x x3 /\ EqR r r3.
  Proof.
    intros w w3 H; induction H as [w|w w1 w3 [F|F] H IH]; intros x r p q -> HL HR.
    - exists x, r, p, q; split; [reflexivity | split; [exact HL | split; [exact HR |
        split; [exact (RfL x) | exact (RfR r)]]]].
    - destruct (SNL x p q w1 HL F) as [x1 [p1 [-> [H1 E1]]]].
      destruct (IH x1 r p1 q eq_refl H1 HR) as [x3 [r3 [p3 [q3 [-> [H3 [H3' [E3 E3']]]]]]]].
      exists x3, r3, p3, q3; split; [reflexivity | split; [exact H3 | split; [exact H3' |
        split; [exact (TrL x x1 x3 E1 E3) | exact E3']]]].
    - destruct (SNR r q p w1 HR (Ln x p HL) F) as [r1 [q1 [-> [H1 E1]]]].
      destruct (IH x r1 p q1 eq_refl HL H1) as [x3 [r3 [p3 [q3 [-> [H3 [H3' [E3 E3']]]]]]]].
      exists x3, r3, p3, q3; split; [reflexivity | split; [exact H3 | split; [exact H3' |
        split; [exact E3 | exact (TrR r r1 r3 E1 E3')]]]].
  Qed.

  (** the pairs of the proof: [Eq], each side, to a pair of [z] *)
  Definition UpZ (r : MR) (x : ML) : Prop :=
    exists r0 x0 p q, EqR r r0 /\ EqL x x0 /\ EL x0 p /\ ER r0 q /\ In_t z (p ++ q).

  Lemma upz_start : forall x r p q, EL x p -> ER r q -> NormZ nsL nsR z (p ++ q) -> UpZ r x.
  Proof.
    intros x r p q HL HR [w3 [Hn Hz]].
    destruct (norm_sound _ _ Hn x r p q eq_refl HL HR)
      as [x3 [r3 [p3 [q3 [-> [H3 [H3' [E3 E3']]]]]]]].
    exists r3, x3, p3, q3; split; [exact E3' | split; [exact E3 | split; [exact H3 |
      split; [exact H3' | exact Hz]]]].
  Qed.

  (** the left answers: its real silent steps, carried along [EqL] at
      each normalisation *)
  Section Left.
    Variables (invL : list (rel A)).
    Hypothesis (S : SoundL ltsL EL invL).

    Lemma taus_left_upto : forall w w', TausN invL nsL nsR w w' ->
      forall xr xe re p q, w = p ++ q -> EL xe p -> ER re q -> EqL xr xe ->
      exists xr' xe' re' p' q', w' = p' ++ q' /\ EL xe' p' /\ ER re' q' /\
        EqL xr' xe' /\ EqR re re' /\ silent ltsL xr xr'.
    Proof.
      intros w w' T; induction T as [w|w w1 w' [F|[F|F]] T IH];
        intros xr xe re p q -> HL HR E.
      - exists xr, xe, re, p, q; split; [reflexivity | split; [exact HL | split; [exact HR |
          split; [exact E | split; [exact (RfR re) | constructor]]]]].
      - destruct (S xe p q None w1 HL F) as [x1 [p1 [-> [H1 St]]]].
        destruct (SiL xe xr (SyL xr xe E) None x1 St) as [xr1 [St' E1]].
        destruct (IH xr1 x1 re p1 q eq_refl H1 HR (SyL x1 xr1 E1))
          as [xr' [xe' [re' [p' [q' [-> [H' [HR' [E' [ER' Si]]]]]]]]]].
        exists xr', xe', re', p', q'; split; [reflexivity | split; [exact H' | split;
          [exact HR' | split; [exact E' | split; [exact ER' | econstructor; [exact St' | exact Si]]]]]].
      - destruct (SNL xe p q w1 HL F) as [x1 [p1 [-> [H1 E1]]]].
        destruct (IH xr x1 re p1 q eq_refl H1 HR (TrL xr xe x1 E E1))
          as [xr' [xe' [re' [p' [q' [-> [H' [HR' [E' [ER' Si]]]]]]]]]].
        exists xr', xe', re', p', q'; split; [reflexivity | split; [exact H' | split;
          [exact HR' | split; [exact E' | split; [exact ER' | exact Si]]]]].
      - destruct (SNR re q p w1 HR (Ln xe p HL) F) as [r1 [q1 [-> [H1 E1]]]].
        destruct (IH xr xe r1 p q1 eq_refl HL H1 E)
          as [xr' [xe' [re' [p' [q' [-> [H' [HR' [E' [ER' Si]]]]]]]]]].
        exists xr', xe', re', p', q'; split; [reflexivity | split; [exact H' | split;
          [exact HR' | split; [exact E' | split; [exact (TrR re r1 re' E1 ER') | exact Si]]]]].
    Qed.

    Lemma answer_left_upto : forall l x0 r0 p q w'', EL x0 p -> ER r0 q ->
      WAnsN invL nsL nsR l (p ++ q) w'' ->
      exists xr x'' r'' p'' q'', w'' = p'' ++ q'' /\ EL x'' p'' /\ ER r'' q'' /\
        weak ltsL x0 xr l /\ EqL xr x'' /\ EqR r0 r''.
    Proof.
      intros [a|] x0 r0 p q w'' HL HR W.
      - destruct W as [w1 [w2 [T1 [F T2]]]].
        destruct (taus_left_upto _ _ T1 x0 x0 r0 p q eq_refl HL HR (RfL x0))
          as [xr1 [xe1 [re1 [p1 [q1 [-> [H1 [HR1 [E1 [ER1 Si1]]]]]]]]]].
        destruct (S xe1 p1 q1 (Some a) w2 H1 F) as [x2 [p2 [-> [H2 St]]]].
        destruct (SiL xe1 xr1 (SyL xr1 xe1 E1) (Some a) x2 St) as [xr2 [St' E2]].
        destruct (taus_left_upto _ _ T2 xr2 x2 re1 p2 q1 eq_refl H2 HR1 (SyL x2 xr2 E2))
          as [xr' [xe' [re' [p' [q' [-> [H' [HR' [E' [ER' Si]]]]]]]]]].
        exists xr', xe', re', p', q'; split; [reflexivity | split; [exact H' | split;
          [exact HR' | split; [exact (wk_some ltsL x0 xr' a xr1 xr2 Si1 St' Si) | split;
          [exact E' | exact (TrR r0 re1 re' ER1 ER')]]]]].
      - destruct (taus_left_upto _ _ W x0 x0 r0 p q eq_refl HL HR (RfL x0))
          as [xr' [xe' [re' [p' [q' [-> [H' [HR' [E' [ER' Si]]]]]]]]]].
        exists xr', xe', re', p', q'; split; [reflexivity | split; [exact H' | split;
          [exact HR' | split; [exact (wk_none ltsL x0 xr' Si) | split; [exact E' | exact ER']]]]].
    Qed.
  End Left.

  (** the right answers, likewise *)
  Section Right.
    Variables (invR : list (rel A)).
    Hypothesis (S : SoundR ltsR ER nL invR).

    Lemma taus_right_upto : forall w w', TausN invR nsL nsR w w' ->
      forall rr re xe p q, w = p ++ q -> EL xe p -> ER re q -> EqR rr re ->
      exists rr' re' xe' p' q', w' = p' ++ q' /\ EL xe' p' /\ ER re' q' /\
        EqR rr' re' /\ EqL xe xe' /\ silent ltsR rr rr'.
    Proof.
      intros w w' T; induction T as [w|w w1 w' [F|[F|F]] T IH];
        intros rr re xe p q -> HL HR E.
      - exists rr, re, xe, p, q; split; [reflexivity | split; [exact HL | split; [exact HR |
          split; [exact E | split; [exact (RfL xe) | constructor]]]]].
      - destruct (S re q p None w1 HR (Ln xe p HL) F) as [r1 [q1 [-> [H1 St]]]].
        destruct (SiR re rr (SyR rr re E) None r1 St) as [rr1 [St' E1]].
        destruct (IH rr1 r1 xe p q1 eq_refl HL H1 (SyR r1 rr1 E1))
          as [rr' [re' [xe' [p' [q' [-> [H' [HR' [E' [EL' Si]]]]]]]]]].
        exists rr', re', xe', p', q'; split; [reflexivity | split; [exact H' | split;
          [exact HR' | split; [exact E' | split; [exact EL' | econstructor; [exact St' | exact Si]]]]]].
      - destruct (SNL xe p q w1 HL F) as [x1 [p1 [-> [H1 E1]]]].
        destruct (IH rr re x1 p1 q eq_refl H1 HR E)
          as [rr' [re' [xe' [p' [q' [-> [H' [HR' [E' [EL' Si]]]]]]]]]].
        exists rr', re', xe', p', q'; split; [reflexivity | split; [exact H' | split;
          [exact HR' | split; [exact E' | split; [exact (TrL xe x1 xe' E1 EL') | exact Si]]]]].
      - destruct (SNR re q p w1 HR (Ln xe p HL) F) as [r1 [q1 [-> [H1 E1]]]].
        destruct (IH rr r1 xe p q1 eq_refl HL H1 (TrR rr re r1 E E1))
          as [rr' [re' [xe' [p' [q' [-> [H' [HR' [E' [EL' Si]]]]]]]]]].
        exists rr', re', xe', p', q'; split; [reflexivity | split; [exact H' | split;
          [exact HR' | split; [exact E' | split; [exact EL' | exact Si]]]]].
    Qed.

    Lemma answer_right_upto : forall l x0 r0 p q w'', EL x0 p -> ER r0 q ->
      WAnsN invR nsL nsR l (p ++ q) w'' ->
      exists rr r'' x'' p'' q'', w'' = p'' ++ q'' /\ EL x'' p'' /\ ER r'' q'' /\
        weak ltsR r0 rr l /\ EqR rr r'' /\ EqL x0 x''.
    Proof.
      intros [a|] x0 r0 p q w'' HL HR W.
      - destruct W as [w1 [w2 [T1 [F T2]]]].
        destruct (taus_right_upto _ _ T1 r0 r0 x0 p q eq_refl HL HR (RfR r0))
          as [rr1 [re1 [xe1 [p1 [q1 [-> [H1 [HR1 [E1 [EL1 Si1]]]]]]]]]].
        destruct (S re1 q1 p1 (Some a) w2 HR1 (Ln xe1 p1 H1) F) as [r2 [q2 [-> [H2 St]]]].
        destruct (SiR re1 rr1 (SyR rr1 re1 E1) (Some a) r2 St) as [rr2 [St' E2]].
        destruct (taus_right_upto _ _ T2 rr2 r2 xe1 p1 q2 eq_refl H1 H2 (SyR r2 rr2 E2))
          as [rr' [re' [xe' [p' [q' [-> [H' [HR' [E' [EL' Si]]]]]]]]]].
        exists rr', re', xe', p', q'; split; [reflexivity | split; [exact H' | split;
          [exact HR' | split; [exact (wk_some ltsR r0 rr' a rr1 rr2 Si1 St' Si) | split;
          [exact E' | exact (TrL x0 xe1 xe' EL1 EL')]]]]].
      - destruct (taus_right_upto _ _ W r0 r0 x0 p q eq_refl HL HR (RfR r0))
          as [rr' [re' [xe' [p' [q' [-> [H' [HR' [E' [EL' Si]]]]]]]]]].
        exists rr', re', xe', p', q'; split; [reflexivity | split; [exact H' | split;
          [exact HR' | split; [exact (wk_none ltsR r0 rr' Si) | split; [exact E' | exact EL']]]]].
    Qed.
  End Right.

  (** the right system weakly simulated by the left, up to [Eq] *)
  Theorem cert_weak_sim_upto : forall relR invL,
    VWSimN invL nsL nsR relR z -> CompleteR ltsR ER z nL relR -> SoundL ltsL EL invL ->
    forall x r p q, EL x p -> ER r q -> NormZ nsL nsR z (p ++ q) -> weak_sim ltsR ltsL r x.
  Proof.
    intros relR invL G C S x r p q HL HR Hz.
    apply (weak_sim_coind UpZ); [| exact (upz_start x r p q HL HR Hz)].
    clear x r p q HL HR Hz; intros r x [r0 [x0 [p [q [Er [Ex [HL [HR Hz]]]]]]]].
    constructor; intros r' l T.
    destruct (SiR r r0 Er l r' T) as [r0' [T0 Er']].
    destruct (C r0 q p l r0' HR (Ln x0 p HL) Hz T0) as [q' [HR' Hm]].
    destruct (G _ _ _ Hz Hm) as [w'' [W Hz'']].
    destruct (answer_left_upto invL S l x0 r0' p q' w'' HL HR' W)
      as [xr [x'' [r'' [p'' [q'' [-> [HL'' [HR'' [Wk [Ex'' Er'']]]]]]]]]].
    destruct (weak_eq ltsL EqL SiL x0 xr l Wk x (SyL x x0 Ex)) as [x1 [Wk1 E1]].
    exists x1; split; [exact Wk1 |].
    exists r'', x'', p'', q''; split; [exact (TrR r' r0' r'' Er' Er'') |].
    split; [exact (TrL x1 xr x'' (SyL xr x1 E1) Ex'') |].
    split; [exact HL'' | split; [exact HR'' | exact Hz'']].
  Qed.

  (** both ways, up to [Eq] *)
  Theorem cert_weak_bisim_upto : forall relR invL relL invR,
    VWSimN invL nsL nsR relR z -> CompleteR ltsR ER z nL relR -> SoundL ltsL EL invL ->
    VWSimN invR nsL nsR relL z -> CompleteL ltsL EL z relL -> SoundR ltsR ER nL invR ->
    forall x r p q, EL x p -> ER r q -> NormZ nsL nsR z (p ++ q) ->
    weak_bisimilar ltsL ltsR x r.
  Proof.
    intros relR invL relL invR G1 C1 S1 G2 C2 S2 x r p q HL HR Hz.
    apply (weak_bisimilar_coind (fun x r => UpZ r x)); [| exact (upz_start x r p q HL HR Hz)].
    clear x r p q HL HR Hz; intros x r [r0 [x0 [p [q [Er [Ex [HL [HR Hz]]]]]]]].
    constructor.
    - intros x' l T.
      destruct (SiL x x0 Ex l x' T) as [x0' [T0 Ex']].
      destruct (C2 x0 p q l x0' HL Hz T0) as [p' [HL' Hm]].
      destruct (G2 _ _ _ Hz Hm) as [w'' [W Hz'']].
      destruct (answer_right_upto invR S2 l x0' r0 p' q w'' HL' HR W)
        as [rr [r'' [x'' [p'' [q'' [-> [HL'' [HR'' [Wk [Er'' Ex'']]]]]]]]]].
      destruct (weak_eq ltsR EqR SiR r0 rr l Wk r (SyR r r0 Er)) as [r1 [Wk1 E1]].
      exists r1; split; [exact Wk1 |].
      exists r'', x'', p'', q''; split; [exact (TrR r1 rr r'' (SyR rr r1 E1) Er'') |].
      split; [exact (TrL x' x0' x'' Ex' Ex'') |].
      split; [exact HL'' | split; [exact HR'' | exact Hz'']].
    - intros r' l T.
      destruct (SiR r r0 Er l r' T) as [r0' [T0 Er']].
      destruct (C1 r0 q p l r0' HR (Ln x0 p HL) Hz T0) as [q' [HR' Hm]].
      destruct (G1 _ _ _ Hz Hm) as [w'' [W Hz'']].
      destruct (answer_left_upto invL S1 l x0 r0' p q' w'' HL HR' W)
        as [xr [x'' [r'' [p'' [q'' [-> [HL'' [HR'' [Wk [Ex'' Er'']]]]]]]]]].
      destruct (weak_eq ltsL EqL SiL x0 xr l Wk x (SyL x x0 Ex)) as [x1 [Wk1 E1]].
      exists x1; split; [exact Wk1 |].
      exists r'', x'', p'', q''; split; [exact (TrR r' r0' r'' Er' Er'') |].
      split; [exact (TrL x1 xr x'' (SyL xr x1 E1) Ex'') |].
      split; [exact HL'' | split; [exact HR'' | exact Hz'']].
  Qed.
End TransferUpto.

(** Normalisation soundness from [DdEnc]'s soundness of an answerer, the
    relation [fun x _ y => Eq x y] in the place of an LTS. *)
Lemma soundNL_of {M A : Type} (E : M -> list N -> Prop) (Eq : M -> M -> Prop)
  (ns : list (rel A)) :
  SoundL (fun x (_ : option A) y => Eq x y) E ns -> SoundNL E Eq ns.
Proof. intros H x p q w1 HE F; exact (H x p q None w1 HE F). Qed.

Lemma soundNR_of {M A : Type} (E : M -> list N -> Prop) (nL : nat) (Eq : M -> M -> Prop)
  (ns : list (rel A)) :
  SoundR (fun x (_ : option A) y => Eq x y) E nL ns -> SoundNR E nL Eq ns.
Proof. intros H r q p w1 HE Lp F; exact (H r q p None w1 HE Lp F). Qed.

(** The start's normalisation steps: a tuple of a relation that reads and
    writes every position of one part (logged backwards: the
    representative's values read, the state's written) *)
Fixpoint interleave (a b : list N) : list N :=
  match a, b with
  | x :: a', y :: b' => x :: y :: interleave a' b'
  | _, _ => []
  end.

Lemma rstep_rw : forall a b q, length a = length b ->
  rstep (repeat RW (length a)) (interleave a b) (a ++ q) (b ++ q).
Proof.
  induction a as [|x a IH]; intros [|y b] q H; simpl in *; try discriminate.
  - split; reflexivity.
  - exists (interleave a b); split; [reflexivity | apply IH; injection H; auto].
Qed.

Lemma rstep_crw : forall p a b, length a = length b ->
  rstep (repeat Copy (length p) ++ repeat RW (length a)) (interleave a b) (p ++ a) (p ++ b).
Proof.
  induction p as [|x p IH]; intros a b H; simpl.
  - pose proof (rstep_rw a b [] H) as R; rewrite !app_nil_r in R; exact R.
  - split; [reflexivity | exact (IH a b H)].
Qed.

Lemma normz_refl {A : Type} (nsL nsR : list (rel A)) (z : dd) (w : list N) :
  In_t z w -> NormZ nsL nsR z w.
Proof. intros H; exists w; split; [constructor | exact H]. Qed.

Lemma normz_l {A : Type} (nsL nsR : list (rel A)) (z : dd) cs r (a b q : list N) :
  In (None, cs, r) nsL -> In_t r (interleave a b) -> cs = repeat RW (length a) ->
  length a = length b -> NormZ nsL nsR z (a ++ q) -> NormZ nsL nsR z (b ++ q).
Proof.
  intros Hin Hr Hcs Hl [w' [Hn Hz]]; exists w'; split; [| exact Hz].
  econstructor; [left | exact Hn].
  exists cs, r; split; [exact Hin |].
  exists (interleave a b); split; [exact Hr | subst cs; exact (rstep_rw a b q Hl)].
Qed.

Lemma normz_r {A : Type} (nsL nsR : list (rel A)) (z : dd) cs r (p a b : list N) :
  In (None, cs, r) nsR -> In_t r (interleave a b) ->
  cs = repeat Copy (length p) ++ repeat RW (length a) ->
  length a = length b -> NormZ nsL nsR z (p ++ a) -> NormZ nsL nsR z (p ++ b).
Proof.
  intros Hin Hr Hcs Hl [w' [Hn Hz]]; exists w'; split; [| exact Hz].
  econstructor; [right | exact Hn].
  exists cs, r; split; [exact Hin |].
  exists (interleave a b); split; [exact Hr | subst cs; exact (rstep_crw p a b Hl)].
Qed.

(** The names the plugin builds proofs with ([src/lts_proof.ml]). *)
Register Cong as mebi.du.Cong.
Register cong_refl as mebi.du.cong_refl.
Register cong_step as mebi.du.cong_step.
Register cong_equiv as mebi.du.cong_equiv.
Register Equiv as mebi.du.Equiv.
Register SimEq as mebi.du.SimEq.
Register NStep as mebi.du.NStep.
Register TStep as mebi.du.TStep.
Register TausN as mebi.du.TausN.
Register ReachN as mebi.du.ReachN.
Register OkLN as mebi.du.OkLN.
Register MoveWN as mebi.du.MoveWN.
Register VWSimN as mebi.du.VWSimN.
Register NormZ as mebi.du.NormZ.
Register all_reachN_self as mebi.du.all_reachN_self.
Register all_back_reachN as mebi.du.all_back_reachN.
Register all_back_reachN_nl as mebi.du.all_back_reachN_nl.
Register all_back_reachN_nr as mebi.du.all_back_reachN_nr.
Register all_back_oklN as mebi.du.all_back_oklN.
Register movewN_l as mebi.du.movewN_l.
Register movewN_tau as mebi.du.movewN_tau.
Register vwsimN_of as mebi.du.vwsimN_of.
Register SoundNL as mebi.du.SoundNL.
Register SoundNR as mebi.du.SoundNR.
Register cert_weak_sim_upto as mebi.du.cert_weak_sim_upto.
Register cert_weak_bisim_upto as mebi.du.cert_weak_bisim_upto.
Register soundNL_of as mebi.du.soundNL_of.
Register soundNR_of as mebi.du.soundNR_of.
Register all_back_oklN_tau as mebi.du.all_back_oklN_tau.
Register all_back_oklN_nl as mebi.du.all_back_oklN_nl.
Register all_back_oklN_nr as mebi.du.all_back_oklN_nr.
Register interleave as mebi.du.interleave.
Register rstep_rw as mebi.du.rstep_rw.
Register rstep_crw as mebi.du.rstep_crw.
Register normz_refl as mebi.du.normz_refl.
Register normz_l as mebi.du.normz_l.
Register normz_r as mebi.du.normz_r.
