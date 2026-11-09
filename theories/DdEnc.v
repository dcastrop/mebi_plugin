(** * States as vectors: the generic part of a goal's encoding

    Proved once (doc/certificate.md, "The theorem from the certificate").
    A goal's proof gives the data (its value tries, its shapes, its
    relations' diagrams) and, per relation and per shape, the facts about
    its own LTS; these lemmas do the rest:
    - [val]: the value of a number, through a binary trie (the kernel
      follows one branch per binary digit); [tall_get]: an entry's
      property, from [TAll] of the trie, found the same way;
    - [enc]: a state's vector, through a trie of shapes (the vector's
      first number picks the shape's predicate);
    - [Reads], [post]: what a tuple reads of a vector, and the vector after
      it;
    - [swapt]: a tuple read backwards (the answerer's relations are logged
      backwards);
    - the code lists' properties as inductive predicates ([NoWO],
      [Within], [Shifted], [RoOnly]): each proved by its constructors;
    - the pair-vector lemmas: a step of the left system (its codes within
      its positions), or of the right one (after as many copies as the
      left has positions);
    - [AllT]: a property of every tuple of a diagram, one node at a time.
    Nothing here is decided by a boolean: every proof a goal gives is
    checked by typing. *)

From Stdlib Require Import List NArith Bool Lia Arith.
From MEBI Require Import Bisimilarity DdTree DdGame.
Import ListNotations.
Local Open Scope N_scope.

(** ** Values by number *)

Inductive trie (T : Type) : Type := tleaf | tnode (x : option T) (l r : trie T).
Arguments tleaf {T}.
Arguments tnode {T} x l r.

Fixpoint tfind {T} (p : positive) (t : trie T) {struct t} : option T :=
  match t with
  | tleaf => None
  | tnode x l r => match p with xH => x | xO p => tfind p l | xI p => tfind p r end
  end.

Definition val {T} (d : T) (t : trie T) (n : N) : T :=
  match n with N0 => d | Npos p => match tfind p t with Some x => x | None => d end end.

(** every entry of a trie, by its key *)
Fixpoint TAll {T} (Q : positive -> T -> Prop) (t : trie T) : Prop :=
  match t with
  | tleaf => True
  | tnode o l r =>
    match o with Some x => Q xH x | None => True end /\
    TAll (fun p => Q (xO p)) l /\ TAll (fun p => Q (xI p)) r
  end.

Lemma tall_find : forall T (t : trie T) Q p x, TAll Q t -> tfind p t = Some x -> Q p x.
Proof.
  intros T t; induction t as [|o l IHl r IHr]; intros Q p x H F; [simpl in F; discriminate|].
  destruct H as [Ho [Hl Hr]]; destruct p as [p|p|]; simpl in F.
  - exact (IHr (fun p => Q (xI p)) p x Hr F).
  - exact (IHl (fun p => Q (xO p)) p x Hl F).
  - subst o; exact Ho.
Qed.

(** the property of an entry, by its key (the entry found by [tfind], as a
    [val] is) *)
Lemma tall_get : forall T (t : trie T) Q p, TAll Q t ->
  match tfind p t with Some x => Q p x | None => True end.
Proof.
  intros T t Q p H; destruct (tfind p t) as [x|] eqn:F;
    [exact (tall_find _ t Q p x H F) | exact I].
Qed.

(** a property of every value a trie gives, the default's included *)
Lemma val_all : forall T (d : T) (tr : trie T) (Q : T -> Prop),
  Q d -> TAll (fun _ x => Q x) tr -> forall n, Q (val d tr n).
Proof.
  intros T d tr Q Hd Ht [|p]; simpl; [exact Hd|].
  destruct (tfind p tr) as [x|] eqn:F; [exact (tall_find _ tr _ p x Ht F) | exact Hd].
Qed.

(** ** States as vectors

    A system's encoding: a vector [s :: rest] encodes [x] when the shape
    numbered [s] says so of [x] and [rest] (its predicate, in a trie). *)

Definition enc {M} (Pt : trie (M -> list N -> Prop)) (x : M) (v : list N) : Prop :=
  match v with [] => False | s :: rest => val (fun _ _ => False) Pt s x rest end.

(** a property of every encoded state, one shape at a time *)
Lemma enc_ind : forall M (Pt : trie (M -> list N -> Prop)) (R : M -> list N -> Prop),
  TAll (fun p P => forall x rest, P x rest -> R x (Npos p :: rest)) Pt ->
  forall x v, enc Pt x v -> R x v.
Proof.
  intros M Pt R H x [|[|p] rest] E; simpl in E; try contradiction.
  destruct (tfind p Pt) as [P|] eqn:F; [|contradiction].
  exact (tall_find _ Pt _ p P H F x rest E).
Qed.

(** ** Code lists, by their constructors *)

Inductive NoWO : list code -> Prop :=
| nw_nil : NoWO []
| nw_copy cs : NoWO cs -> NoWO (Copy :: cs)
| nw_ro cs : NoWO cs -> NoWO (RO :: cs)
| nw_rw cs : NoWO cs -> NoWO (RW :: cs).

Inductive Within : list code -> nat -> Prop :=
| wi_nil n : Within [] n
| wi_cons c cs n : Within cs n -> Within (c :: cs) (S n).

Lemma within_le : forall cs n, Within cs n -> (length cs <= n)%nat.
Proof. intros cs n H; induction H; simpl; lia. Qed.

(** [Shifted n cs cs']: [cs'] is [cs] after [n] copies *)
Inductive Shifted : nat -> list code -> list code -> Prop :=
| sh_0 cs : Shifted 0 cs cs
| sh_S n cs cs' : Shifted n cs cs' -> Shifted (S n) cs (Copy :: cs').

Lemma shifted_eq : forall n cs cs', Shifted n cs cs' -> cs' = repeat Copy n ++ cs.
Proof. intros n cs cs' H; induction H; simpl; congruence. Qed.

Inductive RoOnly : list code -> Prop :=
| ro_nil : RoOnly []
| ro_copy cs : RoOnly cs -> RoOnly (Copy :: cs)
| ro_ro cs : RoOnly cs -> RoOnly (RO :: cs).

(** ** A tuple's step: what it reads, and the vector after it *)

Fixpoint Reads (cs : list code) (t x : list N) : Prop :=
  match cs, t, x with
  | [], [], _ => True
  | Copy :: cs, t, _ :: x => Reads cs t x
  | RO :: cs, v :: t, a :: x => a = v /\ Reads cs t x
  | RW :: cs, v :: _ :: t, a :: x => a = v /\ Reads cs t x
  | WO :: cs, _ :: t, _ :: x => Reads cs t x
  | _, _, _ => False
  end.

Fixpoint post (cs : list code) (t x : list N) : list N :=
  match cs, t, x with
  | [], _, x => x
  | Copy :: cs, t, a :: x => a :: post cs t x
  | RO :: cs, _ :: t, a :: x => a :: post cs t x
  | RW :: cs, _ :: w :: t, _ :: x => w :: post cs t x
  | WO :: cs, w :: t, _ :: x => w :: post cs t x
  | _, _, x => x
  end.

Lemma rstep_reads : forall cs t x y, rstep cs t x y -> Reads cs t x /\ y = post cs t x.
Proof.
  induction cs as [|c cs IH]; intros t x y H.
  - destruct H as [-> ->]; simpl; auto.
  - destruct x as [|a x], y as [|b y]; try (destruct c; contradiction).
    destruct c; simpl in H.
    + destruct H as [-> H]; destruct (IH t x y H) as [K ->]; simpl; auto.
    + destruct H as [-> [t' [-> H]]]; destruct (IH t' x y H) as [K ->]; simpl; auto.
    + destruct H as [t' [-> H]]; destruct (IH t' x y H) as [K ->]; simpl; auto.
    + destruct H as [t' [-> H]]; destruct (IH t' x y H) as [K ->]; simpl; auto.
Qed.

Lemma reads_rstep : forall cs t x, Reads cs t x -> rstep cs t x (post cs t x).
Proof.
  induction cs as [|c cs IH]; intros t x H.
  - destruct t; simpl in H; [split; reflexivity | contradiction].
  - destruct c.
    + destruct x as [|a x]; simpl in H; [contradiction|].
      simpl; split; [reflexivity | apply IH; exact H].
    + destruct t as [|v t], x as [|a x]; simpl in H; try contradiction.
      destruct H as [-> H]; simpl; split; [reflexivity|].
      exists t; split; [reflexivity | apply IH; exact H].
    + destruct t as [|w t], x as [|a x]; simpl in H; try contradiction.
      simpl; exists t; split; [reflexivity | apply IH; exact H].
    + destruct t as [|v [|w t]], x as [|a x]; simpl in H; try contradiction.
      destruct H as [-> H]; simpl; exists t; split; [reflexivity | apply IH; exact H].
Qed.

(** ** A tuple read backwards *)

Fixpoint swapt (cs : list code) (t : list N) : list N :=
  match cs, t with
  | Copy :: cs, t => swapt cs t
  | RO :: cs, v :: t => v :: swapt cs t
  | RW :: cs, a :: b :: t => b :: a :: swapt cs t
  | _, t => t
  end.

Lemma rstep_swap : forall cs t x y, NoWO cs -> rstep cs t y x -> rstep cs (swapt cs t) x y.
Proof.
  intros cs t x y N; revert t x y; induction N as [|cs N IH|cs N IH|cs N IH]; intros t x y H.
  - destruct H as [-> ->]; split; reflexivity.
  - destruct x as [|a x], y as [|b y]; simpl in H |- *; try contradiction.
    destruct H as [-> H]; split; [reflexivity | apply IH; exact H].
  - destruct x as [|a x], y as [|b y]; simpl in H |- *; try contradiction.
    destruct H as [-> [t' [-> H]]]; split; [reflexivity|].
    exists (swapt cs t'); split; [reflexivity | apply IH; exact H].
  - destruct x as [|a x], y as [|b y]; simpl in H |- *; try contradiction.
    destruct H as [t' [-> H]]; exists (swapt cs t'); split; [reflexivity | apply IH; exact H].
Qed.

(** ** Pair vectors *)

Lemma rstep_len : forall cs t x y, rstep cs t x y -> length y = length x.
Proof.
  induction cs as [|c cs IH]; intros t x y H.
  - destruct H as [_ ->]; reflexivity.
  - destruct x as [|a x], y as [|b y]; try (destruct c; contradiction).
    destruct c; simpl in H |- *; f_equal.
    + destruct H as [_ H]; eauto.
    + destruct H as [_ [t' [_ H]]]; eauto.
    + destruct H as [t' [_ H]]; eauto.
    + destruct H as [t' [_ H]]; eauto.
Qed.

(** the left system's step: its codes within its positions *)
Lemma rstep_left : forall cs t p q y, (length cs <= length p)%nat -> rstep cs t (p ++ q) y ->
  exists p', y = p' ++ q /\ rstep cs t p p'.
Proof.
  induction cs as [|c cs IH]; intros t p q y L H.
  - destruct H as [-> <-]; exists p; split; [reflexivity | split; reflexivity].
  - destruct p as [|a p]; [simpl in L; lia|]; simpl in L.
    destruct y as [|b y]; [destruct c; contradiction|].
    destruct c; simpl in H.
    + destruct H as [-> H]; destruct (IH t p q y ltac:(lia) H) as [p' [-> K]].
      exists (b :: p'); split; [reflexivity | split; auto].
    + destruct H as [-> [t' [-> H]]]; destruct (IH t' p q y ltac:(lia) H) as [p' [-> K]].
      exists (b :: p'); split; [reflexivity | split; [reflexivity | exists t'; auto]].
    + destruct H as [t' [-> H]]; destruct (IH t' p q y ltac:(lia) H) as [p' [-> K]].
      exists (b :: p'); split; [reflexivity | exists t'; auto].
    + destruct H as [t' [-> H]]; destruct (IH t' p q y ltac:(lia) H) as [p' [-> K]].
      exists (b :: p'); split; [reflexivity | exists t'; auto].
Qed.

Lemma rstep_left_r : forall cs t p p' q, (length cs <= length p)%nat -> rstep cs t p p' ->
  rstep cs t (p ++ q) (p' ++ q).
Proof.
  induction cs as [|c cs IH]; intros t p p' q L H.
  - destruct H as [-> ->]; split; reflexivity.
  - destruct p as [|a p]; [simpl in L; lia|]; simpl in L.
    destruct p' as [|b p']; [destruct c; contradiction|].
    destruct c; simpl in H |- *.
    + destruct H as [-> H]; split; [reflexivity | apply IH; [lia | exact H]].
    + destruct H as [-> [t' [-> H]]]; split; [reflexivity|]; exists t'; split; [reflexivity | apply IH; [lia | exact H]].
    + destruct H as [t' [-> H]]; exists t'; split; [reflexivity | apply IH; [lia | exact H]].
    + destruct H as [t' [-> H]]; exists t'; split; [reflexivity | apply IH; [lia | exact H]].
Qed.

(** the right system's step: its codes after [length p] copies *)
Lemma rstep_right : forall cs t p q y, rstep (repeat Copy (length p) ++ cs) t (p ++ q) y ->
  exists q', y = p ++ q' /\ rstep cs t q q'.
Proof.
  intros cs t p; induction p as [|a p IH]; intros q y H.
  - exists y; auto.
  - destruct y as [|b y]; [contradiction|]; destruct H as [-> H].
    destruct (IH q y H) as [q' [-> K]]; exists q'; auto.
Qed.

Lemma rstep_right_r : forall cs t p q q', rstep cs t q q' ->
  rstep (repeat Copy (length p) ++ cs) t (p ++ q) (p ++ q').
Proof.
  intros cs t p; induction p as [|a p IH]; intros q q' H; [exact H|].
  split; [reflexivity | apply IH; exact H].
Qed.

(** ** Every tuple of a diagram *)

Definition AllT (r : dd) (Q : list N -> Prop) : Prop := forall t, In_t r t -> Q t.

Lemma allt_0 : forall Q, AllT D0 Q.
Proof. intros Q t []. Qed.

Lemma allt_1 : forall Q, Q [] -> AllT D1 Q.
Proof. intros Q H t E; simpl in E; subst; exact H. Qed.

Lemma allt_nd : forall v d r Q, AllT d (fun t => Q (v :: t)) -> AllT r Q -> AllT (Nd v d r) Q.
Proof.
  intros v d r Q Hd Hr t H; apply in_nd in H as [[t' [-> H]]|H]; [exact (Hd t' H) | exact (Hr t H)].
Qed.

(** ** Read-only relations: a summand's learned projections *)

(** the values at the read positions *)
Fixpoint projv (cs : list code) (x : list N) : list N :=
  match cs, x with
  | Copy :: cs, _ :: x => projv cs x
  | RO :: cs, a :: x => a :: projv cs x
  | _, _ => []
  end.

Lemma rstep_ro : forall cs t a b, RoOnly cs -> rstep cs t a b -> a = b /\ t = projv cs b.
Proof.
  intros cs t a b R; revert t a b; induction R as [|cs R IH|cs R IH]; intros t a b H.
  - destruct H as [-> ->]; auto.
  - destruct a as [|x a], b as [|y b]; simpl in H; try contradiction.
    destruct H as [-> H]; destruct (IH t a b H) as [-> ->]; auto.
  - destruct a as [|x a], b as [|y b]; simpl in H; try contradiction.
    destruct H as [-> [t' [-> H]]]; destruct (IH t' a b H) as [-> ->]; auto.
Qed.

Lemma projv_copies : forall n cs p q, length p = n -> projv (repeat Copy n ++ cs) (p ++ q) = projv cs q.
Proof. intros n cs p q <-; induction p as [|a p IH]; [reflexivity | exact IH]. Qed.

Lemma roonly_copies : forall n cs, RoOnly cs -> RoOnly (repeat Copy n ++ cs).
Proof. induction n as [|n IH]; intros cs H; [exact H | constructor; exact (IH cs H)]. Qed.

(** coverage: a pair of [z] whose right state has shape [sid] has its
    read projection in [D] (the projections a summand of the shape
    learned): [F], the image of [z] by the shape's filter [sh], is
    complete; [G], its image by [D] read only, is sound; [F] within [G] *)
Lemma cov_of : forall nL z sid sh csh kc ckc D F G,
  Srt z -> Srt sh -> sh = Nd sid D1 D0 -> Shifted nL [RO] csh -> Shifted nL kc ckc ->
  PostC csh z sh F -> PostS ckc z D G -> Mi F G D0 -> RoOnly kc ->
  forall p rest, length p = nL -> In_t z (p ++ sid :: rest) -> In_t D (projv kc (sid :: rest)).
Proof.
  intros nL z sid sh csh kc ckc D F G Sz Ssh Esh Hsh Hkc HF HG HM R p rest Lp Hz.
  apply shifted_eq in Hsh, Hkc; subst csh ckc.
  assert (Hq : rstep [RO] [sid] (sid :: rest) (sid :: rest)).
  { split; [reflexivity|]; exists []; split; [reflexivity | split; reflexivity]. }
  pose proof (rstep_right_r [RO] [sid] p _ _ Hq) as Hs; rewrite Lp in Hs.
  assert (Hsh : In_t sh [sid]) by (subst; left; split; reflexivity).
  pose proof (HF Sz Ssh [sid] (p ++ sid :: rest) (p ++ sid :: rest) Hz Hsh Hs) as InF.
  pose proof (mi_empty F G HM _ InF) as InG.
  destruct (HG _ InG) as [t [a [Ha [Ht Hst]]]].
  destruct (rstep_ro _ t a _ (roonly_copies nL kc R) Hst) as [_ ->].
  rewrite (projv_copies nL kc p _ Lp) in Ht; exact Ht.
Qed.

(** coverage for the left system (its positions first) *)
Lemma cov_of_l : forall z sid sh kc D F G,
  Srt z -> Srt sh -> sh = Nd sid D1 D0 ->
  PostC [RO] z sh F -> PostS kc z D G -> Mi F G D0 -> RoOnly kc ->
  forall rest q, In_t z ((sid :: rest) ++ q) -> In_t D (projv kc ((sid :: rest) ++ q)).
Proof.
  intros z sid sh kc D F G Sz Ssh Esh HF HG HM R rest q Hz.
  assert (Hq : rstep [RO] [sid] ((sid :: rest) ++ q) ((sid :: rest) ++ q)).
  { split; [reflexivity|]; exists []; split; [reflexivity | split; reflexivity]. }
  assert (Hsh : In_t sh [sid]) by (subst; left; split; reflexivity).
  pose proof (HF Sz Ssh [sid] _ _ Hz Hsh Hq) as InF.
  pose proof (mi_empty F G HM _ InF) as InG.
  destruct (HG _ InG) as [t [a [Ha [Ht Hst]]]].
  destruct (rstep_ro _ t a _ R Hst) as [_ ->]; exact Ht.
Qed.

(** ** Lists of numbers *)

Lemma cons_inj : forall (a b : N) x y, a :: x = b :: y -> a = b /\ x = y.
Proof. intros a b x y H; injection H; auto. Qed.

(** ** The answerer's tuples: each a step

    [TSound lts E l cs t]: the tuple [t] of a relation (codes [cs], label
    [l]), logged backwards, is a step of [lts] from every state it
    applies to. A relation's tuples share a source shape (read at
    position 0); [tsound_ro] and [tsound_rw] take the vector's shape from
    the tuple, [tsound_tmpl] its slots from the shape's template. *)

Section Sound.
  Context {M A : Type} (lts : LTS M A).

  Definition TSound (E : M -> list N -> Prop) (l : option A) (cs : list code) (t : list N) : Prop :=
    forall x p p', E x p -> rstep cs (swapt cs t) p p' -> exists x', E x' p' /\ lts x l x'.

  Variable Pt : trie (M -> list N -> Prop).

  Lemma tsound_ro : forall l cs t sid,
    (forall x rest rest', val (fun _ _ => False) Pt sid x rest -> rstep cs (swapt cs t) rest rest' ->
       exists x', enc Pt x' (sid :: rest') /\ lts x l x') ->
    TSound (enc Pt) l (RO :: cs) (sid :: t).
  Proof.
    intros l cs t sid H x [|a p] [|b p'] HE Hs; simpl in Hs; try contradiction.
    destruct Hs as [<- [t' [E Hs]]]; injection E as E1 E2; subst.
    exact (H x p p' HE Hs).
  Qed.

  Lemma tsound_rw : forall l cs t sid sid',
    (forall x rest rest', val (fun _ _ => False) Pt sid x rest -> rstep cs (swapt cs t) rest rest' ->
       exists x', enc Pt x' (sid' :: rest') /\ lts x l x') ->
    TSound (enc Pt) l (RW :: cs) (sid' :: sid :: t).
  Proof.
    intros l cs t sid sid' H x [|a p] [|b p'] HE Hs; simpl in Hs; try contradiction.
    destruct Hs as [t' [E Hs]]; injection E as E1 E2 E3; subst.
    exact (H x p p' HE Hs).
  Qed.

  Lemma tsound_tmpl : forall (x y : M) (rest rest' tmpl : list N) cs t (G : M -> list N -> Prop),
    rest = tmpl -> x = y -> rstep cs t rest rest' -> (Reads cs t tmpl -> G y (post cs t tmpl)) ->
    G x rest'.
  Proof.
    intros x y rest rest' tmpl cs t G -> -> H K.
    destruct (rstep_reads cs t tmpl rest' H) as [R ->]; exact (K R).
  Qed.
End Sound.

(** ** From one fact per relation to the transfer's hypotheses *)

Section Glue.
  Context {ML MR A : Type} (ltsL : LTS ML A) (ltsR : LTS MR A).
  Variables (EL : ML -> list N -> Prop) (ER : MR -> list N -> Prop) (nL : nat).

  (** the left system answers: each tuple of its relations backwards is
      one of its steps *)
  Definition InvOkL (e : rel A) : Prop :=
    let '(l, cs, r) := e in NoWO cs /\ Within cs nL /\ AllT r (TSound ltsL EL l cs).

  Lemma soundL_of : forall inv, LenL EL nL -> Forall InvOkL inv -> SoundL ltsL EL inv.
  Proof.
    intros inv Len HF x p q l w1 HE [cs [r [Hin [t [Hr Hs]]]]].
    rewrite Forall_forall in HF; destruct (HF _ Hin) as [Nw [Wi HA]].
    pose proof (rstep_swap cs t (p ++ q) w1 Nw Hs) as Hs'.
    pose proof (within_le cs nL Wi) as Lc; rewrite <- (Len x p HE) in Lc.
    destruct (rstep_left cs (swapt cs t) p q w1 Lc Hs') as [p' [-> Hp]].
    destruct (HA t Hr x p p' HE Hp) as [x' [HE' St]].
    exists x', p'; auto.
  Qed.

  (** the right system answers (its codes after [nL] copies) *)
  Definition InvOkR (e : rel A) : Prop :=
    let '(l, cs', r) := e in
    exists cs, Shifted nL cs cs' /\ NoWO cs /\ AllT r (TSound ltsR ER l cs).

  Lemma swapt_copies : forall n cs t, swapt (repeat Copy n ++ cs) t = swapt cs t.
  Proof. induction n as [|n IH]; intros cs t; [reflexivity | exact (IH cs t)]. Qed.

  Lemma nowo_copies : forall n cs, NoWO cs -> NoWO (repeat Copy n ++ cs).
  Proof. induction n as [|n IH]; intros cs H; [exact H | constructor; exact (IH cs H)]. Qed.

  Lemma soundR_of : forall inv, Forall InvOkR inv -> SoundR ltsR ER nL inv.
  Proof.
    intros inv HF r q p l w1 HE Lp [cs' [rr [Hin [t [Hr Hs]]]]].
    rewrite Forall_forall in HF; destruct (HF _ Hin) as [cs [Sh [Nw HA]]].
    apply shifted_eq in Sh; subst cs'.
    pose proof (rstep_swap _ t (p ++ q) w1 (nowo_copies nL cs Nw) Hs) as Hs'.
    rewrite swapt_copies, <- Lp in Hs'.
    destruct (rstep_right cs (swapt cs t) p q w1 Hs') as [q' [-> Hq]].
    destruct (HA t Hr r q q' HE Hq) as [x' [HE' St]].
    exists x', q'; auto.
  Qed.

  (** the right system moves: each of its steps from a state of [z] is a
      tuple of one of [rs]'s relations *)
  Definition TabInR (rs : list (rel A)) (l : option A) (cs : list code) (t : list N) : Prop :=
    exists cs' rr, Shifted nL cs cs' /\ In (l, cs', rr) rs /\ In_t rr t.

  Definition StepInR (rs : list (rel A)) (l : option A) (q : list N) (r' : MR) : Prop :=
    exists cs t q', TabInR rs l cs t /\ rstep cs t q q' /\ ER r' q'.

  Lemma stepinR_of : forall rs l cs cs' rr t q r', Shifted nL cs cs' -> In (l, cs', rr) rs ->
    In_t rr t -> Reads cs t q -> ER r' (post cs t q) -> StepInR rs l q r'.
  Proof.
    intros rs l cs cs' rr t q r' Sh Hin Ht R HE.
    exists cs, t, (post cs t q); split; [exists cs', rr; auto | split; [apply reads_rstep; exact R | exact HE]].
  Qed.

  Definition CompleteTabR (z : dd) (rs : list (rel A)) : Prop :=
    forall r q p l r', ER r q -> length p = nL -> In_t z (p ++ q) -> ltsR r l r' -> StepInR rs l q r'.

  Lemma completeR_of : forall z rs, CompleteTabR z rs -> CompleteR ltsR ER z nL rs.
  Proof.
    intros z rs C r q p l r' HR Hl Hz T.
    destruct (C r q p l r' HR Hl Hz T) as [cs [t [q' [[cs' [rr [Sh [Hin Hr]]]] [Hs HR']]]]].
    apply shifted_eq in Sh; subst cs'.
    exists q'; split; [exact HR'|].
    exists (repeat Copy nL ++ cs), rr; split; [exact Hin|]; exists t; split; [exact Hr|].
    rewrite <- Hl; apply rstep_right_r; exact Hs.
  Qed.

  (** the left system moves (its codes within its positions) *)
  Definition TabInL (rs : list (rel A)) (l : option A) (cs : list code) (t : list N) : Prop :=
    exists rr, Within cs nL /\ In (l, cs, rr) rs /\ In_t rr t.

  Definition StepInL (rs : list (rel A)) (l : option A) (p : list N) (x' : ML) : Prop :=
    exists cs t p', TabInL rs l cs t /\ rstep cs t p p' /\ EL x' p'.

  Lemma stepinL_of : forall rs l cs rr t p x', Within cs nL -> In (l, cs, rr) rs ->
    In_t rr t -> Reads cs t p -> EL x' (post cs t p) -> StepInL rs l p x'.
  Proof.
    intros rs l cs rr t p x' Wi Hin Ht R HE.
    exists cs, t, (post cs t p); split; [exists rr; auto | split; [apply reads_rstep; exact R | exact HE]].
  Qed.

  Definition CompleteTabL (z : dd) (rs : list (rel A)) : Prop :=
    forall x p q l x', EL x p -> In_t z (p ++ q) -> ltsL x l x' -> StepInL rs l p x'.

  Lemma completeL_of : forall z rs, LenL EL nL -> CompleteTabL z rs -> CompleteL ltsL EL z rs.
  Proof.
    intros z rs Len C x p q l x' HE Hz T.
    destruct (C x p q l x' HE Hz T) as [cs [t [p' [[rr [Wi [Hin Hr]]] [Hs HE']]]]].
    exists p'; split; [exact HE'|].
    exists cs, rr; split; [exact Hin|]; exists t; split; [exact Hr|].
    apply rstep_left_r; [|exact Hs].
    rewrite (Len x p HE); exact (within_le cs nL Wi).
  Qed.
End Glue.

Register trie as mebi.de.trie.
Register tleaf as mebi.de.tleaf.
Register tnode as mebi.de.tnode.
Register val as mebi.de.val.
Register TAll as mebi.de.TAll.
Register enc as mebi.de.enc.
Register enc_ind as mebi.de.enc_ind.
Register NoWO as mebi.de.NoWO.
Register nw_nil as mebi.de.nw_nil.
Register nw_copy as mebi.de.nw_copy.
Register nw_ro as mebi.de.nw_ro.
Register nw_rw as mebi.de.nw_rw.
Register Within as mebi.de.Within.
Register wi_nil as mebi.de.wi_nil.
Register wi_cons as mebi.de.wi_cons.
Register Shifted as mebi.de.Shifted.
Register sh_0 as mebi.de.sh_0.
Register sh_S as mebi.de.sh_S.
Register RoOnly as mebi.de.RoOnly.
Register ro_nil as mebi.de.ro_nil.
Register ro_copy as mebi.de.ro_copy.
Register ro_ro as mebi.de.ro_ro.
Register Reads as mebi.de.Reads.
Register post as mebi.de.post.
Register reads_rstep as mebi.de.reads_rstep.
Register swapt as mebi.de.swapt.
Register AllT as mebi.de.AllT.
Register allt_0 as mebi.de.allt_0.
Register allt_1 as mebi.de.allt_1.
Register allt_nd as mebi.de.allt_nd.
Register projv as mebi.de.projv.
Register cov_of as mebi.de.cov_of.
Register val_all as mebi.de.val_all.
Register tall_get as mebi.de.tall_get.
Register cov_of_l as mebi.de.cov_of_l.
Register cons_inj as mebi.de.cons_inj.
Register TSound as mebi.de.TSound.
Register tsound_ro as mebi.de.tsound_ro.
Register tsound_rw as mebi.de.tsound_rw.
Register tsound_tmpl as mebi.de.tsound_tmpl.
Register InvOkL as mebi.de.InvOkL.
Register soundL_of as mebi.de.soundL_of.
Register InvOkR as mebi.de.InvOkR.
Register soundR_of as mebi.de.soundR_of.
Register TabInR as mebi.de.TabInR.
Register StepInR as mebi.de.StepInR.
Register stepinR_of as mebi.de.stepinR_of.
Register CompleteTabR as mebi.de.CompleteTabR.
Register completeR_of as mebi.de.completeR_of.
Register TabInL as mebi.de.TabInL.
Register StepInL as mebi.de.StepInL.
Register stepinL_of as mebi.de.stepinL_of.
Register CompleteTabL as mebi.de.CompleteTabL.
Register completeL_of as mebi.de.completeL_of.
Register app as mebi.de.app.

(** ** Rewriting, for the per-goal proofs *)

Lemma eq_rew : forall (T : Type) (P : T -> Prop) (x y : T), x = y -> P y -> P x.
Proof. intros T P x y -> H; exact H. Qed.

From Stdlib Require Import DecidableClass.
Register eq_rew as mebi.de.eq_rew.
Register eq_sym as mebi.de.eq_sym.
Register False_ind as mebi.de.False_ind.
Register conj as mebi.de.conj.
Register or_introl as mebi.de.or_introl.
Register or_intror as mebi.de.or_intror.
Register length as mebi.de.length.
Register f_equal as mebi.de.f_equal.
Register list as mebi.de.list.
Register Decidable_sound as mebi.de.Decidable_sound.
Register Decidable as mebi.de.Decidable.
Register Decidable_complete_alt as mebi.de.Decidable_complete_alt.
