(** * Decision diagrams as trees, and one lemma per case of each of
    Sylvan's operations: the facts of a certificate (doc/certificate.md)

    A diagram is a term of [dd]: [D0] holds no vector, [D1] the empty one,
    [Nd v d r] the vectors [v :: w] for [w] in [d] and the vectors of [r].
    The engine's nodes become [let]s inside one chunk, shared as in the
    engine's table.

    A certificate is a proof term built from the lemmas below: one lemma
    per case of an operation (union, difference, the image through a
    relation), applied once per step the engine took. The kernel checks a
    step by unfolding a node and, at most, comparing two numbers: no
    table, no memo, no fixpoint, no search. Each fact states only the
    direction the game's check needs: an image is complete (every step
    lands in it: the moves to answer) or sound (every vector in it is a
    step's: the answers), a difference both. Only a complete image needs
    sorted inputs ([Srt]): it skips a branch when the values cannot
    meet. *)

From Stdlib Require Import List Bool NArith Lia.
Import ListNotations.
Local Open Scope N_scope.

Inductive dd : Type := D0 | D1 | Nd (v : N) (d r : dd).

Fixpoint In_t (x : dd) (w : list N) {struct x} : Prop :=
  match x with
  | D0 => False
  | D1 => w = []
  | Nd v d r => (match w with [] => False | a :: w' => a = v /\ In_t d w' end) \/ In_t r w
  end.

Lemma in1 : In_t D1 [].
Proof. reflexivity. Qed.

Lemma in_d : forall v d r w, In_t d w -> In_t (Nd v d r) (v :: w).
Proof. intros v d r w H; left; auto. Qed.

Lemma in_r : forall v d r w, In_t r w -> In_t (Nd v d r) w.
Proof. intros v d r w H; right; exact H. Qed.

(** [y] is [x], or a node along [x]'s right children: the scan of a list
    of siblings, which every vector scanning it shares (its facts do not
    name the vector) *)
Inductive Sib (x : dd) : dd -> Prop :=
| sib_refl : Sib x x
| sib_step : forall v d r, Sib x (Nd v d r) -> Sib x r.

Lemma sib_in : forall x y, Sib x y -> forall w, In_t y w -> In_t x w.
Proof.
  intros x y S; induction S as [|v d r S IH]; intros w H; [exact H|].
  apply IH; right; exact H.
Qed.

(** a vector of [x], by the sibling holding its first value: one fact per
    level of the vector *)
Lemma in_sib_d : forall x v d r w, Sib x (Nd v d r) -> In_t d w -> In_t x (v :: w).
Proof. intros x v d r w S H; apply (sib_in x (Nd v d r) S); left; auto. Qed.

Lemma in_nd : forall v d r w, In_t (Nd v d r) w <->
  (exists w', w = v :: w' /\ In_t d w') \/ In_t r w.
Proof.
  intros v d r [|a w]; simpl; split.
  - intros [[]|H]; auto.
  - intros [[w' [E _]]|H]; [discriminate | auto].
  - intros [[-> H]|H]; [left; exists w; auto | auto].
  - intros [[w' [E H]]|H]; [injection E as -> ->; auto | auto].
Qed.

(** ** Relations on vectors

    A relation has one code per position: [Copy] (not touched), [RO]
    (read and kept), [WO] (written), [RW] (read and written). Its diagram
    holds tuples: per position, the old value ([RO], [RW]) and then the new
    one ([WO], [RW]). [rstep cs t x y]: the tuple [t] takes [x] to [y].
    The positions after the last code are carried unchanged, as past the
    end of Sylvan's meta: a relation's codes stop at its last touched
    position, and an image there is the set itself, with no step per
    node. *)

Inductive code := Copy | RO | WO | RW.

Fixpoint rstep (cs : list code) (t x y : list N) : Prop :=
  match cs, x, y with
  | [], _, _ => t = [] /\ x = y
  | Copy :: cs, a :: x, b :: y => a = b /\ rstep cs t x y
  | RO :: cs, a :: x, b :: y => a = b /\ exists t', t = a :: t' /\ rstep cs t' x y
  | WO :: cs, _ :: x, b :: y => exists t', t = b :: t' /\ rstep cs t' x y
  | RW :: cs, a :: x, b :: y => exists t', t = a :: b :: t' /\ rstep cs t' x y
  | _, _, _ => False
  end.

Lemma rstep_nil : forall t x y, rstep [] t x y -> t = [] /\ x = y.
Proof. intros t x y H; exact H. Qed.

Lemma rstep_cons_l : forall c cs t x y, rstep (c :: cs) t x y -> exists a x', x = a :: x'.
Proof. intros c cs t [|a x] [|b y] H; destruct c; simpl in H; try contradiction; eauto. Qed.

(** the vectors whose entries are below the domain sizes [ks] *)
Inductive InDom : list nat -> list N -> Prop :=
| dom_nil : InDom [] []
| dom_cons : forall k ks a w, a < N.of_nat k -> InDom ks w -> InDom (k :: ks) (a :: w).

Lemma InDom_len : forall ks w, InDom ks w -> length w = length ks.
Proof. intros ks w H; induction H; simpl; lia. Qed.

(** ** Order: values increase along [r] *)

Definition Above (v : N) (x : dd) : Prop := forall a w, In_t x (a :: w) -> v < a.

(** the value of [x]'s first node, if it has one, is above [v]: the order
    of a right chain node by node, which [Above] gives only through
    vectors (a node whose child is empty has none). *)
Definition Lead (v : N) (x : dd) : Prop :=
  match x with Nd v' _ _ => v < v' | _ => True end.

Fixpoint Srt (x : dd) : Prop :=
  match x with D0 | D1 => True | Nd v d r => Srt d /\ Srt r /\ Above v r /\ Lead v r end.

Lemma srt_0 : Srt D0.
Proof. exact I. Qed.

Lemma srt_1 : Srt D1.
Proof. exact I. Qed.

Lemma srt_nd : forall v d r, Srt d -> Srt r -> Above v r -> Lead v r -> Srt (Nd v d r).
Proof. intros v d r Hd Hr A Ld; simpl; auto. Qed.

Lemma above_0 : forall v, Above v D0.
Proof. intros v a w []. Qed.

Lemma above_le : forall v d r, Srt (Nd v d r) -> forall a w, In_t (Nd v d r) (a :: w) -> v <= a.
Proof.
  intros v d r [_ [_ [A _]]] a w [[-> _]|H]; [lia|]; pose proof (A a w H); lia.
Qed.

Lemma above_nd : forall v v' d r, v < v' -> Srt (Nd v' d r) -> Above v (Nd v' d r).
Proof. intros v v' d r L S a w H; pose proof (above_le v' d r S a w H); lia. Qed.

(** a node's order in one step: no right sibling, or one with a larger value *)
Lemma srt_nd0 : forall v d, Srt d -> Srt (Nd v d D0).
Proof. intros v d H; apply srt_nd; [exact H | exact srt_0 | apply above_0 | exact I]. Qed.

Lemma srt_ndn : forall v d v' d' r', v < v' -> Srt d -> Srt (Nd v' d' r') -> Srt (Nd v d (Nd v' d' r')).
Proof. intros v d v' d' r' L Hd Hr; apply srt_nd; [exact Hd | exact Hr | apply above_nd; assumption | exact L]. Qed.

(** ** Union: [c] holds exactly the vectors of [a] and of [b] *)

Definition U (a b c : dd) : Prop := forall w, In_t c w <-> In_t a w \/ In_t b w.

Lemma u_a0 : forall b, U D0 b b.
Proof. intros b w; simpl; tauto. Qed.

Lemma u_b0 : forall a, U a D0 a.
Proof. intros a w; simpl; tauto. Qed.

Lemma u_same : forall a, U a a a.
Proof. intros a w; tauto. Qed.

Lemma u_lt : forall va da ra b R, U ra b R -> U (Nd va da ra) b (Nd va da R).
Proof. intros va da ra b R H w; rewrite !in_nd, (H w); tauto. Qed.

Lemma u_gt : forall a vb db rb R, U a rb R -> U a (Nd vb db rb) (Nd vb db R).
Proof. intros a vb db rb R H w; rewrite !in_nd, (H w); tauto. Qed.

Lemma u_eq : forall v da ra db rb D R, U da db D -> U ra rb R -> U (Nd v da ra) (Nd v db rb) (Nd v D R).
Proof.
  intros v da ra db rb D R HD HR w; rewrite !in_nd, (HR w); split.
  - intros [[w' [-> H]]|H]; [apply HD in H as [H|H]; [left; left | right; left]; exists w'; auto | tauto].
  - intros [[[w' [-> H]]|H]|[[w' [-> H]]|H]]; try tauto; left; exists w'; split; auto; apply HD; auto.
Qed.

(** Sylvan orders a union's arguments by node index before its cache
    (doc/certificate.md, A): the facts from its log need both orders *)
Lemma u_comm : forall a b c, U a b c -> U b a c.
Proof. intros a b c H w; rewrite (H w); tauto. Qed.

Lemma u_eq0 : forall v da ra db rb R, U da db D0 -> U ra rb R -> U (Nd v da ra) (Nd v db rb) R.
Proof.
  intros v da ra db rb R HD HR w; rewrite !in_nd, (HR w); split; [tauto|].
  intros [[[w' [-> H]]|H]|[[w' [-> H]]|H]]; try tauto; exfalso; apply (HD w'); auto.
Qed.

(** A union's last steps in one fact each (fewer facts to check): a node
    with no right sibling against the other set, and equal values whose
    children or right siblings are the same node *)
Lemma u_lt0 : forall va da b, U (Nd va da D0) b (Nd va da b).
Proof. intros; apply u_lt, u_a0. Qed.

Lemma u_gt0 : forall a vb db, U a (Nd vb db D0) (Nd vb db a).
Proof. intros; apply u_gt, u_b0. Qed.

Lemma u_eq_d : forall v d ra rb R, U ra rb R -> U (Nd v d ra) (Nd v d rb) (Nd v d R).
Proof. intros; apply u_eq; [apply u_same | assumption]. Qed.

Lemma u_eq_r : forall v da db r D, U da db D -> U (Nd v da r) (Nd v db r) (Nd v D r).
Proof. intros; apply u_eq; [assumption | apply u_same]. Qed.

(** ** Difference: [R] is within [a], and [a] within [b] and [R] *)

Definition Mi (a b R : dd) : Prop :=
  (forall w, In_t R w -> In_t a w) /\ (forall w, In_t a w -> In_t b w \/ In_t R w).

Lemma mi_id : forall a b, Mi a b a.
Proof. intros a b; split; auto. Qed.

Lemma mi_a0 : forall b, Mi D0 b D0.
Proof. intros b; split; intros w []. Qed.

Lemma mi_same : forall a, Mi a a D0.
Proof. intros a; split; [intros w []|auto]. Qed.

Lemma mi_lt : forall va da ra b R, Mi ra b R -> Mi (Nd va da ra) b (Nd va da R).
Proof.
  intros va da ra b R [H1 H2]; split; intros w; rewrite !in_nd.
  - intros [H|H]; [left; exact H | right; apply H1; exact H].
  - intros [H|H]; [right; left; exact H|]; destruct (H2 w H); auto.
Qed.

Lemma mi_gt : forall a vb db rb R, Mi a rb R -> Mi a (Nd vb db rb) R.
Proof.
  intros a vb db rb R [H1 H2]; split; [exact H1|]; intros w H.
  destruct (H2 w H) as [K|K]; [left; apply in_nd; right; exact K | right; exact K].
Qed.

Lemma mi_eq : forall v da ra db rb D R, Mi da db D -> Mi ra rb R -> Mi (Nd v da ra) (Nd v db rb) (Nd v D R).
Proof.
  intros v da ra db rb D R [D1' D2'] [R1 R2]; split; intros w; rewrite !in_nd.
  - intros [[w' [-> H]]|H]; [left; exists w'; auto | right; auto].
  - intros [[w' [-> H]]|H].
    + destruct (D2' w' H); [left; left | right; left]; exists w'; auto.
    + destruct (R2 w H); auto.
Qed.

Lemma mi_eq0 : forall v da ra db rb R, Mi da db D0 -> Mi ra rb R -> Mi (Nd v da ra) (Nd v db rb) R.
Proof.
  intros v da ra db rb R [_ D2'] [R1 R2]; split.
  - intros w H; apply in_nd; right; auto.
  - intros w H; apply in_nd in H as [[w' [-> H]]|H].
    + destruct (D2' w' H) as [K|[]]; left; apply in_nd; left; exists w'; auto.
    + destruct (R2 w H) as [K|K]; [left; apply in_nd; right; exact K | right; exact K].
Qed.

(** equal values and the same child: the difference is the right
    siblings' *)
Lemma mi_eq0d : forall v d ra rb R, Mi ra rb R -> Mi (Nd v d ra) (Nd v d rb) R.
Proof. intros; apply mi_eq0; [apply mi_same | assumption]. Qed.

(** equal values and the same right sibling, the children's difference
    empty: the difference is empty *)
Lemma mi_eq0r : forall v da db r, Mi da db D0 -> Mi (Nd v da r) (Nd v db r) D0.
Proof. intros; apply mi_eq0; [assumption | apply mi_same]. Qed.

(** ** Order, from the numbers' digits (doc/certificate.md, "Decided")

    A comparison Sylvan made, written as a proof built from the two
    numbers' binary digits, one lemma per digit, instead of a computation
    of [N.compare] the kernel would repeat. *)

Lemma plt_1_xO : forall q, (1 < q~0)%positive.
Proof. intros; lia. Qed.
Lemma plt_1_xI : forall q, (1 < q~1)%positive.
Proof. intros; lia. Qed.
Lemma plt_xO_xO : forall p q, (p < q)%positive -> (p~0 < q~0)%positive.
Proof. intros; lia. Qed.
Lemma plt_xI_xI : forall p q, (p < q)%positive -> (p~1 < q~1)%positive.
Proof. intros; lia. Qed.
Lemma plt_xO_xI : forall p q, (p < q)%positive -> (p~0 < q~1)%positive.
Proof. intros; lia. Qed.
Lemma plt_xO_xI_eq : forall p, (p~0 < p~1)%positive.
Proof. intros; lia. Qed.
Lemma plt_xI_xO : forall p q, (p < q)%positive -> (p~1 < q~0)%positive.
Proof. intros; lia. Qed.
Lemma nlt_0 : forall q, 0 < Npos q.
Proof. intros; lia. Qed.
Lemma nlt_pos : forall p q, (p < q)%positive -> Npos p < Npos q.
Proof. intros; lia. Qed.

(** a chunk of facts checked for their own sake ([MeBi Check Log],
    doc/certificate.md, A): the facts in a chain, the chunk's statement
    [True], so the kernel checks every one *)
Lemma chk_u : forall a b r, U a b r -> True -> True.
Proof. auto. Qed.
Lemma chk_mi : forall a b r, Mi a b r -> True -> True.
Proof. auto. Qed.
Lemma chk_nil : True.
Proof. exact I. Qed.

Lemma mi_empty : forall a b, Mi a b D0 -> forall w, In_t a w -> In_t b w.
Proof. intros a b [_ H] w Ha; destruct (H w Ha) as [K|[]]; exact K. Qed.

(** ** The image through a relation: complete

    [PostC cs x r R]: every vector of [x], stepped by a tuple of [r], lands
    in [R]. [WoC] is the written position's inner part (any value read, the
    rest of the tuple from [rd]); [WlC] the written values of an [RW]
    position once its read value matched. *)

Definition PostC (cs : list code) (x r R : dd) : Prop :=
  Srt x -> Srt r -> forall t a b, In_t x a -> In_t r t -> rstep cs t a b -> In_t R b.

Definition WoC (cs : list code) (x rd W : dd) : Prop :=
  Srt x -> Srt rd -> forall a0 a t b, In_t x (a0 :: a) -> In_t rd t -> rstep cs t a b -> In_t W b.

Definition WlC (cs : list code) (xa rb W : dd) : Prop :=
  Srt xa -> Srt rb -> forall a b0 t b, In_t xa a -> In_t rb (b0 :: t) -> rstep cs t a b -> In_t W (b0 :: b).

Lemma post_x0 : forall cs r R, PostC cs D0 r R.
Proof. intros cs r R _ _ t a b []. Qed.

Lemma post_r0 : forall cs x R, PostC cs x D0 R.
Proof. intros cs x R _ _ t a b _ []. Qed.

(** past the relation's last position: the image is the set itself (the
    step copies the rest of the vector), with no step per node *)
Lemma post_end : forall x r, PostC [] x r x.
Proof. intros x r _ _ t a b Ha _ Hs; destruct (rstep_nil t a b Hs) as [_ <-]; exact Ha. Qed.

Lemma post_x1 : forall c cs r R, PostC (c :: cs) D1 r R.
Proof. intros c cs r R _ _ t a b Ha _ H; simpl in Ha; subst a; destruct c, b; contradiction. Qed.

Lemma post_copy : forall cs v d xr r R1 R2, PostC cs d r R1 -> PostC (Copy :: cs) xr r R2 ->
  PostC (Copy :: cs) (Nd v d xr) r (Nd v R1 R2).
Proof.
  intros cs v d xr r R1 R2 H1 H2 [Sd [Sx _]] Sr t a b Ha Hr Hs.
  apply in_nd in Ha as [[a' [-> Ha]]|Ha].
  - destruct b as [|b0 b]; [contradiction|]; destruct Hs as [<- Hs].
    apply in_d; exact (H1 Sd Sr t a' b Ha Hr Hs).
  - apply in_r; exact (H2 Sx Sr t a b Ha Hr Hs).
Qed.

Lemma post_copy0 : forall cs v d xr r R2, PostC cs d r D0 -> PostC (Copy :: cs) xr r R2 ->
  PostC (Copy :: cs) (Nd v d xr) r R2.
Proof.
  intros cs v d xr r R2 H1 H2 [Sd [Sx _]] Sr t a b Ha Hr Hs.
  apply in_nd in Ha as [[a' [-> Ha]]|Ha].
  - destruct b as [|b0 b]; [contradiction|]; destruct Hs as [<- Hs]; destruct (H1 Sd Sr t a' b Ha Hr Hs).
  - exact (H2 Sx Sr t a b Ha Hr Hs).
Qed.

(** a node with no right sibling: its image is its child's, in one fact *)
Lemma post_copy_last : forall cs v d r R1, PostC cs d r R1 ->
  PostC (Copy :: cs) (Nd v d D0) r (Nd v R1 D0).
Proof. intros; apply post_copy; [assumption | apply post_x0]. Qed.

Lemma post_copy0_last : forall cs v d r, PostC cs d r D0 ->
  PostC (Copy :: cs) (Nd v d D0) r D0.
Proof. intros; apply post_copy0; [assumption | apply post_x0]. Qed.

(** a read position: the tuple's value is the vector's *)
Lemma rstep_read : forall c cs t a0 a b, c = RO \/ c = RW -> rstep (c :: cs) t (a0 :: a) b ->
  exists t', t = a0 :: t'.
Proof.
  intros c cs t a0 a [|b0 b] [->| ->] H; simpl in H; try contradiction.
  - destruct H as [_ [t' [-> _]]]; eauto.
  - destruct H as [t' [-> _]]; eauto.
Qed.

Lemma post_read_lt : forall c cs vx dx xr vr dr rr R, c = RO \/ c = RW -> vx < vr ->
  PostC (c :: cs) xr (Nd vr dr rr) R -> PostC (c :: cs) (Nd vx dx xr) (Nd vr dr rr) R.
Proof.
  intros c cs vx dx xr vr dr rr R Hc L H [Sd [Sx Ax]] Sr t a b Ha Hr Hs.
  apply in_nd in Ha as [[a' [-> Ha]]|Ha]; [|exact (H Sx Sr t a b Ha Hr Hs)].
  destruct (rstep_read c cs t vx a' b Hc Hs) as [t' ->].
  pose proof (above_le vr dr rr Sr vx t' Hr); lia.
Qed.

Lemma post_read_gt : forall c cs vx dx xr vr dr rr R, c = RO \/ c = RW -> vr < vx ->
  PostC (c :: cs) (Nd vx dx xr) rr R -> PostC (c :: cs) (Nd vx dx xr) (Nd vr dr rr) R.
Proof.
  intros c cs vx dx xr vr dr rr R Hc L H Sx [Sd [Sr Ar]] t a b Ha Hr Hs.
  apply in_nd in Hr as [[t' [-> Hr]]|Hr]; [|exact (H Sx Sr t a b Ha Hr Hs)].
  destruct (rstep_cons_l c cs (vr :: t') a b Hs) as [a0 [a' ->]].
  destruct (rstep_read c cs (vr :: t') a0 a' b Hc Hs) as [t'' E]; injection E as <- _.
  pose proof (above_le vx dx xr Sx vr a' Ha); lia.
Qed.

Lemma post_ro_lt : forall cs vx dx xr vr dr rr R, vx < vr ->
  PostC (RO :: cs) xr (Nd vr dr rr) R -> PostC (RO :: cs) (Nd vx dx xr) (Nd vr dr rr) R.
Proof. intros; apply post_read_lt; auto. Qed.

Lemma post_ro_gt : forall cs vx dx xr vr dr rr R, vr < vx ->
  PostC (RO :: cs) (Nd vx dx xr) rr R -> PostC (RO :: cs) (Nd vx dx xr) (Nd vr dr rr) R.
Proof. intros; apply post_read_gt; auto. Qed.

Lemma post_rw_lt : forall cs vx dx xr vr dr rr R, vx < vr ->
  PostC (RW :: cs) xr (Nd vr dr rr) R -> PostC (RW :: cs) (Nd vx dx xr) (Nd vr dr rr) R.
Proof. intros; apply post_read_lt; auto. Qed.

Lemma post_rw_gt : forall cs vx dx xr vr dr rr R, vr < vx ->
  PostC (RW :: cs) (Nd vx dx xr) rr R -> PostC (RW :: cs) (Nd vx dx xr) (Nd vr dr rr) R.
Proof. intros; apply post_read_gt; auto. Qed.

(** both read the same value [v]: a vector of [xr] and a tuple of [rr] (or
    the other way round) cannot meet, by the order *)
Lemma read_cross : forall c cs v dx xr dr rr t a b, c = RO \/ c = RW ->
  Srt (Nd v dx xr) -> Srt (Nd v dr rr) -> In_t (Nd v dx xr) a -> In_t (Nd v dr rr) t ->
  rstep (c :: cs) t a b ->
  (exists a' t', a = v :: a' /\ t = v :: t' /\ In_t dx a' /\ In_t dr t') \/ (In_t xr a /\ In_t rr t).
Proof.
  intros c cs v dx xr dr rr t a b Hc [_ [_ [Ax _]]] [_ [_ [Ar _]]] Ha Hr Hs.
  destruct (rstep_cons_l c cs t a b Hs) as [a0 [a' ->]].
  destruct (rstep_read c cs t a0 a' b Hc Hs) as [t' ->].
  apply in_nd in Ha as [[a'' [E Ha]]|Ha]; apply in_nd in Hr as [[t'' [E' Hr]]|Hr].
  - injection E as -> ->; injection E' as ->; left; exists a'', t''; auto.
  - injection E as -> ->; pose proof (Ar v t' Hr); lia.
  - injection E' as -> ->; pose proof (Ax v a' Ha); lia.
  - right; auto.
Qed.

Lemma post_ro_eq : forall cs v dx xr dr rr R1 R2, PostC cs dx dr R1 -> PostC (RO :: cs) xr rr R2 ->
  PostC (RO :: cs) (Nd v dx xr) (Nd v dr rr) (Nd v R1 R2).
Proof.
  intros cs v dx xr dr rr R1 R2 H1 H2 Sx Sr t a b Ha Hr Hs.
  destruct (read_cross RO cs v dx xr dr rr t a b (or_introl eq_refl) Sx Sr Ha Hr Hs)
    as [[a' [t' [-> [-> [Ha' Hr']]]]]|[Ha' Hr']].
  - destruct b as [|b0 b]; [contradiction|]; destruct Hs as [<- [t0 [E Hs]]]; injection E as <-.
    apply in_d; exact (H1 (proj1 Sx) (proj1 Sr) t' a' b Ha' Hr' Hs).
  - apply in_r; exact (H2 (proj1 (proj2 Sx)) (proj1 (proj2 Sr)) t a b Ha' Hr' Hs).
Qed.

Lemma post_ro_eq0 : forall cs v dx xr dr rr R2, PostC cs dx dr D0 -> PostC (RO :: cs) xr rr R2 ->
  PostC (RO :: cs) (Nd v dx xr) (Nd v dr rr) R2.
Proof.
  intros cs v dx xr dr rr R2 H1 H2 Sx Sr t a b Ha Hr Hs.
  destruct (read_cross RO cs v dx xr dr rr t a b (or_introl eq_refl) Sx Sr Ha Hr Hs)
    as [[a' [t' [-> [-> [Ha' Hr']]]]]|[Ha' Hr']].
  - destruct b as [|b0 b]; [contradiction|]; destruct Hs as [<- [t0 [E Hs]]]; injection E as <-.
    destruct (H1 (proj1 Sx) (proj1 Sr) t' a' b Ha' Hr' Hs).
  - exact (H2 (proj1 (proj2 Sx)) (proj1 (proj2 Sr)) t a b Ha' Hr' Hs).
Qed.

Lemma post_wo : forall cs x vr dr rr W R2, WoC cs x dr W -> PostC (WO :: cs) x rr R2 ->
  PostC (WO :: cs) x (Nd vr dr rr) (Nd vr W R2).
Proof.
  intros cs x vr dr rr W R2 H1 H2 Sx [Sd [Sr _]] t a b Ha Hr Hs.
  apply in_nd in Hr as [[t' [-> Hr]]|Hr]; [|apply in_r; exact (H2 Sx Sr t a b Ha Hr Hs)].
  destruct a as [|a0 a], b as [|b0 b]; try contradiction.
  destruct Hs as [t0 [E Hs]]; injection E as -> <-.
  apply in_d; exact (H1 Sx Sd a0 a t' b Ha Hr Hs).
Qed.

Lemma post_wo0 : forall cs x vr dr rr R2, WoC cs x dr D0 -> PostC (WO :: cs) x rr R2 ->
  PostC (WO :: cs) x (Nd vr dr rr) R2.
Proof.
  intros cs x vr dr rr R2 H1 H2 Sx [Sd [Sr _]] t a b Ha Hr Hs.
  apply in_nd in Hr as [[t' [-> Hr]]|Hr]; [|exact (H2 Sx Sr t a b Ha Hr Hs)].
  destruct a as [|a0 a], b as [|b0 b]; try contradiction.
  destruct Hs as [t0 [E Hs]]; injection E as -> <-.
  destruct (H1 Sx Sd a0 a t' b Ha Hr Hs).
Qed.

Lemma wo_x0 : forall cs rd W, WoC cs D0 rd W.
Proof. intros cs rd W _ _ a0 a t b []. Qed.

Lemma wo_x1 : forall cs rd W, WoC cs D1 rd W.
Proof. intros cs rd W _ _ a0 a t b H; discriminate H. Qed.

Lemma wo_nd : forall cs vx dx xr rd R1 R2 W, PostC cs dx rd R1 -> WoC cs xr rd R2 -> U R1 R2 W ->
  WoC cs (Nd vx dx xr) rd W.
Proof.
  intros cs vx dx xr rd R1 R2 W H1 H2 HU [Sd [Sx _]] Sr a0 a t b Ha Hr Hs; apply HU.
  apply in_nd in Ha as [[a' [E Ha]]|Ha].
  - injection E as -> ->; left; eapply H1; eauto.
  - right; exact (H2 Sx Sr a0 a t b Ha Hr Hs).
Qed.

Lemma post_rw_eq : forall cs v dx xr dr rr R1 R2 R, WlC cs dx dr R1 -> PostC (RW :: cs) xr rr R2 ->
  U R1 R2 R -> PostC (RW :: cs) (Nd v dx xr) (Nd v dr rr) R.
Proof.
  intros cs v dx xr dr rr R1 R2 R H1 H2 HU Sx Sr t a b Ha Hr Hs; apply HU.
  destruct (read_cross RW cs v dx xr dr rr t a b (or_intror eq_refl) Sx Sr Ha Hr Hs)
    as [[a' [t' [-> [-> [Ha' Hr']]]]]|[Ha' Hr']].
  - destruct b as [|b0 b]; [contradiction|]; destruct Hs as [t0 [E Hs]]; injection E as ->.
    left; exact (H1 (proj1 Sx) (proj1 Sr) a' b0 t0 b Ha' Hr' Hs).
  - right; exact (H2 (proj1 (proj2 Sx)) (proj1 (proj2 Sr)) t a b Ha' Hr' Hs).
Qed.

(** Sylvan's read-write case (doc/certificate.md, A): once the set's and
    the relation's first values agree, the image is the union of the
    write level's image ([dr], the written values) and of the image by the
    relation's right ([rr]) of the same set, matched again there *)
Lemma post_rw_s : forall cs v dx xr dr rr W R2 R, WlC cs dx dr W ->
  PostC (RW :: cs) (Nd v dx xr) rr R2 -> U W R2 R -> PostC (RW :: cs) (Nd v dx xr) (Nd v dr rr) R.
Proof.
  intros cs v dx xr dr rr W R2 R H1 H2 HU Sx Sr t a b Ha Hr Hs; apply HU.
  apply in_nd in Hr as [[t' [-> Hr]]|Hr].
  - destruct a as [|a0 a]; [destruct b; contradiction|]; destruct b as [|b0 b]; [contradiction|].
    destruct Hs as [t0 [E Hs]]; injection E as <- ->.
    apply in_nd in Ha as [[a' [E Ha]]|Ha].
    + injection E as E; subst; left; exact (H1 (proj1 Sx) (proj1 Sr) a' b0 t0 b Ha Hr Hs).
    + destruct Sx as [_ [_ [Ax _]]]; pose proof (Ax v a Ha); lia.
  - right; exact (H2 Sx (proj1 (proj2 Sr)) t a b Ha Hr Hs).
Qed.

(** the relation's last read value: the image is the write level's *)
Lemma post_rw_last : forall cs v dx xr dr W, WlC cs dx dr W ->
  PostC (RW :: cs) (Nd v dx xr) (Nd v dr D0) W.
Proof. intros; eapply post_rw_s; [eassumption | apply post_r0 | apply u_b0]. Qed.

(** an image's fact in a checked chunk ([MeBi Check Log]) *)
Lemma chk_post : forall cs x r R, PostC cs x r R -> True -> True.
Proof. auto. Qed.

Lemma wl_r0 : forall cs xa W, WlC cs xa D0 W.
Proof. intros cs xa W _ _ a b0 t b _ []. Qed.

Lemma wl_r1 : forall cs xa W, WlC cs xa D1 W.
Proof. intros cs xa W _ _ a b0 t b _ H; discriminate H. Qed.

Lemma wl_nd : forall cs xa vb db rb R1 R2, PostC cs xa db R1 -> WlC cs xa rb R2 ->
  WlC cs xa (Nd vb db rb) (Nd vb R1 R2).
Proof.
  intros cs xa vb db rb R1 R2 H1 H2 Sx [Sd [Sr _]] a b0 t b Ha Hr Hs.
  apply in_nd in Hr as [[t' [E Hr]]|Hr].
  - injection E as -> ->; apply in_d; eapply H1; eauto.
  - apply in_r; exact (H2 Sx Sr a b0 t b Ha Hr Hs).
Qed.

Lemma wl_nd0 : forall cs xa vb db rb R2, PostC cs xa db D0 -> WlC cs xa rb R2 ->
  WlC cs xa (Nd vb db rb) R2.
Proof.
  intros cs xa vb db rb R2 H1 H2 Sx [Sd [Sr _]] a b0 t b Ha Hr Hs.
  apply in_nd in Hr as [[t' [E Hr]]|Hr].
  - injection E as -> ->; exfalso; eapply (H1 Sx Sd); eauto.
  - exact (H2 Sx Sr a b0 t b Ha Hr Hs).
Qed.

(** the last written value, in one fact *)
Lemma wl_last : forall cs xa vb db R1, PostC cs xa db R1 -> WlC cs xa (Nd vb db D0) (Nd vb R1 D0).
Proof. intros; apply wl_nd; [assumption | apply wl_r0]. Qed.

Lemma wl_last0 : forall cs xa vb db, PostC cs xa db D0 -> WlC cs xa (Nd vb db D0) D0.
Proof. intros; apply wl_nd0; [assumption | apply wl_r0]. Qed.

(** ** The image through a relation: sound

    [PostS cs x r R]: every vector of [R] is the image of a vector of [x]
    by a tuple of [r] (the answers of the game's check: each is a step).
    Sylvan's skips only drop candidates, so no case needs the order or a
    comparison, and a skipped branch needs no fact. [WlS] is the written
    values of an [RW] position once its read value matched. *)

Definition PostS (cs : list code) (x r R : dd) : Prop :=
  forall b, In_t R b -> exists t a, In_t x a /\ In_t r t /\ rstep cs t a b.

Definition WlS (cs : list code) (xa rb W : dd) : Prop :=
  forall w, In_t W w -> exists b0 b t a, w = b0 :: b /\ In_t xa a /\ In_t rb (b0 :: t) /\ rstep cs t a b.

Lemma posts_0 : forall cs x r, PostS cs x r D0.
Proof. intros cs x r b []. Qed.

(** past the relation's last position (the relation consumed: [D1]) *)
Lemma posts_end : forall x, PostS [] x D1 x.
Proof. intros x b H; exists [], b; split; [exact H|]; split; [reflexivity|]; split; reflexivity. Qed.

(** fewer candidates, fewer images: Sylvan's skips, and a branch whose
    image is empty *)
Lemma posts_x : forall cs v d xr r R, PostS cs xr r R -> PostS cs (Nd v d xr) r R.
Proof.
  intros cs v d xr r R H b Hb; destruct (H b Hb) as [t [a [Ha [Hr Hs]]]].
  exists t, a; split; [apply in_r; exact Ha | auto].
Qed.

Lemma posts_r : forall cs x v d rr R, PostS cs x rr R -> PostS cs x (Nd v d rr) R.
Proof.
  intros cs x v d rr R H b Hb; destruct (H b Hb) as [t [a [Ha [Hr Hs]]]].
  exists t, a; split; [exact Ha|]; split; [apply in_r; exact Hr | exact Hs].
Qed.

Lemma posts_xr : forall cs vx dx xr vr dr rr R, PostS cs xr rr R ->
  PostS cs (Nd vx dx xr) (Nd vr dr rr) R.
Proof. intros; apply posts_x, posts_r; assumption. Qed.

Lemma posts_copy : forall cs v d xr r R1 R2, PostS cs d r R1 -> PostS (Copy :: cs) xr r R2 ->
  PostS (Copy :: cs) (Nd v d xr) r (Nd v R1 R2).
Proof.
  intros cs v d xr r R1 R2 H1 H2 b Hb; apply in_nd in Hb as [[b' [-> Hb]]|Hb].
  - destruct (H1 b' Hb) as [t [a [Ha [Hr Hs]]]]; exists t, (v :: a).
    split; [apply in_d; exact Ha|]; split; [exact Hr|]; split; [reflexivity | exact Hs].
  - destruct (H2 b Hb) as [t [a [Ha [Hr Hs]]]]; exists t, a.
    split; [apply in_r; exact Ha | auto].
Qed.

Lemma posts_ro : forall cs v dx xr dr rr R1 R2, PostS cs dx dr R1 -> PostS (RO :: cs) xr rr R2 ->
  PostS (RO :: cs) (Nd v dx xr) (Nd v dr rr) (Nd v R1 R2).
Proof.
  intros cs v dx xr dr rr R1 R2 H1 H2 b Hb; apply in_nd in Hb as [[b' [-> Hb]]|Hb].
  - destruct (H1 b' Hb) as [t [a [Ha [Hr Hs]]]]; exists (v :: t), (v :: a).
    split; [apply in_d; exact Ha|]; split; [apply in_d; exact Hr|].
    split; [reflexivity|]; exists t; split; [reflexivity | exact Hs].
  - destruct (H2 b Hb) as [t [a [Ha [Hr Hs]]]]; exists t, a.
    split; [apply in_r; exact Ha|]; split; [apply in_r; exact Hr | exact Hs].
Qed.

(** Sylvan's read-write case: the write level's images ([dr]: the written
    values) and the image by the relation's right ([rr]) of the same set,
    their union *)
Lemma posts_rw : forall cs v dx xr dr rr W R2 R, WlS cs dx dr W ->
  PostS (RW :: cs) (Nd v dx xr) rr R2 -> U W R2 R -> PostS (RW :: cs) (Nd v dx xr) (Nd v dr rr) R.
Proof.
  intros cs v dx xr dr rr W R2 R H1 H2 HU b Hb; apply HU in Hb as [Hb|Hb].
  - destruct (H1 b Hb) as [b0 [b' [t [a [-> [Ha [Hr Hs]]]]]]]; exists (v :: b0 :: t), (v :: a).
    split; [apply in_d; exact Ha|]; split; [apply in_d; exact Hr|].
    exists t; split; [reflexivity | exact Hs].
  - destruct (H2 b Hb) as [t [a [Ha [Hr Hs]]]]; exists t, a.
    split; [exact Ha|]; split; [apply in_r; exact Hr | exact Hs].
Qed.

Lemma wls_0 : forall cs xa rb, WlS cs xa rb D0.
Proof. intros cs xa rb w []. Qed.

Lemma wls_nd : forall cs xa vb db rb R1 R2, PostS cs xa db R1 -> WlS cs xa rb R2 ->
  WlS cs xa (Nd vb db rb) (Nd vb R1 R2).
Proof.
  intros cs xa vb db rb R1 R2 H1 H2 w Hw; apply in_nd in Hw as [[b [-> Hb]]|Hw].
  - destruct (H1 b Hb) as [t [a [Ha [Hr Hs]]]]; exists vb, b, t, a.
    split; [reflexivity|]; split; [exact Ha|]; split; [apply in_d; exact Hr | exact Hs].
  - destruct (H2 w Hw) as [b0 [b [t [a [-> [Ha [Hr Hs]]]]]]]; exists b0, b, t, a.
    split; [reflexivity|]; split; [exact Ha|]; split; [apply in_r; exact Hr | exact Hs].
Qed.

Lemma wls_nd0 : forall cs xa vb db rb R2, WlS cs xa rb R2 -> WlS cs xa (Nd vb db rb) R2.
Proof.
  intros cs xa vb db rb R2 H2 w Hw; destruct (H2 w Hw) as [b0 [b [t [a [-> [Ha [Hr Hs]]]]]]].
  exists b0, b, t, a; split; [reflexivity|]; split; [exact Ha|]; split; [apply in_r; exact Hr | exact Hs].
Qed.

(** a sound image's fact in a checked chunk ([MeBi Check Log]) *)
Lemma chk_posts : forall cs x r R, PostS cs x r R -> True -> True.
Proof. auto. Qed.

(** ** For the plugin's reader (lib/certify/dd_read.ml) *)

Register dd as mebi.dt.dd.
Register D0 as mebi.dt.D0.
Register D1 as mebi.dt.D1.
Register Nd as mebi.dt.Nd.
Register In_t as mebi.dt.In_t.
Register Srt as mebi.dt.Srt.
Register code as mebi.dt.code.
Register Copy as mebi.dt.Copy.
Register RO as mebi.dt.RO.
Register WO as mebi.dt.WO.
Register RW as mebi.dt.RW.
Register in1 as mebi.dt.in1.
Register Sib as mebi.dt.Sib.
Register sib_refl as mebi.dt.sib_refl.
Register sib_step as mebi.dt.sib_step.
Register in_sib_d as mebi.dt.in_sib_d.
Register srt_0 as mebi.dt.srt_0.
Register srt_1 as mebi.dt.srt_1.
Register srt_nd as mebi.dt.srt_nd.
Register above_0 as mebi.dt.above_0.
Register above_nd as mebi.dt.above_nd.
Register srt_nd0 as mebi.dt.srt_nd0.
Register srt_ndn as mebi.dt.srt_ndn.
Register U as mebi.dt.U.
Register Mi as mebi.dt.Mi.
Register u_comm as mebi.dt.u_comm.
Register chk_u as mebi.dt.chk_u.
Register chk_mi as mebi.dt.chk_mi.
Register chk_nil as mebi.dt.chk_nil.
Register chk_post as mebi.dt.chk_post.
Register post_rw_s as mebi.dt.post_rw_s.
Register plt_1_xO as mebi.dt.plt_1_xO.
Register plt_1_xI as mebi.dt.plt_1_xI.
Register plt_xO_xO as mebi.dt.plt_xO_xO.
Register plt_xI_xI as mebi.dt.plt_xI_xI.
Register plt_xO_xI as mebi.dt.plt_xO_xI.
Register plt_xO_xI_eq as mebi.dt.plt_xO_xI_eq.
Register plt_xI_xO as mebi.dt.plt_xI_xO.
Register nlt_0 as mebi.dt.nlt_0.
Register nlt_pos as mebi.dt.nlt_pos.
Register u_a0 as mebi.dt.u_a0.
Register u_b0 as mebi.dt.u_b0.
Register u_same as mebi.dt.u_same.
Register u_lt as mebi.dt.u_lt.
Register u_gt as mebi.dt.u_gt.
Register u_eq as mebi.dt.u_eq.
Register u_eq0 as mebi.dt.u_eq0.
Register mi_id as mebi.dt.mi_id.
Register mi_a0 as mebi.dt.mi_a0.
Register mi_same as mebi.dt.mi_same.
Register mi_lt as mebi.dt.mi_lt.
Register mi_gt as mebi.dt.mi_gt.
Register mi_eq as mebi.dt.mi_eq.
Register mi_eq0 as mebi.dt.mi_eq0.
Register post_x0 as mebi.dt.post_x0.
Register post_r0 as mebi.dt.post_r0.
Register post_x1 as mebi.dt.post_x1.
Register post_copy as mebi.dt.post_copy.
Register post_copy0 as mebi.dt.post_copy0.
Register post_ro_lt as mebi.dt.post_ro_lt.
Register post_ro_gt as mebi.dt.post_ro_gt.
Register post_rw_lt as mebi.dt.post_rw_lt.
Register post_rw_gt as mebi.dt.post_rw_gt.
Register post_ro_eq as mebi.dt.post_ro_eq.
Register post_ro_eq0 as mebi.dt.post_ro_eq0.
Register post_wo as mebi.dt.post_wo.
Register post_wo0 as mebi.dt.post_wo0.
Register wo_x0 as mebi.dt.wo_x0.
Register wo_x1 as mebi.dt.wo_x1.
Register wo_nd as mebi.dt.wo_nd.
Register post_rw_eq as mebi.dt.post_rw_eq.
Register wl_r0 as mebi.dt.wl_r0.
Register wl_r1 as mebi.dt.wl_r1.
Register wl_nd as mebi.dt.wl_nd.
Register wl_nd0 as mebi.dt.wl_nd0.
Register post_end as mebi.dt.post_end.
Register PostS as mebi.dt.PostS.
Register WlS as mebi.dt.WlS.
Register posts_0 as mebi.dt.posts_0.
Register posts_end as mebi.dt.posts_end.
Register posts_x as mebi.dt.posts_x.
Register posts_r as mebi.dt.posts_r.
Register posts_xr as mebi.dt.posts_xr.
Register posts_copy as mebi.dt.posts_copy.
Register posts_ro as mebi.dt.posts_ro.
Register posts_rw as mebi.dt.posts_rw.
Register wls_0 as mebi.dt.wls_0.
Register wls_nd as mebi.dt.wls_nd.
Register wls_nd0 as mebi.dt.wls_nd0.
Register chk_posts as mebi.dt.chk_posts.
Register u_lt0 as mebi.dt.u_lt0.
Register u_gt0 as mebi.dt.u_gt0.
Register u_eq_d as mebi.dt.u_eq_d.
Register u_eq_r as mebi.dt.u_eq_r.
Register mi_eq0d as mebi.dt.mi_eq0d.
Register post_copy_last as mebi.dt.post_copy_last.
Register post_copy0_last as mebi.dt.post_copy0_last.
Register post_rw_last as mebi.dt.post_rw_last.
Register wl_last as mebi.dt.wl_last.
Register wl_last0 as mebi.dt.wl_last0.
Register mi_eq0r as mebi.dt.mi_eq0r.
