Require Import MEBI.Bisimilarity.

Set Primitive Projections.

Inductive streamF (Stream : Set -> Set) (A : Set) :=
| nilF : streamF Stream A
| consF : A -> Stream A -> streamF Stream A.
Arguments nilF & {_}{_}.
Arguments consF & {_}{_}.

CoInductive stream (A : Set) : Set := In_stream { out_stream : streamF stream A }.
Arguments out_stream {A%_type_scope} s.
Arguments In_stream {A%_type_scope}.

CoFixpoint fmap {A B : Set} (f : A -> B) (s : stream A) : stream B :=
  match out_stream s with
  | nilF => In_stream nilF
  | consF h t => In_stream (consF (f h) (fmap f t))
  end.

(*************************************)
Inductive parity : Type :=
| EVN : parity
| ODD : parity
.

Inductive eq_parity : parity -> parity -> Prop :=
| TEVN : eq_parity EVN EVN
| TODD : eq_parity ODD ODD
.

Lemma parity_eq : forall a b, eq_parity a b -> a = b.
Proof. intros a b; induction a, b; simpl; trivial; intros H; inversion H. Qed.

Lemma parity_assoc : forall a b, eq_parity a b -> eq_parity b a.
Proof. intros a b; induction a, b; simpl; trivial; intros H; inversion H. Qed.

Definition get_parity (n : nat) : parity := if Nat.even n then EVN else ODD.

Lemma parity_refl : forall n, eq_parity (get_parity n) (get_parity n).
Proof. intros n; destruct (get_parity n). apply TEVN. apply TODD. Qed.

Lemma parity_equiv : forall x y, eq_parity (get_parity x) (get_parity y) ->
  get_parity x = get_parity y.
Proof. intros x y Hxy. 
  destruct (get_parity x), (get_parity y); trivial; destruct Hxy; reflexivity. 
Qed.

Definition inv_parity (p : parity): parity :=
  match p with
  | EVN => ODD
  | ODD => EVN
  end.

Lemma parity_inv_evn : forall n, get_parity n = EVN -> inv_parity (get_parity n) = ODD.
Proof. intros n H. rewrite H. simpl; reflexivity. Qed.

Lemma parity_inv_odd : forall n, get_parity n = ODD -> inv_parity (get_parity n) = EVN.
Proof. intros n H. rewrite H. simpl; reflexivity. Qed.

Lemma parity_inv_inv : forall n, get_parity n = inv_parity (inv_parity (get_parity n)).
Proof. intros n. destruct (get_parity n); simpl; reflexivity. Qed.

Lemma parity_inv_incr : forall n, get_parity (S n) = inv_parity (get_parity n).
Proof. intros n. induction n; simpl; trivial.
  rewrite IHn; rewrite <- parity_inv_inv; apply eq_refl.
Qed. 

Lemma parity_trans : forall x y, eq_parity (get_parity x) (get_parity y) ->
  eq_parity (get_parity (S x)) (get_parity (S y)).
Proof. intros x y H. destruct x.
  - apply parity_eq in H; symmetry in H; apply parity_inv_evn in H.
    rewrite <- parity_inv_incr in H; rewrite H. apply TODD.
  - destruct y.
    + apply parity_eq in H; apply parity_inv_evn in H.
      rewrite <- parity_inv_incr in H; rewrite H. apply TODD.
    + inversion H; subst; symmetry in H1, H2.
      * apply parity_inv_evn in H1; rewrite <- parity_inv_incr in H1. 
        apply parity_inv_evn in H2; rewrite <- parity_inv_incr in H2.
        rewrite H1, H2. apply TODD.
      * apply parity_inv_odd in H1; rewrite <- parity_inv_incr in H1. 
        apply parity_inv_odd in H2; rewrite <- parity_inv_incr in H2.
        rewrite H1, H2. apply TEVN.
Qed.

(*************************************)
Inductive plus_1 : stream nat -> option parity -> stream nat -> Prop :=
| PLUS_ONE : forall n ns,
  plus_1 (In_stream (consF n ns)) 
         (Some (get_parity (S n)))
         (In_stream (consF (S n) (In_stream (consF n ns))))
.

Lemma nats_plus_1_c : forall n ns a c, 
  plus_1 (In_stream (consF n ns)) a c -> 
  out_stream c = consF (S n) (In_stream (consF n ns)).
Proof. intros n ns a c Hin. inversion Hin; subst. simpl; reflexivity. Qed.

Lemma nats_plus_1_s : forall n ns a s, 
  plus_1 s a (In_stream (consF (S n) (In_stream (consF n ns)))) -> 
  out_stream s = consF n ns.
Proof. intros n ns a s Hin. inversion Hin; subst. simpl; reflexivity. Qed.

Inductive clos_t_plus_1 : stream nat -> Prop :=
| TRANS_PLUS_ONE : forall s1 s2 p, 
  plus_1 s1 p s2 ->
  clos_t_plus_1 s2 ->
  clos_t_plus_1 s1
.

(*************************************)
Inductive plus_2 : stream nat -> option parity -> stream nat -> Prop :=
| PLUS_TWO : forall n ns,
  plus_2 (In_stream (consF n ns)) 
         (Some (get_parity (S (S n))))
         (In_stream (consF (S (S n)) (In_stream (consF n ns))))
.

Inductive clos_t_plus_2 : stream nat -> Prop :=
| TRANS_PLUS_TWo : forall s1 s2 p, 
  plus_2 s1 p s2 ->
  clos_t_plus_2 s2 ->
  clos_t_plus_2 s1
.

(*************************************)
Inductive plus_3 : stream nat -> option parity -> stream nat -> Prop :=
| PLUS_THREE : forall n ns,
  plus_3 (In_stream (consF n ns)) 
         (Some (get_parity (S (S (S n)))))
         (In_stream (consF (S (S (S n))) (In_stream (consF n ns))))
.

Inductive clos_t_plus_3 : stream nat -> Prop :=
| TRANS_PLUS_TWREE : forall s1 s2 p, 
  plus_3 s1 p s2 ->
  clos_t_plus_3 s2 ->
  clos_t_plus_3 s1
.

(*************************************)
Inductive plus_1_twice : stream nat -> option parity -> stream nat -> Prop :=
| PLUS_TWICE : forall n ns,
  plus_1 (In_stream (consF n ns)) 
         (Some (get_parity (S n)))
         (In_stream (consF (S n) (In_stream (consF n ns)))) -> 
  plus_1 (In_stream (consF (S n) (In_stream (consF n ns)))) 
         (Some (get_parity (S (S n))))
         (In_stream (consF (S (S n)) (In_stream (consF (S n) (In_stream (consF n ns)))))) -> 
  plus_1_twice (In_stream (consF n ns)) 
               (Some (get_parity (S (S n))))
               (In_stream (consF (S (S n)) (In_stream (consF n ns))))
.

Lemma nats_plus_1_twice_s : forall n ns a s, 
  plus_1_twice s a (In_stream (consF (S (S n)) (In_stream (consF n ns)))) -> 
  out_stream s = consF n ns.
Proof. intros n ns a2 s Hin. inversion Hin; subst; simpl. reflexivity. Qed.

Inductive clos_t_plus_1_twice : stream nat -> Prop :=
| TRANS_PLUS_TWICE : forall s1 s2 p, 
  plus_1_twice s1 p s2 ->
  clos_t_plus_1_twice s2 ->
  clos_t_plus_1_twice s1
.

(*************************************)
Inductive plus_1_thrice : stream nat -> option parity -> stream nat -> Prop :=
| PLUS_THRICE : forall n ns,
  plus_1 (In_stream (consF n ns)) 
         (Some (get_parity (S n))) 
         (In_stream (consF (S n) (In_stream (consF n ns)))) -> 
  plus_1 (In_stream (consF (S n) (In_stream (consF n ns)))) 
         (Some (get_parity (S (S n)))) 
         (In_stream (consF (S (S n)) (In_stream (consF (S n) (In_stream (consF n ns)))))) -> 
  plus_1 (In_stream (consF (S (S n)) (In_stream (consF (S n) (In_stream (consF n ns)))))) 
         (Some (get_parity (S (S (S n))))) 
         (In_stream (consF (S (S (S n))) (In_stream (consF n ns)))) -> 
  plus_1_thrice (In_stream (consF n ns)) 
                (Some (get_parity (S (S (S n))))) 
                (In_stream (consF (S (S (S n))) (In_stream (consF n ns))))
.

Lemma nats_plus_1_thrice_s : forall n ns a s, 
  plus_1_thrice s a (In_stream (consF (S (S (S n))) (In_stream (consF n ns)))) -> 
  out_stream s = consF n ns.
Proof. intros n ns a2 s Hin. inversion Hin; subst; simpl. reflexivity. Qed.

Inductive clos_t_plus_1_thrice : stream nat -> Prop :=
| TRANS_PLUS_THRICE : forall s1 s2 p, 
  plus_1_thrice s1 p s2 ->
  clos_t_plus_1_thrice s2 ->
  clos_t_plus_1_thrice s1
.

(*************************************)
CoFixpoint zeroes : stream nat := In_stream (consF 0 zeroes).

(*************************************)
Example plus_two_sim_twice : forall x xs y ys,
  eq_parity (get_parity x) (get_parity y) -> 
  weak_sim plus_2 plus_1_twice 
    (In_stream (consF x xs)) (In_stream (consF y ys)).
Proof. cofix CH. intros x0 xs0 y0 ys0 Hxy0. apply In_sim, Pack_sim.
  { intros xs1 px01 Hx01.
    destruct Hx01 as [xs01a [xs01b [Hpre Hstr Hpost]]].
    exists ({| out_stream := consF (S (S y0)) (In_stream (consF y0 ys0)) |}).
    split.
    { unfold weak; exists ({| out_stream := consF y0 ys0 |});
      exists ({| out_stream := consF (S (S y0)) (In_stream (consF y0 ys0)) |}).
      apply Pack_weak; try constructor.
      (*************************************************)
      destruct Hpost as [|ns0a ns0b Hns01]; do 2 (
        destruct Hpre; inversion Hstr; subst; (
          replace (get_parity (S (S x0))) with (get_parity (S (S y0))); 
          try (apply PLUS_TWICE; apply PLUS_ONE); 
          try (symmetry; apply parity_eq; do 2 apply parity_trans in Hxy0; apply Hxy0);
          try inversion H ) ).
      (*************************************************)
      (* destruct Hpost as [|ns0a ns0b Hns01]. *)
      (* - destruct Hpre; inversion Hstr; subst. *)
        (* + replace (get_parity (S (S x0))) with (get_parity (S (S y0))). *)
          (* { apply PLUS_TWICE; apply PLUS_ONE. } *)
          (* { symmetry; apply parity_eq. do 2 apply parity_trans in Hxy0. apply Hxy0. } *)
        (* + inversion H. *)
      (* - destruct Hpre; inversion Hstr; subst. *)
        (* + replace (get_parity (S (S x0))) with (get_parity (S (S y0))). *)
          (* { apply PLUS_TWICE; apply PLUS_ONE. } *)
          (* { symmetry; apply parity_eq. do 2 apply parity_trans in Hxy0. apply Hxy0. } *)
        (* + inversion H. *)
        (*************************************************)
    }
    { destruct Hpost as [|ns0a ns0b Hns01].
      - destruct Hpre; inversion Hstr; subst; apply CH. Guarded.
        + apply Hxy0.
        + inversion H.
      - inversion Hns01.
    }
  }
  { intros xs1 Hx01. destruct Hx01; inversion_clear H. }
Qed. 

Example plus_twice_sim_two : forall x xs y ys,
  eq_parity (get_parity x) (get_parity y) -> 
  weak_sim plus_1_twice plus_2 
    (In_stream (consF x xs)) (In_stream (consF y ys)).
Proof. cofix CH. intros x0 xs0 y0 ys0 Hxy0. apply In_sim, Pack_sim.
  { intros xs1 px01 Hx01.
    destruct Hx01 as [xs01a [xs01b [Hpre Hstr Hpost]]].
    exists ({| out_stream := consF (S (S y0)) (In_stream (consF y0 ys0)) |}).
    split.
    { unfold weak; 
      exists ({| out_stream := consF y0 ys0 |});
      exists ({| out_stream := consF (S (S y0)) (In_stream (consF y0 ys0)) |}).
      apply Pack_weak; try constructor.
      (*************************************************)
      destruct Hpost as [|ns0a ns0b Hns01]; do 2 (
        destruct Hpre; inversion Hstr; subst; (
          replace (get_parity (S (S x0))) with (get_parity (S (S y0))); 
          try apply PLUS_TWO;
          try (symmetry; apply parity_eq; do 2 apply parity_trans in Hxy0; apply Hxy0);
          try inversion H ) ) .
      (*************************************************)
      (* destruct Hpost as [|ns0a ns0b Hns01]. *)
      (* - destruct Hpre; inversion Hstr; subst. *)
        (* + replace (get_parity (S (S x0))) with (get_parity (S (S y0))).  *)
          (* { apply PLUS_TWO. } *)
          (* { symmetry; apply parity_eq. do 2 apply parity_trans in Hxy0. apply Hxy0. }  *)
        (* + inversion H.  *)
      (* - destruct Hpre; inversion Hstr; subst. *)
        (* + replace (get_parity (S (S x0))) with (get_parity (S (S y0))).  *)
          (* { apply PLUS_TWO. } *)
          (* { symmetry; apply parity_eq; do 2 apply parity_trans in Hxy0; apply Hxy0. }  *)
        (* + inversion H.  *)
      (*************************************************)
    }
    { destruct Hpost as [|ns0a ns0b Hns01].
      - destruct Hpre; inversion Hstr; subst; apply CH. Guarded.
        + apply Hxy0.
        + inversion H.
      - inversion Hns01.
    }
  }
  { intros xs1 Hx01. destruct Hx01; inversion_clear H. }
Qed.

Example bisim_plus_two_or_twice : forall x xs y ys,
  eq_parity (get_parity x) (get_parity y) -> 
  weak_bisim plus_2 plus_1_twice 
    (In_stream (consF x xs)) (In_stream (consF y ys)).
Proof. split. 
  - apply plus_two_sim_twice, H. 
  - apply plus_twice_sim_two, parity_assoc, H. 
Qed.

(*************************************)
Example parity_one_sim_three : forall x xs y ys,
  eq_parity (get_parity x) (get_parity y) -> 
  weak_sim plus_1 plus_3
    (In_stream (consF x xs)) (In_stream (consF y ys)).
Proof. cofix CH. intros x0 xs0 y0 ys0 Hxy0. apply In_sim, Pack_sim.
  { intros x1 pOdd Hx01.
    destruct Hx01 as [xs01a [xs01b [Hpre Hstr Hpost]]].
    exists ({| out_stream := consF (S (S (S y0))) (In_stream (consF y0 ys0)) |}).
    split.
    { unfold weak; 
      exists ({| out_stream := consF y0 ys0 |});
      exists ({| out_stream := consF (S (S (S y0))) (In_stream (consF y0 ys0)) |}).
      apply Pack_weak; try constructor.
      destruct Hpost as [|ns0a ns0b Hns01]; do 2 (
        destruct Hpre; inversion Hstr; subst; (
          replace (get_parity (S x0)) with (get_parity (S (S (S y0)))); 
          try apply PLUS_THREE;
          try (symmetry; apply parity_eq; do 3 apply parity_trans in Hxy0; apply Hxy0);
          try inversion H ) ) .
    }
    { destruct Hpost as [|ns0a ns0b Hns01].
      - destruct Hpre; inversion Hstr; subst; apply CH. Guarded.
        + apply parity_trans in Hxy0; apply Hxy0.
        + inversion H.
      - inversion Hns01.
    }
  }
  { intros xs1 Hx01. destruct Hx01; inversion_clear H. }
Qed.

Example parity_three_sim_one : forall x xs y ys,
  eq_parity (get_parity x) (get_parity y) -> 
  weak_sim plus_3 plus_1
    (In_stream (consF x xs)) (In_stream (consF y ys)).
Proof. cofix CH. intros x0 xs0 y0 ys0 Hxy0. apply In_sim, Pack_sim.
  { intros x1 pOdd Hx01.
    destruct Hx01 as [xs01a H], H as [xs01b H], H as [Hpre Hstr Hpost].
    exists ({| out_stream := consF (S y0) (In_stream (consF y0 ys0)) |}).
    split.
    { unfold weak; 
      exists ({| out_stream := consF y0 ys0 |});
      exists ({| out_stream := consF (S y0) (In_stream (consF y0 ys0)) |}).
      apply Pack_weak; try constructor.
      destruct Hpost as [|ns0a ns0b Hns01]; do 2 (
        destruct Hpre; inversion Hstr; subst; (
          replace (get_parity (S (S (S x0)))) with (get_parity (S y0)); 
          try apply PLUS_ONE;
          try (symmetry; apply parity_eq; do 3 apply parity_trans in Hxy0; apply Hxy0);
          try inversion H ) ) .
    }
    { destruct Hpost as [|ns0a ns0b Hns01].
      - destruct Hpre; inversion Hstr; subst; apply CH. Guarded.
        + apply parity_trans in Hxy0; apply Hxy0.
        + inversion H.
      - inversion Hns01.
    }
  }
  { intros xs1 Hx01. destruct Hx01; inversion_clear H. }
Qed.

Example bisim_odd_one_or_three : forall x xs y ys,
  eq_parity (get_parity x) (get_parity y) -> 
  weak_bisim plus_1 plus_3
    (In_stream (consF x xs)) (In_stream (consF y ys)).
Proof. split. 
  - apply parity_one_sim_three, H. 
  - apply parity_three_sim_one, parity_assoc, H. 
Qed.

(******************************************************************************)
 Ltac sim_nats_act_trans vy1 y0 ys CH Hxy Hp :=
  exists ({| out_stream := consF vy1 {| out_stream := consF y0 ys |} |});
  inversion_clear Hstr; subst; split; 
  first 
    [ unfold weak;
      exists ({| out_stream := consF y0 ys |});
      exists ({| out_stream := consF vy1 (In_stream (consF y0 ys)) |});
      apply Pack_weak; try constructor; rewrite Hp;
      first [ simpl; constructor 
            | symmetry; apply parity_eq; apply Hp ]
    | apply CH; 
      first [ apply parity_trans in Hxy; apply Hxy
            | inversion H ]
    ].

Ltac sim_nats_act vx1 vy1 CH x0 xs y0 ys Hxy Hp :=
  intros x1 p1 [xs01a [xs01b [[|_xs01a _xs01b H01ab H01rt] Hstr [|ns0a ns0b Hns01tau Hns01rt]]]];
  match goal with 
  | [ Hns01tau : tau _ ?xs01b ?ns0a |- exists _, weak _ _ _ _ /\ weak_sim _ _ _ _ ] => inversion Hns01tau
  | [ H01ab : tau _ {| out_stream := _ |} ?_xs01a |- exists _, weak _ _ _ _ /\ weak_sim _ _ _ _ ] => inversion H01ab
  | |- exists _, weak _ _ _ _ /\ weak_sim _ _ _ _ => sim_nats_act_trans vy1 y0 ys CH Hxy Hp 
  end.

Ltac sim_nats_tau := intros xs1 Hx01; destruct Hx01; inversion_clear H.

Ltac sim_nats_parity vx1 vy1 x0 xs y0 ys CH Hxy Hp :=
  apply In_sim, Pack_sim; 
  first [ sim_nats_act vx1 vy1 CH x0 xs y0 ys Hxy Hp | sim_nats_tau ].

Ltac sim_nats_parity_intro xin yin := 
  let CH := fresh "CH" in 
  cofix CH; intros x0 xs y0 ys Hxy;
  pose xin as dx; pose yin as dy;
  pose (Nat.add dx x0) as vx1; pose (Nat.add dy y0) as vy1; 
  simpl in vy1, vy1;
  assert (get_parity vx1 = get_parity vy1) as Hp; 
  first 
    [ unfold vx1, vy1; apply parity_eq; 
      repeat match goal with 
      | [ Hxy : @eq_parity _ (@get_parity ?py) |- @eq_parity _ (@get_parity ?py) ] => apply Hxy
      | [ Hxy : @eq_parity _ (@get_parity _) |- _ ] => apply parity_trans in Hxy
      end
    | unfold vx1, vy1 in Hp; simpl in Hp;
      sim_nats_parity vx1 vy1 x0 xs y0 ys CH Hxy Hp
    ].
Tactic Notation "solve_sim_nats" constr(xin) constr(yin) := sim_nats_parity_intro xin yin.

(*************************************)
Example ltac_parity_one_sim_three : forall x xs y ys,
  eq_parity (get_parity x) (get_parity y) -> 
  weak_sim plus_1 plus_3
    (In_stream (consF x xs)) (In_stream (consF y ys)).
Proof. solve_sim_nats 1 3. Qed.

(*************************************)
Example ltac_parity_three_sim_one : forall x xs y ys,
  eq_parity (get_parity x) (get_parity y) -> 
  weak_sim plus_3 plus_1
    (In_stream (consF x xs)) (In_stream (consF y ys)).
Proof. solve_sim_nats 3 1. Qed.

(*************************************)
Require Coq.Program.Tactics.
Ltac bisim_nats_parity xin yin := 
  split; Coq.Program.Tactics.reverse;
  first [ sim_nats_parity_intro xin yin
        | let CH := fresh "CH" in 
          cofix CH; intros y0 ys x0 xs Hxy;
          (* pose xin as dy; pose yin as dx; *)
          (* sim_nats_parity dx dy x0 xs y0 ys CH Hxy *)


          pose xin as dx; pose yin as dy;
          pose (Nat.add dx x0) as vx1; pose (Nat.add dy y0) as vy1; 
          simpl in vy1, vy1;
          assert (get_parity vx1 = get_parity vy1) as Hp; 
          first 
            [ unfold vx1, vy1; apply parity_eq; 
              repeat match goal with 
              | [ Hxy : @eq_parity _ (@get_parity ?py) |- @eq_parity _ (@get_parity ?py) ] => apply Hxy
              | [ Hxy : @eq_parity _ (@get_parity _) |- _ ] => apply parity_trans in Hxy
              end
            | unfold vx1, vy1 in Hp; simpl in Hp;
              sim_nats_parity vx1 vy1 x0 xs y0 ys CH Hxy
            ]

        ].
Tactic Notation "solve_bisim_nats" constr(xin) constr(yin) := bisim_nats_parity xin yin.


Example ltac_bisim_odd_one_or_three : forall x xs y ys,
  eq_parity (get_parity x) (get_parity y) -> 
  weak_bisim plus_1 plus_3
    (In_stream (consF x xs)) (In_stream (consF y ys)).
Proof. 
  (* solve_bisim_nats 1 3. *)

  split; Coq.Program.Tactics.reverse.
  - sim_nats_parity_intro 1 3.
  - let CH := fresh "CH" in 
    cofix CH; intros y0 ys x0 xs Hxy.
    pose 3 as dx; pose 1 as dy.
    (* apply parity_assoc in Hxy. *)

          (* pose xin as dx; pose yin as dy; *)
          pose (Nat.add dx x0) as vx1; pose (Nat.add dy y0) as vy1. 
          simpl in vx1, vy1.
          assert (get_parity vx1 = get_parity vy1) as Hp. 
          + unfold vx1, vy1. apply parity_eq. 
              (* repeat *)
               match goal with 
              | [ Hxy : @eq_parity _ (@get_parity ?py) |- @eq_parity _ (@get_parity ?py) ] => apply Hxy
              | [ Hxy : @eq_parity _ (@get_parity _) |- _ ] => apply parity_trans in Hxy
              end.
            | unfold vx1, vy1 in Hp; simpl in Hp;
              sim_nats_parity vx1 vy1 x0 xs y0 ys CH Hxy
            ].



    (* sim_nats_parity dx dy x0 xs y0 ys CH Hxy. *)
    pose (Nat.add dx x0) as vx1; pose (Nat.add dy y0) as vy1; 
    simpl in vy1, vy1;
    apply In_sim, Pack_sim. 
    { 
      (* sim_nats_act vx1 vy1 CH x0 xs y0 ys Hxy. *)
      assert (get_parity vx1 = get_parity vy1) as Hp. 
      * unfold vx1, vy1; apply parity_eq; 
        repeat match goal with 
        | [ Hxy : @eq_parity _ (@get_parity ?py) |- @eq_parity _ (@get_parity ?py) ] => apply Hxy
        | [ Hxy : @eq_parity _ (@get_parity _) |- _ ] => apply parity_trans in Hxy
        end.
      * unfold vx1, vy1 in Hp; simpl in Hp;
        intros x1 p1 [xs01a [xs01b [[|_xs01a _xs01b H01ab H01rt] Hstr [|ns0a ns0b Hns01tau Hns01rt]]]]
        . 
        match goal with 
        | [ Hns01tau : tau _ ?xs01b ?ns0a |- exists _, weak _ _ _ _ /\ weak_sim _ _ _ _ ] => inversion Hns01tau
        | [ H01ab : tau _ {| out_stream := _ |} ?_xs01a |- exists _, weak _ _ _ _ /\ weak_sim _ _ _ _ ] => inversion H01ab
        | |- exists _, weak _ _ _ _ /\ weak_sim _ _ _ _ => 
            (* sim_nats_act_trans vy1 y0 ys CH Hxy Hp  *)
            exists ({| out_stream := consF vy1 {| out_stream := consF y0 ys |} |});
            inversion_clear Hstr; subst; split 
        end.
          - unfold weak;
                exists ({| out_stream := consF y0 ys |});
                exists ({| out_stream := consF vy1 (In_stream (consF y0 ys)) |}).
                apply Pack_weak; try constructor. rewrite Hp.
                first [ simpl; constructor 
                      | symmetry; apply parity_eq; apply Hp ].
          - apply CH; 
                first [ apply parity_trans in Hxy; apply Hxy
                      | inversion H ]
              ]
        end.

    }
    + sim_nats_tau.



  (* split; Coq.Program.Tactics.reverse. *)
  (* sim_nats_parity 1 3. *)
  (* sim_nats_parity 3 1. *)


  split.
  - apply ltac_parity_one_sim_three, H. 
  - apply ltac_parity_three_sim_one, parity_assoc, H. 
Qed.
