Require Import MEBI.Bisimilarity.

Set Primitive Projections.

Inductive streamF (Stream : Set -> Set) (A : Set) :=
| nilF : streamF Stream A
| consF : A -> Stream A -> streamF Stream A.
Arguments nilF & {_}{_}.
Arguments consF & {_}{_}.

CoInductive stream (A : Set) : Set := 
  In_stream { out_stream : streamF stream A }.
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

Definition eq_parity (a b : parity) : Prop :=
  match a, b with
  | EVN, EVN => True
  | ODD, ODD => True
  | _, _ => False
  end.

Lemma assoc_parity (a b : parity) : eq_parity a b -> eq_parity b a.
Proof. induction a, b; simpl; trivial. Qed.

Definition get_parity (n : nat) : parity := if Nat.even n then EVN else ODD.

Lemma refl_parity : forall n, eq_parity (get_parity n) (get_parity n).
Proof. intros n. destruct (get_parity n); simpl; trivial. Qed.

Lemma equiv_parity : forall x y, eq_parity (get_parity x) (get_parity y) ->
  get_parity x = get_parity y.
Proof. 
  intros x y Hxy. destruct (get_parity x), (get_parity y); simpl; trivial.
  - destruct Hxy. 
  - destruct Hxy.
Qed.

(*************************************)
Inductive plus_1 : stream nat -> option parity -> stream nat -> Prop :=
| PLUS_ONE : forall n ns,
  plus_1  (In_stream (consF n ns)) 
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
  plus_2  (In_stream (consF n ns)) 
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
  plus_3  (In_stream (consF n ns)) 
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
  plus_1 (In_stream (consF n ns)) (Some (get_parity (S n))) (In_stream (consF (S n) (In_stream (consF n ns)))) -> 
  plus_1 (In_stream (consF (S n) (In_stream (consF n ns)))) (Some (get_parity (S (S n)))) (In_stream (consF (S (S n)) (In_stream (consF (S n) (In_stream (consF n ns)))))) -> 
  plus_1_twice  (In_stream (consF n ns)) 
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
  plus_1 (In_stream (consF n ns)) (Some (get_parity (S n))) (In_stream (consF (S n) (In_stream (consF n ns)))) -> 
  plus_1 (In_stream (consF (S n) (In_stream (consF n ns)))) (Some (get_parity (S (S n)))) (In_stream (consF (S (S n)) (In_stream (consF (S n) (In_stream (consF n ns)))))) -> 
  plus_1 (In_stream (consF (S (S n)) (In_stream (consF (S n) (In_stream (consF n ns)))))) (Some (get_parity (S (S (S n))))) (In_stream (consF (S (S (S n))) (In_stream (consF n ns)))) -> 
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
Proof.
  cofix CH. intros x0 xs0 y0 ys0 Hxy0. apply In_sim, Pack_sim.
  assert (get_parity (S (S x0)) = get_parity (S (S y0))) as Hp.
  { apply equiv_parity, Hxy0. }
  { intros xs1 px01 Hx01.
    destruct Hx01 as [xs0a H], H as [xs0b H], H as [Hpre Hstr Hpost].
    exists ({| out_stream := consF (S (S y0)) (In_stream (consF y0 ys0)) |}).
    split.
    { unfold weak; 
      exists ({| out_stream := consF y0 ys0 |});
      exists ({| out_stream := consF (S (S y0)) (In_stream (consF y0 ys0)) |}).
      apply Pack_weak; try constructor.
      destruct Hpost as [|ns0a ns0b Hns01].
      - destruct Hpre; inversion Hstr; subst.
        + rewrite Hp; apply PLUS_TWICE; apply PLUS_ONE.
        + inversion H. 
      - destruct Hpre; inversion Hstr; subst.
        + rewrite Hp; apply PLUS_TWICE; apply PLUS_ONE.
        + inversion H.
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
Proof.
  cofix CH.
Admitted.

Example plus_bisim_two_twice : forall x xs y ys,
  eq_parity (get_parity x) (get_parity y) -> 
  weak_bisim plus_2 plus_1_twice 
    (In_stream (consF x xs)) (In_stream (consF y ys)).
Proof. 
  split. apply plus_two_sim_twice, H. 
  apply plus_twice_sim_two, assoc_parity, H. 
Qed.

(*************************************)
