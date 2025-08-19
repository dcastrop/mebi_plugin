Require Import MEBI.Bisimilarity.
Require Coq.Program.Tactics.

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

(* CoInductive stream (A:Set) : Type := *)
(* | Cons : A -> stream A -> stream A. *)
(* Arguments Cons {A%_type_scope}. *)

Definition get_opt_hd (s : stream nat) : option nat :=
  match out_stream s with
  (* | Cons h t => Some h *)
  | nilF => None
  | consF x _xs => Some x
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

Lemma parity_inv_eq : forall x y, get_parity x = get_parity y -> 
  inv_parity (get_parity x) = inv_parity (get_parity y).
Proof. intros x y Hxy. rewrite Hxy. reflexivity. Qed.

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

Lemma parity_some_opt : forall x y, 
  get_parity x = get_parity y ->
  Some (get_parity x) = Some (get_parity y).
Proof. intros x y Hxy. destruct (get_parity x); rewrite Hxy; reflexivity. Qed.

Lemma parity_opt_some : forall x y, 
  Some (get_parity x) = Some (get_parity y) ->
  get_parity x = get_parity y.
Proof. intros x y Hxy. inversion Hxy. reflexivity. Qed.

(******************************************************************************)
(* Inductive stream_parity : stream nat -> Type :=
| SEVN : forall s x c, out_stream s = consF x c -> get_parity x = EVN -> 
    stream_parity s
| SODD : forall s x c, out_stream s = consF x c -> get_parity x = ODD -> 
    stream_parity s
.  *)

(* Fail (stream_parity (In_stream nilF)). *)
(* Check (stream_parity (In_stream (consF 0 (In_stream nilF)))). *)

Definition get_stream_parity (s : stream nat) : option parity := 
  match get_opt_hd s with
  | None => None
  | Some x => Some (get_parity x)
  end.

Lemma parity_stream_nil : forall s,
  out_stream s = nilF -> get_stream_parity s = None.
Proof.
  intros s Hs. unfold get_stream_parity, get_opt_hd. 
  rewrite Hs; reflexivity.
Qed.

Lemma parity_stream_cons : forall s h t, out_stream s = consF h t ->
  get_stream_parity s = Some (get_parity h).
Proof.
  intros s h t Hs. 
  unfold get_stream_parity, get_opt_hd. 
  rewrite Hs; reflexivity.
Qed.

Lemma parity_stream_none_hd : forall a,
  get_stream_parity a = None ->
  get_opt_hd a = None.
Proof.
  intros a HaNone. 
  unfold get_stream_parity in HaNone. destruct (get_opt_hd a). 
  - discriminate HaNone. 
  - reflexivity.
Qed.

Lemma parity_stream_none_out : forall a,
  get_stream_parity a = None ->
  out_stream a = nilF.
Proof.
  intros a HaNone.
  apply parity_stream_none_hd in HaNone. unfold get_opt_hd in HaNone.
  destruct (out_stream a). 
  - reflexivity. 
  - discriminate HaNone.
Qed.

Lemma parity_stream_eq_nil : forall a b,
  get_stream_parity a = get_stream_parity b ->
  out_stream a = nilF ->
  out_stream b = nilF.
Proof.
  intros a b Hab HaNil.
  rewrite parity_stream_none_out. reflexivity.
  rewrite <- Hab.
  apply parity_stream_nil, HaNil.
Qed.

Lemma parity_stream_some_out_parity : forall a ax ac,
  out_stream a = consF ax ac ->
  get_stream_parity a = Some (get_parity ax).
Proof.
  intros a ax ac HaCons.
  unfold get_stream_parity, get_opt_hd. rewrite HaCons. reflexivity.
Qed.

Lemma parity_stream_some_hd_opt_eq : forall a ax bx,
  get_opt_hd a = Some ax ->
  get_opt_hd a = Some bx ->
  ax = bx.
Proof. intros a ax bx Haax Habx.
  inversion Haax; rewrite Habx in H0.
  inversion H0; subst; reflexivity.
Qed.

Lemma parity_stream_some_hd_opt : forall a ax,
  get_stream_parity a = Some (get_parity ax) ->
  get_opt_hd a = Some ax.
Proof. intros a ax Ha.
  inversion Ha. unfold get_stream_parity in H0.
  destruct (get_opt_hd a); [| discriminate H0].
Admitted.

Lemma parity_stream_eq_some_cons : forall a b ac bc ax bx,
  get_stream_parity a = get_stream_parity b ->
  get_stream_parity a = Some (get_parity ax) ->
  get_stream_parity b = Some (get_parity bx) ->
  out_stream a = consF ax ac ->
  out_stream b = consF bx bc.
Proof.
  intros a b ac bc ax bx Hab Ha Hb Hao.
Admitted.

Lemma parity_stream_eq_impl
: forall hx sx0 sx1 hy sy0 sy1,
  out_stream sx1 = consF hx sx0 ->
  out_stream sy1 = consF hy sy0 ->
  get_stream_parity sx1 = get_stream_parity sy1 ->
  get_parity hx = get_parity hy.
Proof.
  intros hx sx0 sx1 hy sy0 sy1 Hsx1o Hsy1o Hxy.
  inversion Hsx1o as [Hsxp]; apply parity_stream_cons in Hsxp; subst.
  inversion Hsy1o as [Hsyp]; apply parity_stream_cons in Hsyp; subst.
  rewrite Hsxp, Hsyp in Hxy. 
  apply parity_opt_some, Hxy.
Qed.

(******************************************************************************)
Lemma parity_plus_odd : forall x incr,
  get_parity incr = ODD ->
  get_parity (x + incr) = inv_parity (get_parity x).
Proof.
  intros x incr Hpn. induction x.
  - apply Hpn.
  - rewrite parity_inv_incr, <- IHx. apply parity_inv_incr.
Qed.

Lemma parity_plus_odd_trans 
(ltsX ltsY : stream nat -> option parity -> stream nat -> Prop) 
: forall dx dy hx sx0 sx1 sx2 hy sy0 sy1 sy2,
  get_parity dx = ODD -> 
  get_parity dy = ODD ->
  out_stream sx1 = consF hx sx0 ->
  out_stream sy1 = consF hy sy0 ->
  get_parity hx = get_parity hy ->
  ltsX sx1 (Some (get_parity (hx + dx))) sx2 ->
  ltsY sy1 (Some (get_parity (hy + dy))) sy2 ->
  out_stream sx2 = consF (hx + dx) sx1 ->
  out_stream sy2 = consF (hy + dy) sy1 ->
  get_parity (hx + dx) = get_parity (hy + dy).
Proof.
  intros dx dy. intros hx sx0 sx1 sx2 hy sy0 sy1 sy2. 
  intros Hdx Hdy. intros Hsx1o Hsy1o Hxy1; intros Htx Hty; intros Hsx2o Hsy2o.
  rewrite !parity_plus_odd; [apply parity_inv_eq, Hxy1 | apply Hdy | apply Hdx].
Qed.

Lemma parity_plus_odd_trans_stream
(ltsX ltsY : stream nat -> option parity -> stream nat -> Prop) 
: forall dx dy hx sx0 sx1 sx2 hy sy0 sy1 sy2,
  get_parity dx = ODD -> 
  get_parity dy = ODD ->
  out_stream sx1 = consF hx sx0 ->
  out_stream sy1 = consF hy sy0 ->
  get_stream_parity sx1 = get_stream_parity sy1 ->
  ltsX sx1 (Some (get_parity (hx + dx))) sx2 ->
  ltsY sy1 (Some (get_parity (hy + dy))) sy2 ->
  out_stream sx2 = consF (hx + dx) sx1 ->
  out_stream sy2 = consF (hy + dy) sy1 ->
  get_stream_parity sx2 = get_stream_parity sy2.
Proof.
  intros dx dy; intros hx sx0 sx1 sx2 hy sy0 sy1 sy2. 
  intros Hdx Hdy; intros Hsx1o Hsy1o Hxy1; intros Htx Hty; intros Hsx2o Hsy2o.

  inversion Hsx2o as [Hsxp2]; apply parity_stream_cons in Hsxp2; subst.
  inversion Hsy2o as [Hsyp2]; apply parity_stream_cons in Hsyp2; subst.
  rewrite Hsxp2, Hsyp2; apply parity_some_opt.

  apply (@parity_plus_odd_trans ltsX ltsY dx dy hx sx0 sx1 sx2 hy sy0 sy1 sy2); trivial.

  apply (@parity_stream_eq_impl hx sx0 sx1 hy sy0 sy1); trivial.
Qed.

(*************************************)
Inductive plus_1 : stream nat -> option parity -> stream nat -> Prop :=
| PLUS_ONE_NIL : forall s,
  out_stream s = nilF ->
  plus_1 s (Some (get_parity (S 0))) (In_stream (consF (S 0) s))
| PLUS_ONE_CONS : forall s n ns,
  out_stream s = consF n ns ->
  plus_1 s (Some (get_parity (S n))) (In_stream (consF (S n) s))
.

Inductive clos_t_plus_1 : stream nat -> Prop :=
| TRANS_PLUS_ONE : forall s1 s2 p, 
  plus_1 s1 p s2 -> clos_t_plus_1 s2 -> clos_t_plus_1 s1
.

(*************************************)
Inductive plus_2 : stream nat -> option parity -> stream nat -> Prop :=
| PLUS_TWO_NIL : forall s,
  out_stream s = nilF ->
  plus_2 s (Some (get_parity (S (S 0)))) (In_stream (consF (S (S 0)) s))
| PLUS_TWO_CONS : forall s n ns,
  out_stream s = consF n ns ->
  plus_2 s (Some (get_parity (S (S n)))) (In_stream (consF (S (S n)) s))
.

Inductive clos_t_plus_2 : stream nat -> Prop :=
| TRANS_PLUS_TWo : forall s1 s2 p, 
  plus_2 s1 p s2 -> clos_t_plus_2 s2 -> clos_t_plus_2 s1
.

(*************************************)
Inductive plus_3 : stream nat -> option parity -> stream nat -> Prop :=
| PLUS_THREE_NIL : forall s,
  out_stream s = nilF ->
  plus_3 s (Some (get_parity (S (S (S 0))))) (In_stream (consF (S (S (S 0))) s))
| PLUS_THREE_CONS : forall s n ns,
  out_stream s = consF n ns ->
  plus_3 s (Some (get_parity (S (S (S n))))) (In_stream (consF (S (S (S n))) s))
.

Inductive clos_t_plus_3 : stream nat -> Prop :=
| TRANS_PLUS_TWREE : forall s1 s2 p, 
  plus_3 s1 p s2 -> clos_t_plus_3 s2 -> clos_t_plus_3 s1
.

(******************************************************************************)
Lemma silent_plus_1 : forall s0 p s1,
  weak plus_1 s0 (Some p) s1 -> plus_1 s0 (Some p) s1.
Proof. intros s0 p s1 [s01a [s01b [Hpre Hstr Hpost]]].
  destruct Hpre, Hpost; try apply Hstr; inversion H.
Qed.  

Lemma silent_plus_3 : forall s0 p s1,
  weak plus_3 s0 (Some p) s1 -> plus_3 s0 (Some p) s1.
Proof. intros s0 p s1 [s01a [s01b [Hpre Hstr Hpost]]].
  destruct Hpre, Hpost; try apply Hstr; inversion H.
Qed. 

(******************************************************************************)
Example parity_plus_one_sim_plus_three : forall sx sy,
  get_stream_parity sx = get_stream_parity sy ->
  weak_sim plus_1 plus_3 sx sy.
Proof. cofix CH. intros sx1 sy1 Hsxy. apply In_sim, Pack_sim.
  { intros sx2 p Htx; apply silent_plus_1 in Htx.

    inversion Htx as 
    [ _sx1 Hsx1Nil Hsx1 Hp Hsx2 | _sx1 hx1 sx0 Hsx01 Hsx1 Hp Hsx2 ]; subst. 
    (* sx1 = nilF *)
    { inversion Hsxy as [Hsy1Nil]; 
      apply parity_stream_eq_nil in Hsy1Nil; [| apply Hsx1Nil].

      exists ({| out_stream := consF 3 sy1 |}); split.
      { unfold weak. 
        exists sy1, {| out_stream := consF 3 sy1 |}.
        apply Pack_weak; constructor; apply Hsy1Nil. }        
      { apply CH. Guarded. simpl. reflexivity. } }
    (* sx1 = consF hx sx0 *)
    { inversion Hsxy as [Hsxy1]. 
      unfold get_stream_parity, get_opt_hd in Hsxy1; rewrite Hsx01 in Hsxy1. 
      destruct (out_stream sy1) as [| hy1 sy0]; [inversion Hsxy1; subst |].
      inversion Hsxy1 as [Hpxy].

      assert (Hhx1S : S hx1 = hx1 + 1). 
      { symmetry. rewrite plus_n_O. simpl; rewrite !plus_n_Sm; reflexivity. }
      
      assert (Hhy1S : S (S (S hy1)) = hy1 + 3). 
      { symmetry. rewrite plus_n_O. simpl; rewrite !plus_n_Sm; reflexivity. }

      assert (Hsy01 : out_stream sy1 = consF hy1 sy0). 
      { apply (@parity_stream_eq_some_cons sx1 sy1 sx0 sy0 hx1 hy1).
        - apply Hsxy.
        - unfold get_stream_parity, get_opt_hd. rewrite Hsx01. reflexivity.
        - rewrite <- Hsxy, <- Hsxy1.
          unfold get_stream_parity, get_opt_hd. rewrite Hsx01. reflexivity.
        - apply Hsx01. }

      assert (Hty : plus_3 sy1 (Some (get_parity (hy1 + 3))) {| out_stream := consF (hy1 + 3) sy1 |}).
      { rewrite <- Hhy1S. apply (@PLUS_THREE_CONS sy1 hy1 sy0 Hsy01). }

      assert (Hsxy2 : get_parity (hx1 + 1) = get_parity (hy1 + 3)).
      { apply (@parity_plus_odd_trans 
                  plus_1 plus_3 1 3 
                  hx1 sx0 sx1 {| out_stream := consF (hx1 + 1) sx1 |}
                  hy1 sy0 sy1 {| out_stream := consF (hy1 + 3) sy1 |}
                  ); try reflexivity.
        - apply Hsx01.
        - apply Hsy01.
        - apply Hpxy.
        - rewrite <- Hhx1S; apply Htx.
        - apply Hty. }

      exists ({| out_stream := consF (hy1 + 3) sy1 |}); split.
      { rewrite Hhx1S, Hsxy2.
        unfold weak. exists sy1, {| out_stream := consF (hy1 + 3) sy1 |}. 
        apply Pack_weak.
        - constructor.
        - apply Hty. 
        - constructor. }  
      { apply CH. Guarded.
        apply (@parity_plus_odd_trans_stream 
                  plus_1 plus_3 1 3 
                  hx1 sx0 sx1 ({| out_stream := consF (S hx1) sx1 |}) 
                  hy1 sy0 sy1 ({| out_stream := consF (hy1 + 3) sy1 |})); 
                  try reflexivity.
        - apply Hsx01.
        - apply Hsy01.
        - apply Hsxy.
        - rewrite <- Hhx1S; apply Htx.
        - apply Hty. 
        - simpl. rewrite Hhx1S. reflexivity. } } } 
  { intros sx2 Htx. destruct Htx; inversion_clear H. }
Qed.





(*************************************)

(*************************************)

(*************************************)

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
  plus_1_twice s1 p s2 -> clos_t_plus_1_twice s2 -> clos_t_plus_1_twice s1
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
  plus_1_thrice s1 p s2 -> clos_t_plus_1_thrice s2 -> clos_t_plus_1_thrice s1
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

Ltac sim_nats_act vy1 y0 ys CH Hxy Hp :=
  intros x1 p1 [xs01a [xs01b [[|_xs01a _xs01b H01ab H01rt] Hstr [|ns0a ns0b Hns01tau Hns01rt]]]];
  match goal with 
  | [ Hns01tau : tau _ ?xs01b ?ns0a 
    |- exists _, weak _ _ _ _ /\ weak_sim _ _ _ _ ] => inversion Hns01tau
  | [ H01ab : tau _ {| out_stream := _ |} ?_xs01a 
    |- exists _, weak _ _ _ _ /\ weak_sim _ _ _ _ ] => inversion H01ab
  | |- exists _, weak _ _ _ _ /\ weak_sim _ _ _ _ => sim_nats_act_trans vy1 y0 ys CH Hxy Hp 
  end.

Ltac sim_nats_tau := intros xs1 Hx01; destruct Hx01; inversion_clear H.

Ltac sim_nats_parity vy1 y0 ys CH Hxy Hp :=
  apply In_sim, Pack_sim; 
  first [ sim_nats_act vy1 y0 ys CH Hxy Hp | sim_nats_tau ].

Ltac sim_nats_parity_intro xin yin := 
  let CH := fresh "CH" in 
  cofix CH; intros x0 xs y0 ys Hxy;
  pose xin as dx; pose yin as dy;
  pose (Nat.add dx x0) as vx1; pose (Nat.add dy y0) as vy1; simpl in vy1, vy1;
  assert (get_parity vx1 = get_parity vy1) as Hp; 
  first 
    [ unfold vx1, vy1; apply parity_eq; 
      repeat match goal with 
      | [ Hxy : @eq_parity _ (@get_parity ?py) 
      |- @eq_parity _ (@get_parity ?py) ] => apply Hxy
      | [ Hxy : @eq_parity _ (@get_parity _) 
      |- _ ] => apply parity_trans in Hxy
      end
    | unfold vx1, vy1 in Hp; simpl in Hp;
      sim_nats_parity vy1 y0 ys CH Hxy Hp
    ].
Tactic Notation "solve_sim_nats" constr(xin) constr(yin) := sim_nats_parity_intro xin yin.

(*************************************)
Example ltac_parity_one_sim_three : forall x xs y ys,
  eq_parity (get_parity x) (get_parity y) -> 
  weak_sim plus_1 plus_3
    (In_stream (consF x xs)) (In_stream (consF y ys)).
Proof. solve_sim_nats 1 3. Qed.

Example ltac_parity_three_sim_one : forall x xs y ys,
  eq_parity (get_parity x) (get_parity y) -> 
  weak_sim plus_3 plus_1
    (In_stream (consF x xs)) (In_stream (consF y ys)).
Proof. solve_sim_nats 3 1. Qed.

(*************************************)
Example ltac_parity_two_sim_twice : forall x xs y ys,
  eq_parity (get_parity x) (get_parity y) -> 
  weak_sim plus_2 plus_1_twice
    (In_stream (consF x xs)) (In_stream (consF y ys)).
Proof. 
  (* NOTE: "No applicable tactic." *)
  (* solve_sim_nats 2 2. *)
Admitted.

Example ltac_parity_twice_sim_two : forall x xs y ys,
  eq_parity (get_parity x) (get_parity y) -> 
  weak_sim plus_1_twice plus_2
    (In_stream (consF x xs)) (In_stream (consF y ys)).
Proof. 
  (* NOTE: ["eq_parity (get_parity (S (S x0))) (get_parity vy1)"]*)
  (* solve_sim_nats 2 2. *)
Admitted.

(*************************************)
Example ltac_parity_three_sim_thrice : forall x xs y ys,
  eq_parity (get_parity x) (get_parity y) -> 
  weak_sim plus_3 plus_1_thrice
    (In_stream (consF x xs)) (In_stream (consF y ys)).
Proof. 
  (* NOTE: doesn't handle the multiple layers of [plus_1_thrice] *)
  (* solve_sim_nats 3 3. *)
Admitted.

Example ltac_parity_thrice_sim_three : forall x xs y ys,
  eq_parity (get_parity x) (get_parity y) -> 
  weak_sim plus_1_thrice plus_3
    (In_stream (consF x xs)) (In_stream (consF y ys)).
Proof. solve_sim_nats 3 3. Qed.

(******************************************************************************)
Ltac bisim_nats_parity xin yin := 
  split; Coq.Program.Tactics.reverse;
  first 
    [ sim_nats_parity_intro xin yin
    | let CH := fresh "CH" in 
      cofix CH; intros x0 xs y0 ys Hxy;
      pose xin as dx; pose yin as dy;
      pose (Nat.add dx x0) as vx1; pose (Nat.add dy y0) as vy1; 
      simpl in vx1, vy1;
      assert (get_parity vx1 = get_parity vy1) as Hp; 
      first 
        [ unfold vx1, vy1; apply parity_eq; 
          repeat match goal with 
          | [ Hxy : @eq_parity _ (@get_parity ?py) |- @eq_parity _ (@get_parity ?py) ] => apply Hxy
          | [ Hxy : @eq_parity _ (@get_parity _) |- _ ] => apply parity_trans in Hxy
          end
        | unfold vx1, vy1 in Hp; simpl in Hp; 
          (* since this is [y => x], we need to swap [Hp] and [y] for [x]. *)
          symmetry in Hp; sim_nats_parity vx1 x0 xs CH Hxy Hp
        ]
    ].
Tactic Notation "solve_bisim_nats" constr(xin) constr(yin) := bisim_nats_parity xin yin.

Example ltac_bisim_odd_one_or_three : forall x xs y ys,
  eq_parity (get_parity x) (get_parity y) -> 
  weak_bisim plus_1 plus_3
    (In_stream (consF x xs)) (In_stream (consF y ys)).
Proof. solve_bisim_nats 1 3. Qed.

(******************************************************************************)
Example ltac_bisim_parity_incr_two : forall x xs y ys,
  eq_parity (get_parity x) (get_parity y) -> 
  weak_bisim plus_2 plus_1_twice
    (In_stream (consF x xs)) (In_stream (consF y ys)).
Proof. 
  (* NOTE: "No applicable tactic." *)
  (* solve_bisim_nats 2 2. *)
Admitted.


Example ltac_bisim_parity_incr_three : forall x xs y ys,
  eq_parity (get_parity x) (get_parity y) -> 
  weak_bisim plus_3 plus_1_thrice
    (In_stream (consF x xs)) (In_stream (consF y ys)).
Proof. 
  (* NOTE: doesn't handle the multiple layers of [plus_1_thrice] *)
  (* solve_bisim_nats 3 3.  *)
Admitted.
