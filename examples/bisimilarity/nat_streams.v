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

Definition get_opt_hd (s : stream nat) : option nat :=
  match out_stream s with
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

(******************************************************************************)
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
  - inversion HaNone. 
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
  - inversion HaNone.
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

Lemma parity_plus_odd : forall x incr,
  get_parity incr = ODD ->
  get_parity (x + incr) = inv_parity (get_parity x).
Proof.
  intros x incr Hpn. induction x.
  - apply Hpn.
  - rewrite parity_inv_incr, <- IHx. apply parity_inv_incr.
Qed.

Lemma parity_plus_1_3_odd_trans : forall hx sx0 sx1 sx2 hy sy0 sy1 sy2,
  out_stream sx1 = consF hx sx0 ->
  out_stream sy1 = consF hy sy0 ->
  get_parity hx = get_parity hy ->
  plus_1 sx1 (Some (get_parity (1 + hx))) sx2 ->
  plus_3 sy1 (Some (get_parity (3 + hy))) sy2 ->
  out_stream sx2 = consF (1 + hx) sx1 ->
  out_stream sy2 = consF (3 + hy) sy1 ->
  get_parity (1 + hx) = get_parity (3 + hy).
Proof.
  intros hx sx0 sx1 sx2 hy sy0 sy1 sy2. 
  intros Hsx1o Hsy1o Hxy1; intros Htx Hty; intros Hsx2o Hsy2o.
  replace (1 + hx) with (hx + 1).
  replace (3 + hy) with (hy + 3).
  rewrite !parity_plus_odd; [apply parity_inv_eq; apply Hxy1 | | ]; trivial.
  rewrite !plus_Sn_m, <- !plus_n_Sm, <- plus_n_O, plus_O_n; reflexivity.
  rewrite !plus_Sn_m, <- !plus_n_Sm, <- plus_n_O, plus_O_n; reflexivity.
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
  get_stream_parity sx0 = get_stream_parity sy0 ->
  ltsX sx1 (Some (get_parity (hx + dx))) sx2 ->
  ltsY sy1 (Some (get_parity (hy + dy))) sy2 ->
  out_stream sx2 = consF (hx + dx) sx1 ->
  out_stream sy2 = consF (hy + dy) sy1 ->
  get_stream_parity sx2 = get_stream_parity sy2.
Proof.
  intros dx dy. intros hx sx0 sx1 sx2 hy sy0 sy1 sy2. 
  intros Hdx Hdy. intros Hsx1o Hsy1o Hxy1; intros Htx Hty; intros Hsx2o Hsy2o.

  inversion Hsx1o as [Hsxp]; apply parity_stream_cons in Hsxp; subst.
  inversion Hsy1o as [Hsyp]; apply parity_stream_cons in Hsyp; subst.

  destruct get_stream_parity. admit.
  - 

  
  intros hx sx0 sx1 hy sy0 sy1 Hsxy Hsxo Hsyo Htx Hty.

  inversion Hsxo as [Hsxp]; apply parity_stream_cons in Hsxp; subst.
  inversion Hsyo as [Hsyp]; apply parity_stream_cons in Hsyp; subst.


  (* unfold get_stream_parity, get_opt_hd in H0; rewrite Hsxo in H0. *)

  apply parity_stream_cons in Hsxo, Hsyo; rewrite Hsxo, Hsyo.

  unfold get_stream_parity in Hsxo, Hsyo; unfold get_opt_hd in Hsxo, Hsyo.


  
  - 

  destruct (get_parity hx).
  - 



  destruct (get_parity hx), (get_parity hy); try reflexivity.
  - inversion Hsxo.


    inversion Htx; subst. 
    inversion Hty; subst.
    + inversion Hsxy.
  
    destruct (get_stream_parity sx1).
  

  inversion Htx; subst. inversion Hty; subst.
  - trivial.
  - inversion  
  


  apply parity_stream_cons in Hsxo. 


  (* unfold get_stream_parity. *)
  (* apply parity_stream_cons.  *)
  apply parity_stream_cons in Hsxo. 

  inversion Htx; subst. 
  - 
    (* rewrite H.  *)
    inversion Hty; subst.


  destruct (out_stream sx1) as [|]; [inversion Hsxo|]. 
  inversion Hsxo; subst.

  inversion Htx; subst.
  
  unfold get_stream_parity in Hsxy. 
  destruct (get_opt_hd sx0) as [AA|], (get_opt_hd sy0) as [BB|].
  - admit.



  destruct (get_stream_parity sx0) as [px|], (get_stream_parity sy0) as [py|].
  - admit.
  - inversion Hsxy.
  - inversion Hsxy. 
  - admit.

(******************************************************************************)
Example parity_plus_one_sim_plus_three : forall sx sy,
  get_stream_parity sx = get_stream_parity sy ->
  weak_sim plus_1 plus_3 sx sy.
Proof. cofix CH. intros sx1 sy1 Hsxy. 
  apply In_sim, Pack_sim.
  { intros sx2 p Htx; apply silent_plus_1 in Htx.
    inversion Htx as 
    [ _sx1 Hsx1Nil Hsx1 Hp Hsx2 | _sx1 hx1 _sx0 Hsx01 Hsx1 Hp Hsx2 ]; subst. 
    { 
        inversion Hsxy as [Hsy1Nil]; 
        apply parity_stream_eq_nil in Hsy1Nil; [| apply Hsx1Nil].
        exists ({| out_stream := consF 3 sy1 |}); split.
      - apply silent_plus_3.
      - apply CH. Guarded.
        (* inversion Hsxy as [Hsy1Nil]; 
        apply parity_stream_eq_nil in Hsy1Nil; [| apply Hsx1Nil]. *)
        constructor.
    }
    { exists ({| out_stream := consF (3 + hy) sy1 |}); split.

    }


        rewrite parity_stream_nil in Hsxy. 
        rewrite parity_stream_nil in Hsxy. 
      
    }

    (* ; subst. *)
    { 

    }


  }






Lemma parity_from_plus_1 : forall s0 p s1, plus_1 s0 (Some p) s1 -> 
  exists v, out_stream s1 = consF v s0 -> (get_parity v) = p.
Proof. intros s0 p s1 Ht. inversion Ht; subst; eexists; intros Hs1; reflexivity.
Qed.

(* Lemma parity_trans_plus_one_three : forall sx0 sy0,
  get_stream_parity sx0 = get_stream_parity sy0 ->
  forall sx1 vx, out_stream sx1 = consF vx sx0 ->
  plus_1 sx0 (Some (get_parity vx)) sx1 ->
  forall sy1 vy, out_stream sy1 = consF vy sy0 ->
  plus_3 sy0 (Some (get_parity vy)) sy1 ->
  get_stream_parity sx1 = get_stream_parity sy1.
Proof. 
  intros sx0 sy0 Hsxy0. 
  intros sx1 vx Hsx1 Hpl1.
  

  

  intros sy1 vy Hsy1 Hpl3.
  unfold get_stream_parity.
  
  induction (get_hd sx1) as [| nx], (get_hd sy1) as [| ny]; trivial.
  assert (get_parity (S ny) = EVN).
  {

  }
  


  

  apply parity_from_plus_1 in Hpl1.

  Hpl1 Hpl3.
  apply parity_from_plus_1 in Hpl1. inversion Hpl1; subst.
  

  inversion Hpl1; subst.
  - 

  induction (get_stream_parity sx0).
  {

  }  *)

(* Lemma parity_of_stream : forall s,
  get_stream_parity s -> 
  match out_stream s with
  | nilF -> get_parity  *)


Example cons_parity_plus_one_sim_plus_three : forall hx0 sx0 sx1 hy0 sy0 sy1,
  out_stream sx1 = consF hx0 sx0 ->
  out_stream sy1 = consF hy0 sy0 ->
  get_parity hx0 = get_parity hy0 ->
  weak_sim plus_1 plus_3 sx1 sy1.
Proof.
  intros hx0 sx0 sx1.
  intros hy0 sy0 sy1.
  intros Hsx01.
  intros Hsy01.
  intros Hp0.

  apply In_sim, Pack_sim.

  { intros sx2 p Htx; apply silent_plus_1 in Htx.

    inversion Htx; subst.


    induction Htx as [ sx1 HsxNil | sx1 hx0' sx0' Hsx01' ].
    - rewrite Hsx01 in HsxNil; inversion HsxNil.
    - rewrite Hsx01 in Hsx01'.    
      contradiction.
    
      inversion Hsx. 


  }



(******************************************************************************)
Example parity_plus_one_sim_plus_three : forall sx sy,
  get_stream_parity sx = get_stream_parity sy ->
  weak_sim plus_1 plus_3 sx sy.
Proof. cofix CH. intros sx0 sy0 Hsxy. apply In_sim, Pack_sim.

  { intros sx1 p Htx. apply silent_plus_1 in Htx.
    exists (In_stream (consF (S (get_hd sy0)) sy0)); split.
    { admit. }
    { apply CH. Guarded.
      inversion Htx as [ _sx0 Hsx0 | _sx0 xn xns Hsx0 ]; subst.
      - unfold get_stream_parity, get_parity. simpl.
        destruct (get_hd sy0).
        + reflexivity.
        + 
    }



    { inversion Htx as [ _sx0 Hsx0 | _sx0 xn xns Hsx0 ]; subst; 
      destruct get_stream_parity.
      -  

      Print parity_trans.
      rewrite parity_trans in Hsxy.


      destruct (get_stream_parity sy0).
      - 
      - 
    
      rewrite parity_nil_stream in Hsxy. 

    }




    { inversion Htx as [ _sx0 Hsx0 | _sx0 xn xns Hsx0 ]; subst.
      - rewrite parity_nil_stream in Hsxy. 
        assert (get_parity (get_hd sx0) = EVN) as Hsxp.
        { 

        }
        destruct (get_stream_parity sx0) in Hsxy.
        destruct (get_stream_parity sy0).
      - admit. 
    } 
    { apply CH. Guarded. 

    }
    inversion Htx as [ _sx0 Hsx0 | _sx0 xn xns Hsx0 ]; subst.
    {
      rewrite parity_nil_stream in Hsxy. 
      assert (get_parity (get_hd sx0) = EVN) as Hsxp.
      { 

      }
      destruct (get_stream_parity sx0) in Hsxy.
      destruct (get_stream_parity sy0).

    }
    {

    }
    inversion sy0 as [Hsy0]; inversion_clear Hsy0 as [| bb cc]; subst.
    admit. 
    admit. 
    admit. 
    admit. 
    { split.
      - 
        unfold weak.
      
      apply silent_plus_3.

    }
    { 

    }
    

  }











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
