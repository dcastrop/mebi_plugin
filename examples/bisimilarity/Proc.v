(* Require Import MEBI.loader. *)
Require Coq.Program.Tactics.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.
Import NoBranching.

Inductive bLTS : term -> option bool -> term -> Prop :=
| BISIM_WRAP : forall t1 t2 a, termLTS t1 a t2 -> bLTS t1 (Some a) t2.

Lemma bLTS_unwrap : forall x1 x2 a, bLTS x1 (Some a) x2 -> termLTS x1 a x2.
Proof. intros x1 x2 a Ht. inversion Ht; subst. apply H1. Qed.

Lemma blts_not_none_label : forall x1 x2 a, bLTS x1 a x2 -> a <> None.
Proof. intros x1 x2 a Ht HlNone. rewrite HlNone in Ht. inversion Ht. Qed.

Lemma blts_weak_some_label : forall x1 x2 a, 
  weak bLTS x1 a x2 -> exists l, a = (Some l).
Proof. intros x1 x2 a [x1b [x2a Hwtr]]. 
  inversion Hwtr as [Hpre Hstr Hpost]. destruct Hpre, Hpost; try inversion H.
  inversion Hstr; subst. exists a0. reflexivity.
Qed.  

Lemma blts_weak_silent : forall x1 x2 l,
  weak bLTS x1 (Some l) x2 -> bLTS x1 (Some l) x2.
Proof. intros x1 x2 l [x1b [x2a [Hpre Hstr Hpost]]].
  destruct Hpre, Hpost; try apply Hstr; inversion H.
Qed.  

Lemma blts_silent1_refl : forall x1 x2, silent1 bLTS x1 x2 -> x1 = x2.
Proof. intros x1 x2 Hs1. inversion Hs1; subst. inversion H. inversion H. Qed.

Lemma blts_wsim_silent1 : forall x1 x2 y1, weak_sim bLTS bLTS x1 y1 ->
  silent1 bLTS x1 x2 -> exists y2, 
    silent bLTS y1 y2 /\ weak_sim bLTS bLTS x2 y2.
Proof. intros x1 x2 y1 Hxy Htx. exists y1. split; [ constructor | 
  apply blts_silent1_refl in Htx; rewrite <- Htx; apply Hxy ].
Qed.

Module Test1.
  (* these do nothing, should be of[tpar (tact ASend ...) (tact ARecv ...)] *)
  Example x : term := tfix (tact ASend trec).  
  Example y : term := tfix (tact ASend (tact ASend trec)). 

  Theorem ws_xy : forall x0 y0, x0 = x -> y0 = y -> weak_sim bLTS bLTS x0 y0.
  Proof. intros x0 y0 Hx0 Hy0; unfold x, y in Hx0, Hy0. 

    cofix CH1. apply In_sim, Pack_sim; 
    (* [| intros x1 Htx; apply (@blts_wsim_silent1 x0 x1 y0 CH1 Htx)]. *)
    (* NOTE: the above can't pass CH1 to another lemma -- ill-formed. *)
    [ | intros _x1 Htx; apply blts_silent1_refl in Htx; 
        rewrite <- Htx; exists y0; split; [ constructor | apply CH1] ]. 
    Guarded.

    intros x1 ax01 Htx1. 
    apply blts_weak_silent, bLTS_unwrap in Htx1. rewrite Hx0 in Htx1.
    inversion Htx1 as [| | | | | | x2 Hx2 Hax01 Hx1 | | |]. symmetry in Hx1.
    (* remember (tact ASend trec) as x2 eqn:Hx2 in |- . *)
    (* remember (tsubst x0 (tact ASend trec)) eqn:Hx1.  *)
    remember (tsubst y0 (tact ASend (tact ASend trec))) as y1 eqn:Hy1; 
    exists y1.
    rewrite <- Hx1; split.
    { unfold weak; exists y0, y1; apply Pack_weak; constructor.
      rewrite Hy1, Hy0. apply do_fix. }
    { cofix CH2. apply In_sim, Pack_sim;
      [ | intros _x2 Htx; apply blts_silent1_refl in Htx; 
          rewrite <- Htx; exists y1; split; [ constructor | apply CH2] ]. 
      Guarded.

      intros _x2 ax02 Htx2.
      apply blts_weak_silent, bLTS_unwrap in Htx2. rewrite Hx1 in Htx2.
      inversion Htx2. }
  Qed.
  
  Theorem ws_yx : forall x0 y0, x0 = x -> y0 = y -> weak_sim bLTS bLTS y0 x0.
  Proof. intros x0 y0 Hx0 Hy0; unfold x, y in Hx0, Hy0.
    
    cofix CH1. apply In_sim, Pack_sim; 
    [ | intros _y1 Hty; apply blts_silent1_refl in Hty; 
        rewrite <- Hty; exists x0; split; [ constructor | apply CH1] ]. 
    Guarded.

    intros y1 ay01 Hty1. 
    apply blts_weak_silent, bLTS_unwrap in Hty1. rewrite Hy0 in Hty1.
    inversion Hty1 as [| | | | | | y2 Hy2 Hay01 Hy1 | | |]; symmetry in Hy1.
    remember (tsubst x0 (tact ASend trec)) as x1 eqn:Hx1; 
    exists x1.
    rewrite <- Hy1; split.
    { unfold weak. exists x0, x1. apply Pack_weak; constructor.
      rewrite Hx1, Hx0. apply do_fix. }
    { cofix CH2. apply In_sim, Pack_sim;
      [ | intros _y2 Hty; apply blts_silent1_refl in Hty; 
          rewrite <- Hty; exists x1; split; [ constructor | apply CH2] ]. 
      Guarded.

      intros _y2 ay02 Hty2.
      apply blts_weak_silent, bLTS_unwrap in Hty2. rewrite Hy1 in Hty2.
      inversion Hty2. }
  Qed.

  Theorem bs_xy : forall x0 y0, x0 = x -> y0 = y -> weak_bisim bLTS bLTS y0 x0.
  Proof. intros x0 y0 Hx0 Hy0; unfold x, y in Hx0, Hy0. rewrite Hx0, Hy0.
    unfold weak_bisim; split; [apply ws_yx | apply ws_xy]; trivial.
  Qed.

End Test1.



Module Test2.
  Example x : term := tfix (tseq (tpar (tact ASend tend) 
                                       (tact ARecv tend)) trec).

  Example y : term := tfix (tseq (tpar (tact ASend tend) 
                                       (tact ARecv tend)) 
                                 (tseq (tpar (tact ASend tend) 
                                             (tact ARecv tend)) trec)).

  Theorem ws_xy : forall x0 y0, x0 = x -> y0 = y -> weak_sim bLTS bLTS x0 y0.
  Proof. intros x0 y0 Hx0 Hy0; unfold x, y in Hx0, Hy0. 

    cofix CH1. apply In_sim, Pack_sim; 
    (* [| intros x1 Htx; apply (@blts_wsim_silent1 x0 x1 y0 CH1 Htx)]. *)
    (* NOTE: the above can't pass CH1 to another lemma -- ill-formed. *)
    [ | intros _x1 Htx; apply blts_silent1_refl in Htx; 
        rewrite <- Htx; exists y0; split; [ constructor | apply CH1] ]. 
    Guarded.

    intros x1 ax01 Htx1. 
    apply blts_weak_silent, bLTS_unwrap in Htx1. rewrite Hx0 in Htx1.
    inversion Htx1 as [ | | | | | | x2 Hx2 Hax01 Hx1 | | | ]; symmetry in Hx1.
    compute in Hx1; rewrite <- Hx2.
    remember (tsubst y0 (tseq (tpar (tact ASend tend) 
                                    (tact ARecv tend)) 
                              (tseq (tpar (tact ASend tend) 
                                          (tact ARecv tend)) trec))) 
    as y1 eqn:Hy1; compute in Hy1; exists y1. split.
    { unfold weak; exists y0, y1; apply Pack_weak; constructor.
      rewrite Hy1, Hy0. apply do_fix. }
    { cofix CH2. apply In_sim, Pack_sim;
      [ | intros _x2 Htx; apply blts_silent1_refl in Htx; 
          rewrite <- Htx; exists y1; split; [ constructor | apply CH2] ]. 
      Guarded.

      intros _x2 ax02 Htx2.
      apply blts_weak_silent, bLTS_unwrap in Htx2. rewrite Hx2 in Htx2.
      inversion Htx2 as 
        [ | | | x2s x2c _x0 _ax02 Htx2sc Hx2s Hax12 H_x2 | | | | | | ];
      symmetry in H_x2; rename H into H_x0.

      (* TODO: *)

      inversion






      
      remember 
        (tseq (tpar (tend) (tend)) 
              (tseq (tpar (tact ASend tend) (tact ARecv tend)) 
                    (tfix (tseq (tpar (tact ASend tend) (tact ARecv tend)) 
                                (tseq (tpar (tact ASend tend) (tact ARecv tend)) trec))))) as y2 eqn:Hy2; exists y2.
      rewrite <- Hx2; split.
      { unfold weak; exists y1, y2; apply Pack_weak; constructor.
        inversion Htx2sc as 
          [ x2sl x2sr Hx2sl Hax12s Hx2c | | | | | | 
          | x2sl x2sr Hx2sl Hax12s Hx2c | | ];
        rename H into Hx2sr; symmetry in Hx2c.
        { rewrite Hy2, Hy1, Hy0. 
          (* subst. apply Htx2. *)
          (* inversion Htx2sc. *)

          (* apply do_senda, do_seq. *)
          admit. 
        }
        { rewrite Hy2, Hy1, Hy0. 
          subst. apply do_comm.
        }

        
        admit. 
        (* destruct (termLTS y1 ax02 y2). *)

        rewrite Hy2, Hy1, Hy0.


        
      

        
        apply do_seq. }
      
      
      }
  Qed.
  
  Theorem ws_yx : forall x0 y0, x0 = x -> y0 = y -> weak_sim bLTS bLTS y0 x0.
  Proof. intros x0 y0 Hx0 Hy0; unfold x, y in Hx0, Hy0.
    
    cofix CH1. apply In_sim, Pack_sim; 
    [ | intros _y1 Hty; apply blts_silent1_refl in Hty; 
        rewrite <- Hty; exists x0; split; [ constructor | apply CH1] ]. 
    Guarded.

    intros y1 ay01 Hty1. 
    apply blts_weak_silent, bLTS_unwrap in Hty1. rewrite Hy0 in Hty1.
    inversion Hty1 as [| | | | |y2 Hy2 Hay01 Hy1| | |]; symmetry in Hy1.
    remember (tsubst x0 (tact ASend trec)) as x1 eqn:Hx1; 
    exists x1.
    rewrite <- Hy1; split.
    { unfold weak. exists x0, x1. apply Pack_weak; constructor.
      rewrite Hx1, Hx0. apply do_fix. }
    { cofix CH2. apply In_sim, Pack_sim;
      [ | intros _y2 Hty; apply blts_silent1_refl in Hty; 
          rewrite <- Hty; exists x1; split; [ constructor | apply CH2] ]. 
      Guarded.

      intros _y2 ay02 Hty2.
      apply blts_weak_silent, bLTS_unwrap in Hty2. rewrite Hy1 in Hty2.
      inversion Hty2. }
  Qed.

  Theorem bs_xy : forall x0 y0, x0 = x -> y0 = y -> weak_bisim bLTS bLTS y0 x0.
  Proof. intros x0 y0 Hx0 Hy0; unfold x, y in Hx0, Hy0. rewrite Hx0, Hy0.
    unfold weak_bisim; split; [apply ws_yx | apply ws_xy]; trivial.
  Qed.

End Test2.


Module Test3.
  Example x : term := tpar (tfix (tact ASend (tact BSend trec))) 
                           (tfix (tact ARecv (tact BSend trec))). 
  Example y : term := tpar (tact ASend (tfix (tact BSend (tact ASend trec)))) 
                           (tact ARecv (tfix (tact BRecv (tact ASend trec)))). 
  
End Test3.

