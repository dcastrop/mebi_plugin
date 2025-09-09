(* Require Import MEBI.loader. *)
Require Coq.Program.Tactics.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.

(****************************************************************************)
(* NOTE: makes goals easier to read, e.g., [H : tend = t] -> [H : t = tend] *)
Ltac ltr_goals :=
  repeat match goal with
  | [ t : term, H : tend       = ?t |- _ ] => symmetry in H
  | [ t : term, H : trec       = ?t |- _ ] => symmetry in H
  | [ t : term, H : tfix _     = ?t |- _ ] => symmetry in H
  | [ t : term, H : tact _ _   = ?t |- _ ] => symmetry in H
  | [ t : term, H : tseq _ _   = ?t |- _ ] => symmetry in H
  | [ t : term, H : tpar _ _   = ?t |- _ ] => symmetry in H
  | [ t : term, H : tsubst _ _ = ?t |- _ ] => symmetry in H
  | |- _  => idtac
  end.

(* NOTE: computes certain fun, e.g., [tsubst _ _] for recursive unfoldings *)
Ltac compute_goals :=
  repeat match goal with
  | [ t : term, H : ?t = tsubst _ _ |- _ ] => compute in H
  | |- _ => idtac
  end.

(* NOTE: reformats all goals in a consistent way, resolves certain fun calls *)
Ltac fmt_goals := ltr_goals; compute_goals.

(* NOTE: puts terms into the hypothesis (useful when they get too long). *)
Ltac move_terms_to_hypothesis :=
  ltr_goals;
  match goal with
  (* do nothing if both are vars with defined goals *)
  | [ Hx0 : ?xv0 = ?xt0, Hy0 : ?yv0 = ?yt0 
    |- @weak_sim _ _ _ _ _ ?xv0 ?yv0 ] => idtac
  (* otherwise, repackage the terms as variables in the goal *)
  | |- _ =>
    repeat match goal with
    (* if [x] term is defined and unpacked then repack *)
    | [ Hx0 : ?xv0 = ?xt0, Hx1 : ?xv1 = ?xt1
      , Htx : @weak _ _ _ ?xv0 (Some _) ?xv1
      |- @weak_sim _ _ _ _ _ ?xt1 _ ] => rewrite <- Hx1
    (* if [y] term is defined and unpacked then repack *)
    | [ Hx1 : ?xv1 = ?xt1, Hy1 : ?yv1 = ?yt1
      |- @weak_sim _ _ _ _ _ ?xv1 ?yt1 ] => rewrite <- Hy1
    (* if current hypothesis has unpackaged y term: *)
    | [ Hx0 : ?xv0 = ?xt0
      (* , Hx1 : ?xv1 = ?xt1 *)
      , Htx : @weak _ _ _ ?xv0 (Some _) ?xv1
      |- @weak_sim _ _ _ _ _ ?xv1 _ ] => 
      let y1 := fresh "y0" in let Hy1 := fresh "Hy0" in 
      match goal with 
      | |- @weak_sim _ _ _ _ _ ?xv1 tend                 => remember tend               as y1 eqn:Hy1
      | |- @weak_sim _ _ _ _ _ ?xv1 trec                 => remember trec               as y1 eqn:Hy1
      | |- @weak_sim _ _ _ _ _ ?xv1 (tfix ?yt1)          => remember (tfix yt1)         as y1 eqn:Hy1
      | |- @weak_sim _ _ _ _ _ ?xv1 (tact ?yt1a ?yt1b)   => remember (tact yt1a yt1b)   as y1 eqn:Hy1
      | |- @weak_sim _ _ _ _ _ ?xv1 (tseq ?yt1a ?yt1b)   => remember (tseq yt1a yt1b)   as y1 eqn:Hy1
      | |- @weak_sim _ _ _ _ _ ?xv1 (tseq ?yt1a ?yt1b)   => remember (tseq yt1a yt1b)   as y1 eqn:Hy1
      | |- @weak_sim _ _ _ _ _ ?xv1 (tpar ?yt1a ?yt1b)   => remember (tpar yt1a yt1b)   as y1 eqn:Hy1
      | |- @weak_sim _ _ _ _ _ ?xv1 (tsubst ?yt1a ?yt1b) => remember (tsubst yt1a yt1b) as y1 eqn:Hy1
      end
    end
  end.

(****************************************************************************)
Require Import Notations.
Require Import List.
Import ListNotations.
  
Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

(****************************************************************************)
Ltac silent_to_clos_rt :=
  repeat match goal with
  | [ H : @silent _ _ _ ?x ?y |- _ ] => apply Operators_Properties.clos_rt1n_rt in H
  | [ |- @silent _ _ _ ?x ?y ] => apply Operators_Properties.clos_rt_rt1n
  | |- _ => idtac
  end.

(****************************************************************************)
Section Lemmas.
  Context {M A : Type} (lts : LTS M A).
  
  (****************************************************************************)
  Lemma tau_is_silent1 : forall x y, lts x None y -> silent1 lts x y.
  Proof. intros x y Hxy. info_auto with rel_db. Qed.

  Lemma silent1_is_silent : forall x y, silent1 lts x y -> silent lts x y.
  Proof. intros x y Hxy. info_auto with rel_db. Qed.

  Lemma tau_silent_is_silent1 : 
        forall x y z, tau lts x y -> silent lts y z -> silent1 lts x z.
  Proof. intros x y z Hxy Hyz. info_eauto with rel_db. Qed. 
  
  Lemma strong_is_weak : 
        forall x y a, lts x (Some a) y -> weak lts x (Some a) y.
  Proof. intros x y a Hxy. exists x, y. info_auto with rel_db. Qed.
  
  (****************************************************************************)
  Lemma expand_silent1 : 
        forall x y z, silent1 lts x y -> silent lts y z -> 
                      silent1 lts x z.
  Proof. intros x y z Hxy Hyz. induction Hxy; eapply clos_rt_clos_t.
    { exact H. } { exact Hyz. } { exact H. } 
    { apply clos_t_clos_rt, IHHxy; exact Hyz. }
  Qed.

  Lemma do_expand_silent1 : 
        forall x z, silent1 lts x z -> 
          exists y, silent1 lts x y -> silent lts y z.
  Proof. intros x z Hxz.
    induction Hxz; exists y; intros; 
      [apply rt1n_refl | apply clos_t_clos_rt in Hxz; apply Hxz ].
  Qed.

  (****************************************************************************)
  Lemma silent1_pre_weak : 
        forall x y z a, silent1 lts x y -> lts y (Some a) z -> 
                        weak lts x (Some a) z.
  Proof. intros x y z a Hxy Hyz. unfold weak. exists y, z.
    apply Pack_weak; [ apply silent1_is_silent, Hxy | apply Hyz | apply rt1n_refl ].
  Qed.
  
  Lemma silent1_post_weak : 
        forall x y z a, lts x (Some a) y -> silent1 lts y z ->
                        weak lts x (Some a) z.
  Proof. intros x y z a Hxy Hyz. unfold weak. exists x, y.
    apply Pack_weak; [ apply rt1n_refl | apply Hxy | apply silent1_is_silent, Hyz ].
  Qed.
  
  Lemma silent1_both_weak : 
        forall w x y z a, silent1 lts w x -> lts x (Some a) y -> 
                          silent1 lts y z -> weak lts w (Some a) z.
  Proof. intros w x y z a Hwx Hxy Hyz. unfold weak. exists x, y.
    apply Pack_weak; 
      [ apply silent1_is_silent, Hwx | apply Hxy | apply silent1_is_silent, Hyz ].
  Qed.

  (****************************************************************************)
  Lemma silent_weak_expands_weak : 
        forall x y z a, silent lts x y -> weak lts y (Some a) z -> 
                        weak lts x (Some a) z.
  Proof. intros x y z a Hxy Hyz. 
    inversion Hyz as [ ?y [ ?z [ Hpre Hstr Hpost ]]]; exists y0, z0. 
    apply Pack_weak; [ | apply Hstr | apply Hpost ].
    induction Hxy; [ apply Hpre | ].
    eapply rt1n_trans; [ apply H | ].
    apply IHHxy; [ apply Hyz | apply Hpre ]. 
  Qed. 

  Lemma weak_silent_expands_weak : 
        forall x y z a, weak lts x (Some a) y -> silent lts y z -> 
                        weak lts x (Some a) z.
  Proof. intros x y z a Hxy Hyz.
    inversion Hxy as [ ?x [ ?y [ Hpre Hstr Hpost ]]]; exists x0, y0.
    apply Pack_weak; [ apply Hpre | apply Hstr | ].
    silent_to_clos_rt; apply (@rt_trans _ _ y0 y z); [ apply Hpost | apply Hyz ].
  Qed.  

  (****************************************************************************)
  Lemma do_expand_weak_pre_silent1 : 
        forall x y z a, weak lts x (Some a) z -> silent1 lts x y -> 
                        weak lts y (Some a) z ->
            exists w v, silent lts y w -> lts w (Some a) v -> silent lts v z.
  Proof. intros x y z a Hxz Hxy Hyz. info_eauto with rel_db. Qed.
End Lemmas.

(****************************************************************************)
Ltac wsim_case_weak_action_filter_from_silent Htx :=
  let xa := fresh "xa0" in let xb := fresh "xb0" in 
  let xc := fresh "xc0" in let xd := fresh "xd0" in 
  let Hpre_tau := fresh "Hpre_tau0" in 
  let Hpre_cft := fresh "Hpre_cft0" in 
  let Hstr := fresh "Hstr0" in 
  let Hpost := fresh "Hpost0" in 
  inversion Htx as [xa [xd [[| xb xc Hpre_tau Hpre_cft] Hstr Hpost]]]; 
  [ destruct Hpost; inversion Hstr | ].

(****************************************************************************)
Ltac wsim_case_weak_action := 
  let nx := fresh "x0" in intros nx;
  let a := fresh "a0" in intros a;
  let Htx := fresh "Htx0" in intros Htx;
  match goal with
  | [ Hx : ?cx = ?vx, Hy : ?cy = ?vy
    , Htx : @weak _ _ _ ?cx (Some a) nx 
    |- exists ny, @weak _ _ _ ?cy (Some a) ny /\ @weak_sim _ _ _ _ _ nx ny ] => 
      rewrite Hy
  end.

Ltac wsim_case_silent_action := 
  let nx := fresh "x0" in intros nx;
  let Htx := fresh "Htx0" in intros Htx;
  match goal with
  | [ Hx : ?cx = ?vx, Hy : ?cy = ?vy
    , Htx : @silent1 _ _ _ ?cx nx 
    |- exists ny, @silent _ _ _ ?cy ny /\ @weak_sim _ _ _ _ _ nx ny ] => 
      rewrite Hy
  end.

(****************************************************************************)
(* NOTE: called at the beginning of each new proof that terms are wsim. *)
Ltac wsim_new_cofix :=
  let CH := fresh "CH0" in cofix CH; 
  apply In_sim, Pack_sim; [ wsim_case_weak_action | wsim_case_silent_action ].
Tactic Notation "wsim_next" := wsim_new_cofix.

(* NOTE: entrypoint, handles first cofix before [wsim_new_cofix]. *)
Ltac wsim_pre_cofix_unfold :=
  let x := fresh "x0" in intros x;
  let y := fresh "y0" in intros y;
  let Hx := fresh "Hx0" in intros Hx;
  let Hy := fresh "Hy0" in intros Hy;
  match goal with 
  | [ Hx : x = ?vx, Hy : y = ?vy 
    |- @weak_sim _ _ _ _ _ x y ] => unfold vx, vy in Hx, Hy; wsim_new_cofix
  end.
Tactic Notation "solve_wsim" := wsim_pre_cofix_unfold.

(****************************************************************************)
Section Test2.
  Example p : term := tfix (tseq (tpar (tact (send A) tend) 
                                       (tact (recv A) tend)) trec).

  Example q : term := tfix (tseq (tpar (tact (recv A) tend) 
                                       (tact (send A) tend)) 
                                 (tseq (tpar (tact (send A) tend) 
                                             (tact (recv A) tend)) trec)).
  
  Example r : term := tseq (tpar (tact (recv A) tend) 
                                 (tact (send A) tend)) 
                           (tfix (tseq (tpar (tact (send A) tend) 
                                             (tact (recv A) tend)) trec)).

  (****************************************************************************)
  (* NOTE: quick checks to see if current ltac breaks any of the other cases *)
  Example wsim_test_pq : forall x y, x = p -> y = q -> weak_sim termLTS termLTS x y. Proof. solve_wsim. Admitted.
  Example wsim_test_pr : forall x y, x = p -> y = r -> weak_sim termLTS termLTS x y. Proof. solve_wsim. Admitted.
  Example wsim_test_qp : forall x y, x = q -> y = q -> weak_sim termLTS termLTS x y. Proof. solve_wsim. Admitted.
  Example wsim_test_qr : forall x y, x = q -> y = r -> weak_sim termLTS termLTS x y. Proof. solve_wsim. Admitted.
  Example wsim_test_rp : forall x y, x = r -> y = p -> weak_sim termLTS termLTS x y. Proof. solve_wsim. Admitted.
  Example wsim_test_rq : forall x y, x = r -> y = q -> weak_sim termLTS termLTS x y. Proof. solve_wsim. Admitted.

  Require Import Coq.Program.Equality.

  (****************************************************************************)
  (* NOTE: below is the "hands-on" proof for testing *)
  Example wsim_testing : forall x y,
    x = p -> y = q ->
    (* x = p -> y = r -> *)
    (* x = q -> y = p -> *)
    (* x = q -> y = r -> *)
    (* x = r -> y = p -> *)
    (* x = r -> y = q -> *)
    weak_sim termLTS termLTS x y.
  Proof.
    intros; subst; unfold p, q.

    (* NOTES:
     - Removed the use of induction, but induction may be needed somewhere in
     the below proof due to cycles in the LTS. We should recognise this every
     time that, by saturation, we produce terms that we had already considered.
     - We may avoid the use of induction if we can produce some lemma that allows
     us to consider *only* distinct states. Not sure what that would look like.
     *)
    cofix CH.
    apply In_sim, Pack_sim; intros.
    { repeat destruct H.
      inversion pre; subst; [inversion str|].
      inversion H; subst; unfold tsubst in H, H0; clear H.
      inversion H0; subst.
      { clear pre H0. inversion str; subst.
        inversion H3; subst. clear H2 H3 str.
        unfold silent in post.
        inversion post; subst.
        { clear post.
          eexists. split. econstructor. eexists. constructor.
          eapply rt1n_trans. econstructor. unfold tsubst. reflexivity.
          eapply rt1n_trans. unfold tau. eapply do_seq. eapply do_comm.
          eapply rt1n_refl.
          constructor. constructor.
          eapply rt1n_trans. eapply do_seq, do_par_end.
          eapply rt1n_trans. eapply do_seq_end.
          eapply rt1n_refl.

          cofix CH0.
          apply In_sim , Pack_sim; intros.
          { repeat destruct H.
            inversion pre; subst; [inversion str; subst; inversion H3|].
            inversion H; subst; unfold tsubst in H, H0; clear H.
            inversion H5; subst.
            { clear H5. inversion H0; subst; [inversion str; subst; inversion H4|].
              inversion H; subst; [inversion H6; subst |].
              clear H.
              inversion H1; subst; [inversion str|].
              inversion H; subst. unfold tsubst in *.
              clear H. inversion H2; subst.
              { clear H0 H1 H2. inversion str; subst. inversion H3; subst.
                clear H2 str pre H3.
                inversion post; subst.
                { clear post; eexists; split.
                  eexists. eexists. eapply Pack_weak.
                  eapply rt1n_refl.
                  constructor. constructor.
                  eapply rt1n_trans. eapply do_seq, do_par_end.
                  eapply rt1n_trans. eapply do_seq_end.
                  eapply rt1n_refl.
                  (* Cannot use CH0 due to ill guarded recursion *)

                  cofix CH1.
          apply In_sim , Pack_sim; intros.
          { repeat destruct H.
            inversion pre; subst; [inversion str; subst; inversion H3|].
            inversion H; subst; unfold tsubst in H, H0; clear H.
            inversion H5; subst.
            { clear H5. inversion H0; subst; [inversion str; subst; inversion H4|].
              inversion H; subst; [inversion H6; subst |].
              clear H.
              inversion H1; subst; [inversion str|].
              inversion H; subst. unfold tsubst in *.
              clear H. inversion H2; subst.
              { clear H0 H1 H2. inversion str; subst. inversion H3; subst.
                clear H2 str pre H3.
                inversion post; subst.
                { clear post; eexists; split.
                  eexists. eexists. eapply Pack_weak.
                  eapply rt1n_trans. eapply do_fix. unfold tsubst. reflexivity.
                  eapply rt1n_trans. eapply do_seq. eapply do_comm.
                  eapply rt1n_refl.
                  constructor. constructor.
                  eapply rt1n_trans. eapply do_seq, do_par_end.
                  eapply rt1n_trans. eapply do_seq_end.
                  eapply rt1n_refl.
                  eapply CH0.
                }
                (* NOTE: continuing from here *)
                { clear post; eexists; split. 
                  do 2 eexists. apply Pack_weak. 
                  eapply rt1n_trans. apply do_fix. unfold tsubst. reflexivity.
                  eapply rt1n_trans. apply do_seq, do_comm.
                  eapply rt1n_refl.
                  constructor. constructor.
                  eapply rt1n_trans. apply do_seq, do_par_end. 
                  eapply rt1n_trans. apply do_seq_end.
                  apply rt1n_refl.

                  inversion H; subst.
                  eapply rt1n_trans in H0; [|apply H]; clear H H5 t'.
                  inversion H0; subst; [apply CH0|]. clear H0.
                  
                  inversion H; subst. inversion H5; subst.
                  { eapply rt1n_trans in H1; [|apply H]. clear H H5.
                    inversion H1; subst; [apply CH0|]. clear H0.

                    inversion H; subst. inversion H5; subst.


                  }


                  (* inversion H; subst.
                  eapply rt1n_trans in H1; [|apply H]; clear H H5 t'.
                  inversion H1; subst; [apply CH0|]. clear H1.

                  inversion H; subst.
                  eapply rt1n_trans in H0; [|apply H]; clear H H5 t'.
                  inversion H0; subst; [apply CH0|]. clear H0.

                  inversion H; subst.
                  eapply rt1n_trans in H1; [|apply H]; clear H H5 t'.
                  inversion H1; subst; [apply CH0|]. clear H1.

                  eapply rt1n_trans in H1; [|apply H].
                  
                  clear H H5 t'.
                  inversion H1; subst.
                  { clear H1. inversion H6; subst.



                  }
                  {

                  } 
                    inversion H6; subst.
                    admit. 



                   *)

                  (* NOTE: CHm2 stops repeatitive cases later on *)
                  cofix CHm2.
                  inversion H0; subst.
                  { apply CHm2. Guarded. } (* Without CHm2 would go on forever *)
                  { 
                    cofix CHm2_1.
                    clear H0. inversion H; subst.
                    eapply rt1n_trans in H1; [|apply H]; clear H H5 t'.
                    inversion H1; subst.
                    { apply CHm2_1. Guarded.  
                    clear H1. cofix CH2.
                      admit.
                    }
                    { apply CHm2_1. Guarded.  }
                  }
                  
                  [eapply CH0|].

                  (* NOTE: handle each case of [m2] *)
              inversion H; subst. inversion H5; subst.
              { clear H post. inversion_clear H0; subst; [clear m2|].
                { (* NOTE: case where [m2 = tseq tend (tfix ...)] *)
              cofix CH2.
              admit.
                }
                { inversion H; subst; [inversion H6|].
                  clear H H5. inversion_clear H1; subst; [clear m2|].
                  { (* NOTE: [m2 = tseq (tfix ...) trec] *)
              cofix CH2. 
              admit.
                  }
                  { inversion H; subst; simpl in H, H0. 
                    clear H. inversion_clear H0; subst; [clear m2|].
                    { (* NOTE: [m2 = tseq (tpar send recv) (tfix ...)] *)
              cofix CH2.
              admit.
                    }
                    { inversion H; subst. inversion H5; subst.
                      clear H H5. inversion_clear H1; subst; [clear m2|].
                      { (* NOTE: [m2 = tseq (tpar recv send) (tfix ...)]*)
              cofix CH2.
              admit.
                      }
                      { inversion H; subst. inversion H5; subst.
                        clear H H5. inversion_clear H0; subst; [clear m2|].
                        { (* NOTE: [m2 = tseq (tpar send recv) (tfix ...)] *)
              (* NOTE: same as 2 cases above  *)
              admit.
                        } 
                        {
              (* NOTE: will be same as 2 cases above  *)
              admit.      
                        }
                      }
                    }
                  }
                }
              }
              { clear H5 H post. 
                inversion_clear H0; subst; [clear m2|].
                { (* NOTE: [m2 = tseq (tpar tend tend) (tfix ...)] *)
            cofix CH2.
            admit.
                }
                { inversion H; subst. inversion H5; subst. 
                  { (* NOTE: tpar tend tend => tend *)
                    clear H H5. inversion_clear H1; subst; [clear m2|].
                    { (* NOTE: [m2 = tseq tend (tfix ...)] *)
            cofix CH2.
            admit.
                    } 
                    { inversion H; subst; [inversion H5|].
                      clear H. inversion_clear H0; subst; [clear m2|].
                      { (* NOTE: [m2 = tfix ...] *)
            cofix CH2.
            admit.
                      }
                      { inversion H; subst. simpl in H, H1. 
                        clear H. inversion_clear H1; subst; [clear m2|].
                        { (* NOTE: [m2 = tseq (tpar send recv) (tfix ...)] *)
            cofix CH2.
            admit.
                        }
                        { inversion H; subst. inversion H5; subst.
                          clear H H5. inversion_clear H0; subst; [clear m2|].
                          { (* NOTE: [m2 = tseq (tpar recv send) (tfix ...)] *)
            cofix CH2.
            admit.
                          }
                          { inversion H; subst. inversion H5; subst.
                            clear H H5. inversion_clear H1; subst; [clear m2|].
                            { (* NOTE: [m2 = tseq (tpar send recv) (tfix ...)] *)
            (* NOTE: same as case 2 above *)
            admit.
                            }
                            {
            (* NOTE: will be same as 2 cases above  *)
            admit.
                            }
                          }
                        }
                      }
                    }
                  }
                  { (* NOTE: tpar tend tend => tpar tend tend *)
                    clear H5 H. inversion_clear H1; [clear m2|].
                    { (* NOTE: [m2 = tseq (tpar tend tend) (tfix ...)] *)
            cofix CH2.
            admit.
                    }
                    { inversion H; subst. inversion H5; subst. 
                      { (* NOTE: tpar tend tend => tend *)
            (* NOTE: same as an outer-case *)
            admit.
                      }
                      { (* NOTE: tpar tend tend => tpar tend tend *)
            (* NOTE: same as an outer-case *)
            admit.
                      }
                    }
                  }
                }
              }
            }
          }
          { clear H H0 H1 H2 H3 y. 
            inversion pre; subst; [inversion str; subst; inversion H3|].
            inversion H; subst. inversion H5; subst.
            { clear H H5 pre. 
              inversion H0; subst; [inversion str; subst; inversion H4|].
              inversion H; subst; [inversion H6|]. clear H H0.
              inversion H1; subst; [inversion str|]. clear H1. 
              inversion H; subst. simpl in H0, H. clear H. 
              inversion H0; subst.
              { clear H0. inversion str; subst. 
                inversion H3; subst. clear str H2 H3.
                inversion post; subst.
                { clear post. eexists. split.
                  do 2 eexists. eapply Pack_weak.
                  eapply rt1n_trans. apply do_fix. simpl; reflexivity.
                  eapply rt1n_trans. apply do_seq, do_comm.
                  eapply rt1n_refl. apply do_seq, do_handshake. 
                  eapply rt1n_trans. apply do_seq, do_par_end.
                  eapply rt1n_trans. apply do_seq_end.
                  eapply rt1n_refl.
                  apply CH0.
                }
                { clear post. eexists. split.
                  do 2 eexists. eapply Pack_weak.
                  eapply rt1n_trans. apply do_fix. simpl; reflexivity.
                  eapply rt1n_trans. apply do_seq, do_comm.
                  eapply rt1n_refl. apply do_seq, do_handshake. 
                  eapply rt1n_trans. apply do_seq, do_par_end.
                  eapply rt1n_trans. apply do_seq_end.
                  eapply rt1n_refl.

                  inversion H; subst. 
                  eapply rt1n_trans in H0; [|apply H]; clear H H5.
                   
                  inversion H0; subst; [apply CH0|]. 

                }
              }
              {

              }
              



            } 
            {

            }
            
            ; unfold tsubst in H, H0; clear H.
            inversion H5; subst.

          inversion pre. 
            inversion H0.
            
            clear H0 H1 H2. inversion str; subst. inversion H3; subst.
            clear H2 str pre H3.
            inversion post; subst.
            { clear post; eexists; split.
              eexists. eexists. eapply Pack_weak.
              eapply rt1n_trans. eapply do_fix. unfold tsubst. reflexivity.
              eapply rt1n_trans. eapply do_seq. eapply do_comm.
              eapply rt1n_refl.
              constructor. constructor.
              eapply rt1n_trans. eapply do_seq, do_par_end.
              eapply rt1n_trans. eapply do_seq_end.
              eapply rt1n_refl.
              eapply CH0.
            }
            admit.
          } }
          { admit.
          } }
          { admit.
          } }
          { admit.
          } }
          { admit.
          } }
          { admit.
          } }
          { admit.
          } }
          { admit.
          } }
          { admit.
          } }
          { admit.
          } 
          


    (*     } *)
    (*     inversion post. *)
    (*   } *)
    (*   unfold tau in H. *)
    (*   inversion H; subst. *)
    (*   (* Need induction due to commutativity *) *)
    (*   inversion H6; subst. *)
    (*   inversion H; subst; inversion H6; subst. *)
    (*   inversion H1; subst. inversion str; subst. *)

    (*   { *)
    (*     admit. *)
    (*     inversion H1. *)
    (*   } *)
    (* } *)



    (* solve_wsim. *)
    (* { eexists.  *)
    (*   (* NOTE: first need to determine [a0] -- by expanding [Htx0] *) *)
    (*   (* NOTE: [einduction] keeps [Htx0] -- used by [pack_hyp_weak] *) *)
    (*   einduction Htx0 as [x0b [x1a [Hpre Hstr Hpost]]]. *)

    (*   (* NOTE: it cannot be that [x0 = x0b] *) *)
    (*   inversion Hpre as [ Heqx0b | x0c x0d Htx0c Htx0cb Heqx0db ]; *)
    (*     [ rewrite <- Heqx0b, Hx0 in Hstr; inversion Hstr | ]; subst x0d. *)

    (*   rewrite Hx0 in Htx0c; inversion Htx0c. subst t; subst t'; rename H0 into Hx0c. *)

    (*   inversion Htx0cb; subst x0b. *)
    (*   { rewrite Hx0c in Hstr; inversion Hstr. subst t; subst a; subst s; subst x1a. *)
    (*     (* NOTE: determine that [a0 = A] *) *)
    (*     inversion H3. subst a; subst l; subst r0; subst a0; subst t'; *)
    (*       clear H2; rename H3 into Htx0_inner. *)

    (*     (* NOTE: we can now resolve [y1] *) *)
    (*     split. *)
    (*     { (* NOTE: expand [y] as far as we can *) *)
    (*       eapply silent1_pre_weak; [ | eapply do_seq, do_handshake ].  *)
    (*       eapply expand_silent1; [ | eapply silent1_is_silent ]. *)
    (*         { eapply tau_is_silent1, do_fix; compute; apply eq_refl. } *)
    (*         { eapply t1n_step; unfold tau. eapply do_seq, do_comm. } } *)
    (*     { (* NOTE: package [y1] *) *)
    (*       move_terms_to_hypothesis. *)

    (*       (* NOTE: need determine [x1] *) *)
    (*       (* wsim_next. *) *)
    (*       admit.  *)
    (*     } } *)
    (*   { (* NOTE: handle the case where [do_comm] for ever *) *)
    (*     rewrite Hx0c in H; inversion H; fmt_goals. *)
    (*       subst t; subst s; subst a; subst y. *)
    (*     admit.  *)
    (*   } } *)
    (* { exists y0. split; [ rewrite <- Hy0; eapply rt1n_refl |].  *)

    (*   (* NOTE: determine [x1] *) *)
    (*   inversion Htx0; [ subst y | subst z; rename y into x0b ]. *)
    (*   { (* NOTE: case [termLTS x0 None x1] *) *)
    (*     rewrite Hx0 in H; inversion_clear H; rename H0 into Hx1. *)

    (*     wsim_next. *)
    (*     admit. admit.  *)
    (*   } *)
    (*   { (* NOTE: case [termLTS x0 None x0b /\ silent termLTS x0b x1]*) *)
    (*     rewrite Hx0 in H; inversion_clear H;  *)
    (*       rename H0 into Htxb1; rename H1 into Hx0b. *)

    (*     (* NOTE: need to determine [x1] *) *)
    (*     (* wsim_next. *) *)
    (*     admit. } *)
    (* } *)
  Admitted.
                            
(****************************************************************************)

  Theorem wsim_xy : forall x y, x = p -> y = q -> weak_sim termLTS termLTS x y.
  Proof. 
  Admitted.

  Theorem wsim_yx : forall x y, x = q -> y = p -> weak_sim termLTS termLTS x y.
  Admitted.

  Theorem wbisim_xy 
  : forall x y, x = p -> y = q -> weak_bisim termLTS termLTS x y.
  Proof.
    intros x0 y0 Hx0 Hy0; unfold q, p in Hx0, Hy0. 
    rewrite Hx0, Hy0; unfold weak_bisim. 
    split; [ apply wsim_xy | apply wsim_yx ]; trivial.
  Qed.
End Test2.

(* TODO *)
Module Test3.
  Example p : term := tpar (tfix (tact (send A) (tact (send B) trec))) 
                           (tfix (tact (recv A) (tact (recv B) trec))). 
  Example q : term := tpar (tact (send A) (tfix (tact (send B) (tact (send A) trec)))) 
                           (tact (recv A) (tfix (tact (recv B) (tact (recv A) trec)))). 
  
  Theorem ws_xy : forall x0 y0, x0 = p -> y0 = q -> weak_sim termLTS termLTS x0 y0.
  Proof. 
    intros x0 y0 Hx0 Hy0; unfold p, q in Hx0, Hy0. 
  Admitted.

  Theorem ws_yx : forall x0 y0, x0 = q -> y0 = p -> weak_sim termLTS termLTS x0 y0.
  Proof. 
    intros x0 y0 Hx0 Hy0; unfold p, q in Hx0, Hy0.
  Admitted.

  Theorem bs_xy : forall x0 y0, x0 = p -> y0 = q -> weak_bisim termLTS termLTS x0 y0.
  Proof. 
    intros x0 y0 Hx0 Hy0; unfold p, q in Hx0, Hy0. 
    rewrite Hx0, Hy0; unfold weak_bisim. 
    split; [ apply ws_xy | apply ws_yx ]; trivial.
  Qed.
End Test3.

