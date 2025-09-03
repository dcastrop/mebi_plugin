(* Require Import MEBI.loader. *)
Require Coq.Program.Tactics.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.

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
  Lemma tau_rt_to_silent1 : 
        forall x y z, tau lts x y -> silent lts y z -> silent1 lts x z.
  Proof. intros x y z Hxy Hyz. eauto with rel_db. Qed. 
  Hint Resolve tau_rt_to_silent1 : rel_db.
  
  (****************************************************************************)
  Lemma expand_weak_to_strong : 
        forall x y a, lts x (Some a) y -> weak lts x (Some a) y.
  Proof. intros x y a Hxy. exists x, y. auto with rel_db. Qed.
  Hint Resolve expand_weak_to_strong : rel_db.
  
  (****************************************************************************)
  Lemma is_silent1 : forall x y, lts x None y -> silent1 lts x y.
  Proof. intros x y Hxy. auto with rel_db. Qed.
  Hint Resolve is_silent1 : rel_db.

  Lemma is_silent : forall x y, silent1 lts x y -> silent lts x y.
  Proof. intros x y Hxy. auto with rel_db. Qed.
  Hint Resolve is_silent : rel_db.

  (****************************************************************************)
  Lemma expand_silent1 : 
        forall x y z, silent1 lts x y -> silent lts y z -> 
                      silent1 lts x z.
  Proof. intros x y z Hxy Hyz. induction Hxy; eapply clos_rt_clos_t.
    { exact H. } { exact Hyz. } { exact H. } 
    { apply clos_t_clos_rt, IHHxy; exact Hyz. }
  Qed.
  Hint Resolve expand_silent1 : rel_db.


  Lemma do_expand_silent1 : 
        forall x z, silent1 lts x z -> 
          exists y, silent1 lts x y -> silent lts y z.
  Proof. intros x z Hxz.
    induction Hxz. 
    - exists y. intros. apply rt1n_refl.
    - exists y. intros. apply clos_t_clos_rt in Hxz. apply Hxz.
  Qed.

  (* Lemma do_expand_silent_pre_some : 
        forall x y z a, silent lts x y -> lts y (Some a) z -> 
                        silent1 lts x y.
              (* exists w, silent1 lts x y -> . *)
  Proof.
    intros x y z a Hxy Hyz.
    einduction Hxy.
    admit.
    apply is_silent1. 
    eapply IHs.

    eapply is_silent in Hxy.

    induction Hxy.
     (* eapply clos_rt_clos_t. *)
    admit.
  Admitted. *)

  (****************************************************************************)
  Lemma expand_silent1_pre_weak : 
        forall x y z a, silent1 lts x y -> lts y (Some a) z -> 
                        weak lts x (Some a) z.
  Proof. intros x y z a Hxy Hyz. unfold weak. exists y, z.
    apply Pack_weak; [ apply is_silent, Hxy | apply Hyz | apply rt1n_refl ].
  Qed.
  Hint Resolve expand_silent1_pre_weak : rel_db.
  
  Lemma expand_silent1_post_weak : 
        forall x y z a, lts x (Some a) y -> silent1 lts y z ->
                        weak lts x (Some a) z.
  Proof. intros x y z a Hxy Hyz. unfold weak. exists x, y.
    apply Pack_weak; [ apply rt1n_refl | apply Hxy | apply is_silent, Hyz ].
  Qed.
  Hint Resolve expand_silent1_post_weak : rel_db.
  
  Lemma expand_silent1_both_weak : 
        forall w x y z a, silent1 lts w x -> lts x (Some a) y -> 
                          silent1 lts y z -> weak lts w (Some a) z.
  Proof. intros w x y z a Hwx Hxy Hyz. unfold weak. exists x, y.
    apply Pack_weak; [ apply is_silent, Hwx | apply Hxy | apply is_silent, Hyz ].
  Qed.
  Hint Resolve expand_silent1_both_weak : rel_db.

  (****************************************************************************)
  Lemma expand_silent_pre_weak : 
        forall x y z a, silent lts x y -> weak lts y (Some a) z -> 
                        weak lts x (Some a) z.
  Proof. intros x y z a Hxy Hyz. 
    inversion Hyz as [ ?y [ ?z [ Hpre Hstr Hpost ]]]; exists y0, z0. 
    apply Pack_weak; [ | apply Hstr | apply Hpost ].
    induction Hxy; [ apply Hpre | ].
    eapply rt1n_trans; [ apply H | ].
    apply IHHxy; [ apply Hyz | apply Hpre ]. 
  Qed. 

  Lemma expand_silent_post_weak : 
        forall x y z a, weak lts x (Some a) y -> silent lts y z -> 
                        weak lts x (Some a) z.
  Proof. intros x y z a Hxy Hyz.
    inversion Hxy as [ ?x [ ?y [ Hpre Hstr Hpost ]]]; exists x0, y0.
    apply Pack_weak; [ apply Hpre | apply Hstr | ].
    silent_to_clos_rt; apply (@rt_trans _ _ y0 y z); [ apply Hpost | apply Hyz ].
  Qed.  
  Hint Resolve expand_silent_pre_weak expand_silent_post_weak : rel_db.
  
  (****************************************************************************)
  Lemma expand_weak_pre_silent1 : 
        forall x y z a, weak lts x (Some a) z -> silent1 lts x y -> 
                        lts y (Some a) z.
  Proof. intros x y z a Hxz Hxy.
    

    (* eauto with rel_db.
    apply expand_silent1_post_weak.

    einduction Hxz as [xy [zz [Hpre Hstr Hpost]]]. *)
  Admitted.
    
End Lemmas.


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

(****************************************************************************)
(* NOTE: *)

Ltac wsim_case_weak_action_filter_from_silent Htx :=
  let xa := fresh "xa0" in let xb := fresh "xb0" in 
  let xc := fresh "xc0" in let xd := fresh "xd0" in 
  let Hpre_tau := fresh "Hpre_tau0" in 
  let Hpre_cft := fresh "Hpre_cft0" in 
  let Hstr := fresh "Hstr0" in 
  let Hpost := fresh "Hpost0" in 
  inversion Htx as [xa [xd [[| xb xc Hpre_tau Hpre_cft] Hstr Hpost]]]; 
  [ destruct Hpost; inversion Hstr | ].

Ltac wsim_case_weak_action_filter :=
  match goal with
  (* do_fix *)
  | [ Hx : ?cx = tfix _, Htx : @weak _ _ _ ?cx (Some ?a) _ 
    |- exists ny, @weak _ _ _ ?cy (Some ?a) ny /\ _ ] => wsim_case_weak_action_filter_from_silent Htx
    
  (* do_seq_end*)
  | [ Hx : ?cx = tseq tend _, Htx : @weak _ _ _ ?cx (Some ?a) _ 
    |- exists ny, @weak _ _ _ ?cy (Some ?a) ny /\ _ ] => wsim_case_weak_action_filter_from_silent Htx
    
  (* do_par_end *)
  | [ Hx : ?cx = tpar tend  _, Htx : @weak _ _ _ ?cx (Some ?a) _ 
    |- exists ny, @weak _ _ _ ?cy (Some ?a) ny /\ _ ] => wsim_case_weak_action_filter_from_silent Htx
  
  | |- _ => idtac
  end.

Ltac wsim_case_weak_action := 
  let nx := fresh "x0" in intros nx;
  let a := fresh "a0" in intros a;
  let Htx := fresh "Htx0" in intros Htx;
  match goal with
  | [ Hx : ?cx = ?vx, Hy : ?cy = ?vy
    , Htx : @weak _ _ _ ?cx (Some a) nx 
    |- exists ny, @weak _ _ _ ?cy (Some a) ny /\ @weak_sim _ _ _ _ _ nx ny ] => 
      rewrite Hy
      (* ; rewrite Hx in Htx *)
      (* ; first [ wsim_case_weak_action_filter | idtac ] *)
  end.

(****************************************************************************)
(* NOTE: *)

Ltac wsim_case_silent_action := 
  let nx := fresh "x0" in intros nx;
  let Htx := fresh "Htx0" in intros Htx;
  match goal with
  | [ Hx : ?cx = ?vx, Hy : ?cy = ?vy
    , Htx : @silent1 _ _ _ ?cx nx 
    |- exists ny, @silent _ _ _ ?cy ny /\ @weak_sim _ _ _ _ _ nx ny ] => 
      rewrite Hy; rewrite Hx in Htx; 
      first [ wsim_case_weak_action_filter | idtac ]
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
    solve_wsim.
    {
      eexists. split. 
      {

        (* NOTE: expand [y] as far as we can *)
        eapply expand_silent1_pre_weak. 
        
        eapply expand_silent1; [ | eapply is_silent ].
        eapply is_silent1, do_fix; compute; apply eq_refl.

        eapply t1n_step; unfold tau.
        eapply do_seq, do_comm.

        eapply do_seq.
        (* eapply do_handshake.  *)
        (* NOTE: we can't apply the above since we don't know what [a0] is *)

        (* NOTE: first need to determine [a0] -- by expanding [Htx0] *)
        induction Htx0 as [x0b [x1a [Hpre Hstr Hpost]]].

        (* NOTE: it cannot be that [x0=x0b] *)
        destruct Hpre; [ rewrite Hx0 in Hstr; inversion Hstr | ].
        
        (* NOTE: turn [H] and [Hpre] into [silent1 x0 z] *)
        eapply tau_rt_to_silent1 in H; [ | apply Hpre ].

        eapply do_expand_silent1 in H as [].


  Admitted.

        rewrite Hx0 in H. unfold tau in H.
        inversion H; compute in H1. rewrite 
        eapply do_fix in H. 

        (* induction a0.
        admit.

        inversion Hpre. 
        - rewrite <- H, Hx0 in Hstr. inversion Hstr.
        - 


        induction Hpre; [ rewrite Hx0 in Hstr; inversion Hstr | ].




        eapply do_expand_silent1 in Hpre.

        eapply (@expand_silent _ _ _ x0 x0b x1a) in Hpre; [| apply Hstr].

        eapply do_expand_silent1 in Hpre; destruct Hpre as [x0a H]; rewrite Hx0 in H. 
        einduction H; [ | | apply is_silent1, do_fix; compute ].
        admit. admit.
        
        
        
        
        apply is_silent1 in H.
        apply do_fix; compute.
 
        admit.
        
        admit.
        


        (* Check expand_silent1. *)
        eapply (@expand_silent1 _ _ _ x0 x0b) in Hpre.



        eapply Relation_Operators.rt1n_trans in Hpre. *)





        (* NOTE: now that we know [a0] -- expand y *)
        eapply do_handshake. 
        (* eapply do_comm.  *)



        (* edestruct ?[t']. *)
        einduction ?[t'].
        - discriminate Hx0.
        - discriminate Hx0.
        - discriminate Hx0.


        einduction (termLTS (tpar (tact (send A) tend) (tact (recv A) tend)) (Some a0) ?t').


        (* { *)

        (* }  *)


        (* unfold weak. do 2 eexists. apply Pack_weak. *)

        (* eapply do_fix. *)


        unfold weak. eapply silent_leading_weakly_visible.
        - eapply expand_silent1. 
          (* can keep expanding until we find the right term. *)
          + eapply is_silent1. eapply do_fix; compute. trivial.
          + eapply is_silent, is_silent1. eapply do_seq, do_comm. 
        - eapply do_seq.
          eapply do_handshake. 
          eauto with rel_db. 
      }

    }

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

