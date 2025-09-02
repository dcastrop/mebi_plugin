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

  Lemma is_strong : forall t1 t2 a, 
    termLTS t1 (Some a) t2 -> weak termLTS t1 (Some a) t2.
  Proof. intros. exists t1, t2. apply Pack_weak; try constructor; apply H. Qed.

  Lemma is_silent1 : forall t1 t2, termLTS t1 None t2 -> silent1 termLTS t1 t2.
  Proof. intros. inversion H; constructor; constructor; apply H0. Qed.

  Lemma is_silent : forall x y, silent1 termLTS x y -> silent termLTS x y.
  Proof. intros. eauto with rel_db. Qed.

  (* NOTE: x => y *)
  (* Lemma do_weak : forall x0 x1 y0 y1 y2 y3 y4 a, 
    weak_sim termLTS termLTS x0 y0 ->
    weak termLTS x0 (Some a) x1 ->
    termLTS y0 None y1 -> 
    silent termLTS y1 y2 ->
    termLTS y2 (Some a) y3 -> 
    silent termLTS y3 y4 ->
    weak termLTS y0 (Some a) y4.
  Proof. intros ? ? ? ? y2 y3; intros. exists y2, y3. eauto with rel_db. Qed. *)

  (* Lemma wsim_explore_silent1 : 
    forall x0 y0, weak_sim termLTS termLTS x0 y0 ->
    forall x1, silent1 termLTS x0 x1 ->
    exists x1a, termLTS x0 None x1a /\ silent termLTS x1a x1.
  Proof. intros. inversion H0; subst; [ exists x1 | exists y ].
    - split; [ apply H1 | apply rt1n_refl ].
    - split; [ apply H1 | apply clos_t_clos_rt in H2; apply H2 ].
  Qed. *)

  (* Lemma wsim_explore_silent :
    forall x0 y0, weak_sim termLTS termLTS x0 y0 ->
    forall x1, silent1 termLTS x0 x1 ->
    (exists y1, silent termLTS y0 y1 /\ weak_sim termLTS termLTS x1 y1) ->
    (weak_sim termLTS termLTS x1 y0) \/ (
      exists y1a y1, termLTS y0 None y1a /\ silent termLTS y1a y1
    ).
  Proof.
    intros. 
    apply (@wsim_explore_silent1 x0 y0) in H0; [| apply H].

    inversion H0 as [x1a []]. inversion H1 as [y1a []].

    inversion H4; subst; [ left; apply H5 | right; exists y1a ].

    inversion H7; exists y1a; subst; [ split; [ apply H6 | apply rt1n_refl ] |].
    
    split; [| apply rt1n_refl].

    (* NOTE: from here we need the information from the plugin. *)
    (* NOTE: I.e., next state reached *)


  Admitted. *)

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
      (* NOTE: are we to use the plugin to provide the following? *)
      remember (tsubst y0 (tseq (tpar (tact (recv A) tend) 
                                      (tact (send A) tend)) 
                                (tseq (tpar (tact (send A) tend) 
                                            (tact (recv A) tend)) trec))) 
      as y1 eqn:Hy1; compute in Hy1; rewrite Hy0 in Hy1. 

      remember (tseq (tpar (tact (send A) tend) 
                           (tact (recv A) tend)) 
                     (tseq (tpar (tact (send A) tend) 
                                 (tact (recv A) tend)) 
                           (tfix (tseq (tpar (tact (recv A) tend) 
                                             (tact (send A) tend)) 
                                       (tseq (tpar (tact (send A) tend) 
                                                   (tact (recv A) tend)) trec)))))
      as y2 eqn:Hy2.

      remember (tseq (tpar tend tend) 
                     (tseq (tpar (tact (send A) tend) 
                                 (tact (recv A) tend)) 
                           (tfix (tseq (tpar (tact (recv A) tend) 
                                             (tact (send A) tend)) 
                                       (tseq (tpar (tact (send A) tend) 
                                                   (tact (recv A) tend)) trec)))))
      as y3 eqn:Hy3.

      exists y3; split; [rewrite Hy3 |].
      { unfold weak. exists y2, y3.
        apply Pack_weak. 
        - rewrite Hy2.
          (* apply do_fix. *)
          (* apply do_comm. *)
          admit.
        - rewrite Hy2, Hy3. 
          apply do_seq. 
          (* apply do_handshake. *)
          admit.
        - rewrite <- Hy3. apply rt1n_refl. }
      {
        wsim_case_weak_action_filter_from_silent Htx0.
        (* TODO: determine a cleaner [x1] *)

        (* need to figure out what [x1] could be before continuing *)
        - wsim_next. admit. admit.
        - wsim_next. admit. admit.
        - admit.  
        - admit.  
        - admit.
      }
    }
    { admit. }

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

