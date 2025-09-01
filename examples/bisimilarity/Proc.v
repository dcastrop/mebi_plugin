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
  
  Lemma is_silent1_start : forall x1 x2 x3,
    termLTS x1 None x2 -> silent termLTS x2 x3 -> silent1 termLTS x1 x3.
  Proof. intros x1 x2 x3 Ht12 Hs23. eauto with rel_db. Qed.

  Lemma is_silent : forall t1 t2, termLTS t1 None t2 -> silent termLTS t1 t2.
  Proof. intros. 
    inversion H; apply clos_t_clos_rt; constructor; constructor. apply H0. Qed.

  Lemma silent_with_silent1_start : forall x1 x2 x3,
    termLTS x1 None x2 ->
    silent termLTS x2 x3 ->
    silent termLTS x1 x3.
  Proof. intros x1 x2 x3 Ht12 Hs23. eauto with rel_db. Qed.
  (* Proof. intros x1 x2 x3 Ht12 Hs23. unfold silent. unfold silent in Hs23.
    apply clos_t_clos_rt.
    apply (@clos_rt_clos_t _ (tau termLTS) x1 x2 x3); [| apply Hs23 ].
    eauto with rel_db.
  Qed. *)

  (* NOTE: x => y *)
  Lemma do_weak : forall x0 x1 y0 y1 y2 y3 y4 a, 
    weak_sim termLTS termLTS x0 y0 ->
    weak termLTS x0 (Some a) x1 ->
    termLTS y0 None y1 -> 
    silent termLTS y1 y2 ->
    termLTS y2 (Some a) y3 -> 
    silent termLTS y3 y4 ->
    weak termLTS y0 (Some a) y4.
  Proof. 
    intros x0 x1 y0 y1 y2 y3 y4 a Hws Hwx Hty01 Hsy12 Hwy23 Hsy34. 
    exists y2, y3. eauto with rel_db. 
    (* apply Pack_weak.
    - apply (@silent_with_silent1_start y0 y1 y2 Hty01 Hsy12).
    - apply Hwy23.
    - apply Hsy34. *)
  Qed.  

  Lemma wsim_explore_silent1 : 
    forall x0 y0, weak_sim termLTS termLTS x0 y0 ->
    forall x1, silent1 termLTS x0 x1 ->
    exists x1a, termLTS x0 None x1a /\ silent termLTS x1a x1.
  Proof. intros. inversion H0; subst; [ exists x1 | exists y ].
    - split; [ apply H1 | apply rt1n_refl ].
    - split; [ apply H1 | apply clos_t_clos_rt in H2; apply H2 ].
  Qed.

  Lemma wsim_explore_silent :
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
  Admitted.

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

Ltac wsim_case_weak_action_filter_from_silent :=
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
    |- exists ny, @weak _ _ _ ?cy (Some ?a) ny /\ _ ] => wsim_case_weak_action_filter_from_silent
    
  (* do_seq_end*)
  | [ Hx : ?cx = tseq tend _, Htx : @weak _ _ _ ?cx (Some ?a) _ 
    |- exists ny, @weak _ _ _ ?cy (Some ?a) ny /\ _ ] => wsim_case_weak_action_filter_from_silent
    
  (* do_par_end *)
  | [ Hx : ?cx = tpar tend  _, Htx : @weak _ _ _ ?cx (Some ?a) _ 
    |- exists ny, @weak _ _ _ ?cy (Some ?a) ny /\ _ ] => wsim_case_weak_action_filter_from_silent
  
  | |- _ => idtac
  end.

Ltac wsim_case_weak_action := 
  let nx := fresh "x0" in intros nx;
  let a := fresh "a0" in intros a;
  let Htx := fresh "Htx0" in intros Htx;
  match goal with
  | [ Hx : ?cx = ?vx, Hy : ?cy = ?vy
    , Htx : @weak _ _ _ ?cx (Some a) nx 
    |- exists ny, @weak _ _ _ ?cy (Some ?a) ny /\ @weak_sim _ _ _ _ _ nx ny ] => 
      rewrite Hy; rewrite Hx in Htx; 
      first [ wsim_case_weak_action_filter | idtac ]
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
  | [ Hx : ?x = ?vx, Hy : ?y = ?vy 
    |- @weak_sim _ _ _ _ _ ?x ?y ] => unfold vx, vy in Hx, Hy; wsim_new_cofix
  end.
Tactic Notation "solve_wsim" := wsim_pre_cofix_unfold.

Ltac wsim_pre_cofix_unfold_with_states :=
  let x := fresh "x0" in intros x;
  let y := fresh "y0" in intros y;
  let xstates := fresh "xstates0" in intros xstates;
  let ystates := fresh "ystates0" in intros ystates;
  let HMx := fresh "HMx0" in intros HMx;
  let HMy := fresh "HMy0" in intros HMy;
  let Hx := fresh "Hx0" in intros Hx;
  let Hy := fresh "Hy0" in intros Hy;
  match goal with 
  | [ Hx : ?x = ?vx, Hy : ?y = ?vy 
    |- @weak_sim _ _ _ _ _ ?x ?y ] => unfold vx, vy in Hx, Hy; wsim_new_cofix
  end.
Tactic Notation "solve_wsim_with_states" := wsim_pre_cofix_unfold_with_states.

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
  (* NOTE: these would be obtained via the plugin *)
  Example p1 : term := tseq (tpar (tact (send A) tend) 
                                  (tact (recv A) tend)) 
                            (tfix (tseq (tpar (tact (send A) tend) 
                                              (tact (recv A) tend)) trec)).
  Example p2 : term := tseq (tpar (tact (recv A) tend) 
                                  (tact (send A) tend)) 
                            (tfix (tseq (tpar (tact (send A) tend) 
                                              (tact (recv A) tend)) trec)).
  Example p3 : term := tseq (tpar tend tend) 
                            (tfix (tseq (tpar (tact (send A) tend) 
                                              (tact (recv A) tend)) trec)).
  Example p4 : term := tseq tend 
                            (tfix (tseq (tpar (tact (send A) tend) 
                                              (tact (recv A) tend)) trec)).

  (* NOTE: these would be obtained via the plugin *)
  Example q1 : term := tseq (tpar (tact (recv A) tend) 
                                  (tact (send A) tend)) 
                            (tseq (tpar (tact (send A) tend) 
                                        (tact (recv A) tend)) 
                                  (tfix (tseq (tpar (tact (recv A) tend) 
                                                    (tact (send A) tend)) 
                                              (tseq (tpar (tact (send A) tend) 
                                                          (tact (recv A) tend)) trec)))).
  Example q2 : term := tseq (tpar (tact (send A) tend) 
                                  (tact (recv A) tend)) 
                            (tseq (tpar (tact (send A) tend) 
                                        (tact (recv A) tend)) 
                                  (tfix (tseq (tpar (tact (recv A) tend) 
                                                    (tact (send A) tend)) 
                                              (tseq (tpar (tact (send A) tend) 
                                                          (tact (recv A) tend)) trec)))).
  Example q3 : term := tseq (tpar tend tend) 
                            (tseq (tpar (tact (send A) tend) 
                                        (tact (recv A) tend)) 
                                  (tfix (tseq (tpar (tact (recv A) tend) 
                                                    (tact (send A) tend)) 
                                              (tseq (tpar (tact (send A) tend) 
                                                          (tact (recv A) tend)) trec)))).
  Example q4 : term := tseq tend 
                            (tseq (tpar (tact (send A) tend) 
                                        (tact (recv A) tend)) 
                                  (tfix (tseq (tpar (tact (recv A) tend) 
                                                    (tact (send A) tend)) 
                                              (tseq (tpar (tact (send A) tend) 
                                                          (tact (recv A) tend)) trec)))).
  Example q5 : term := tseq (tpar (tact (send A) tend) 
                                  (tact (recv A) tend)) 
                            (tfix (tseq (tpar (tact (recv A) tend) 
                                              (tact (send A) tend)) 
                                        (tseq (tpar (tact (send A) tend) 
                                                    (tact (recv A) tend)) trec))).
  Example q6 : term := tseq (tpar (tact (recv A) tend) 
                                  (tact (send A) tend)) 
                            (tfix (tseq (tpar (tact (recv A) tend) 
                                              (tact (send A) tend)) 
                                        (tseq (tpar (tact (send A) tend) 
                                                    (tact (recv A) tend)) trec))).
  Example q7 : term := tseq (tpar tend tend) 
                            (tfix (tseq (tpar (tact (recv A) tend) 
                                              (tact (send A) tend)) 
                                        (tseq (tpar (tact (send A) tend) 
                                                    (tact (recv A) tend)) trec))).
  Example q8 : term := tseq tend 
                            (tfix (tseq (tpar (tact (recv A) tend) 
                                              (tact (send A) tend)) 
                                        (tseq (tpar (tact (send A) tend) 
                                                    (tact (recv A) tend)) trec))).

  (* NOTE: these would be obtained via the plugin *)
  Example r1 : term := tseq (tpar (tact (send A) tend) 
                                  (tact (recv A) tend)) 
                            (tfix (tseq (tpar (tact (send A) tend) 
                                              (tact (recv A) tend)) trec)).
  Example r2 : term := tseq (tpar tend tend) 
                            (tfix (tseq (tpar (tact (send A) tend) 
                                              (tact (recv A) tend)) trec)).
  Example r3 : term := tseq tend 
                            (tfix (tseq (tpar (tact (send A) tend) 
                                              (tact (recv A) tend)) trec)).
  Example r4 : term := tfix (tseq (tpar (tact (send A) tend) 
                                        (tact (recv A) tend)) trec).

  (****************************************************************************)
  
  Definition p_states : list (term * list term) := 
    [ (p,  [p1])
    ; (p1, [p2; p3])
    ; (p2, [p1]) 
    ; (p3, [p3; p4]) 
    ; (p4, [p]) ].

  Definition q_states : list (term * list term) := 
    [ (q,  [q1])
    ; (q1, [q2])
    ; (q2, [q1; q3])
    ; (q3, [q3; q4])
    ; (q4, [q5])
    ; (q5, [q6; q7])
    ; (q6, [q5])
    ; (q7, [q7; q8])
    ; (q8, [q]) ].

  Definition r_states : list (term * list term) := 
    [ (r,  [r1])
    ; (r1, [r; r2])
    ; (r2, [r2; r3])
    ; (r3, [r4])
    ; (r4, [r1]) ].

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
    forall (xstates ystates : list (term * list term)),
    xstates = p_states -> ystates = q_states ->
    (*********************)
    x = p -> y = q -> 
    (* x = p -> y = r -> *)
    (* x = q -> y = p -> *)
    (* x = q -> y = r -> *)
    (* x = r -> y = p -> *)
    (* x = r -> y = q -> *)
    weak_sim termLTS termLTS x y.
  Proof.
    (* solve_wsim. *)
    solve_wsim_with_states.

    (* TODO: inductively define statespaces for the lists, which ensure there is a transition *)
    (* TODO: make ltac that use the provided information *)

    { admit. }
    {
      unfold silent.

      Check wsim_explore_silent1.
      rewrite <- Hx0 in Htx0; apply (@wsim_explore_silent1 x0 y0) in Htx0; rewrite Hx0 in Htx0.
      apply (@wsim_explore_silent1 x0 y0 CH0 x1) in Htx0.

      apply is_silent1 in Htx0.
      (* apply do_silent1 in Htx0.
      inversion_clear Htx0 as [?x1_pre [?x2_post [Htx0_pre [Htx0_str Htx0_post]]]]. *)



    }

      (* first [ wsim_case_weak_action_filter | idtac ] *)

      eexists. 

      ltac:(exact x0).

      Check ?[n2].
      split; [| wsim_new_cofix].
      (* apply do_weak. *)

      About do_weak.
      


      assert (termLTS_tc y0).
      { 

        trivial. termLTS_tc.
      rewrite Hy0. 
        Check termLTS_ts.
        apply (termLTS_ts _ _ _).  }
    }


      (* inversion Htx0 as [?xa [?xb [Hpre Hstr Hpost]]]. *)
      (* inversion Htx0 as [?xa [?xd [[| ?xb ?xc Hpre_tau Hpre_cft] Hstr Hpost]]]. *)
      (* inversion Htx0 as [?xa [?xd [Hpre Hstr Hpost]]]. *)
      inversion Hpre_tau0; fmt_goals. rewrite H1 in Hpre_cft0.
      inversion Hpre_cft0; fmt_goals. 
      {
        rewrite H in Hstr0.
        inversion Hstr0; fmt_goals.
        inversion H6; fmt_goals. rewrite H7 in H5. rewrite H5 in Hstr0.
      }

      destruct Hpost; inversion Hstr; fmt_goals. 
      admit.
      admit.
      admit.
      admit.


    }

                                       
(****************************************************************************)

  Theorem wsim_xy : forall x y, x = p -> y = q -> weak_sim termLTS termLTS x y.
  Proof. 
  Admitted.

  Theorem wsim_yx : forall x y, x = q -> y = p -> weak_sim termLTS termLTS x y.
  Admitted.

  Theorem wbisim_xy 
  : forall x y, x = p -> y = q -> weak_bisim termLTS termLTS x y.
  Proof.
    intros x0 y0 Hx0 Hy0; unfold x, y in Hx0, Hy0. 
    rewrite Hx0, Hy0; unfold weak_bisim. 
    split; [ apply wsim_xy | apply wsim_yx ]; trivial.
  Qed.
End Test2.

(****************************************************************************)
(****************************************************************************)
(****************************************************************************)
(****************************************************************************)

  Ltac handle_weak_transition :=
    match goal with 
    | [ Hty : ?ty = ?vy
      , Hny : ?ny = ?nvy
        |- @weak _ _ _ ?vy _ ?ny
      ] =>
      unfold weak; exists ty, ny; rewrite Hty, Hny; 
      apply Pack_weak; try constructor; constructor
    end.

  Ltac handle_constr_x :=
    match goal with
    (* synchronous send a/b/c *)
    | [ Htr : termLTS (tpar ?vxl ?vxr) ?a ?nx
      , Ha : true = ?a
      , Hnx : tpar ?nxl ?nxr = ?nx
        |- 
          exists eny, @weak _ _ _ ?vy (Some true) eny /\ 
            @weak_sim _ _ _ _ _ _ eny
      ] => 
        symmetry in Hnx, Ha
        (* ; handle_constr_y *)

    (* do_seq *)
    | [ CH0 : @weak_sim _ _ _ _ _ ?tx ?ty
      , Htx : ?tx = tseq ?vx1 ?vx2
      , Hty : ?ty = ?vy
      , Htr : termLTS (tseq ?vx1 ?vx2) ?a ?nx
      , Hx1 : termLTS ?vx1 ?a ?nx1
      , Hnx : tseq ?nx1 ?vx2 = ?nx
        |- 
          exists eny, @weak _ _ _ ?vy (Some ?a) eny /\ 
            @weak_sim _ _ _ _ _ (tseq ?nx1 ?vx2) eny
      ] => 
        symmetry in Hnx; rewrite <- Hnx; inversion Hx1 
        (* ; handle_constr_x *)

    (* do_fix *)
    | [ CH0 : @weak_sim _ _ _ _ _ ?tx ?ty
      , Htx : ?tx = tfix ?vx
      , Htr : termLTS (tfix ?vx) ?a ?nx
      , Hax : false = ?a
      , Hnx : tsubst (tfix ?vx) ?vx = ?nx
      , Hty : ?ty = _
        |- 
          exists eny, @weak _ _ _ ?vy (Some false) eny /\ 
            @weak_sim _ _ _ _ _ (tsubst (tfix ?bx) ?bx) eny
      ] => 
        symmetry in Hnx, Hax; rewrite <- Hnx; compute in Hnx
        (* ; handle_constr_y *)

    end.

  Ltac handle_constr_y :=
    match goal with 

    (* do_fix *)
    | [ CH0 : @weak_sim _ _ _ _ _ ?tx ?ty
      , Htx : ?tx = ?vx
      , Hty : ?ty = (tfix ?vy)
      , Htr : termLTS ?vx ?a ?nx
      , Hnx : ?nx = _
        |- 
          exists eny, @silent _ _ _ (tfix ?vy) eny /\ 
            @weak_sim _ _ _ _ _ ?nx eny
      ] => 
        let Hny := fresh "Hny" in let ny := fresh "ny" in 
        remember (tsubst (tfix vy) vy) as ny eqn:Hny; compute in Hny; 
        exists ny
        (* ; split; [ handle_weak_transition | ] *)

    end.

  (* Ltac sim_visible :=
    intros; 
    match goal with
    | [ Hx : ?tx = _
      , Hy : ?ty = ?vy
      , Htx : @weak _ _ _ ?tx (Some ?a) ?nx
      |- 
        exists eny, @weak _ _ _ ?ty (Some ?a) eny /\ 
          @weak_sim _ _ _ _ _ ?nx eny
      ] => 
        apply blts_weak_silent, bLTS_unwrap in Htx; 
        rewrite Hy; rewrite Hx in Htx; 
        inversion Htx
        (* ; handle_constr_x *)

    end. *)


  (* NOTE: for silent actions that occur within a weak visible action *)
  Ltac wsim_weak_silent_action :=
   match goal with
    | [ Hx : ?tx = ?vx, Hy : ?ty = ?vy
      , Htx : @weak _ ?_M ?_A ?vx (Some ?a) ?nx
      , Hpre : silent _ ?vx ?tx'
      , Hstr : _ ?tx' (Some ?a) ?nx'
      , Hpost : silent _ ?nx' ?nx
      |- exists eny, @weak _ _ _ ?vy (Some ?a) eny /\ 
          @weak_sim _ _ _ _ _ ?nx eny
      ] => 
        destruct Hpre
        ; [ destruct Hpost; inversion Hstr
          ; match goal with  
            | [ H : termLTS ?vx (Some ?a) ?nx |- _ ] => inversion H
            end
          | inversion H
            ; match goal with 
              | [ Hpre : Relation_Operators.clos_refl_trans_1n term _ ?vxb ?nxa
                , H : termLTS ?vx None ?vxb 
                |- _ ] => 
                inversion H; compute_tsubst
                (* ; inversion Hpre *)
                (* NOTE: leaves only the case where the visible action is later *)
              end
          ]
    end.

  (* NOTE: the first constructor of [Pack_sim] -- for weakly-similar actions *)
  Ltac wsim_weak :=
    intros; 
    match goal with
    | [ Hx : ?tx = _, Hy : ?ty = ?vy
      , _Htx : @weak _ _ _ ?tx (Some ?a) ?nx
      |- exists eny, @weak _ _ _ ?ty (Some ?a) eny /\ 
          @weak_sim _ _ _ _ _ ?nx eny
      ] => 
        let Htx := fresh "Htx" in rename _Htx into Htx
        ; rewrite Hy; rewrite Hx in Htx
        ; let Htx_w := fresh "Htx_w" in inversion Htx as [? Htx_w']
        ; inversion_clear Htx_w' as [? Htx_w]
        ; let Hpre := fresh "Hpre" in let Hstr := fresh "Hstr" in 
          let Hpost := fresh "Hpost" in destruct Htx_w as [Hpre Hstr Hpost]
        (* ; wsim_weak_silent_action *)
    end.

  (* NOTE: the second constructor of [Pack_sim] -- for silent actions *)
  Ltac wsim_silent :=
    intros; 
    match goal with
    | [ Hx : ?tx = _
      , Hy : ?ty = ?vy
      , Htx : @silent1 _ _ _ ?tx ?nx
      |- 
        exists eny, @silent _ _ _ ?ty eny /\ 
          @weak_sim _ _ _ _ _ ?nx eny
      ] => 
        rewrite Hy
        ; rewrite Hx in Htx
    end.

  Theorem ws_xy : forall x0 y0, x0 = x -> y0 = y -> weak_sim bLTS bLTS x0 y0.
  Proof. 
    solve_wsim.
    -
    match goal with
    | [ Hx : ?tx = ?vx, Hy : ?ty = ?vy
      , Htx : @weak _ ?_M ?_A ?vx (Some ?a) ?nx
      , Hpre : silent _ ?vx ?tx'
      , Hstr : _ ?tx' (Some ?a) ?nx'
      , Hpost : silent _ ?nx' ?nx
      |- exists eny, @weak _ _ _ ?vy (Some ?a) eny /\ 
          @weak_sim _ _ _ _ _ ?nx eny
      ] => 
        destruct Hpre
        ; [ destruct Hpost; inversion Hstr
          ; match goal with  
            | [ H : termLTS ?vx (Some ?a) ?nx |- _ ] => inversion H
            end
          | inversion H
            ; match goal with 
              | [ Hpre : Relation_Operators.clos_refl_trans_1n term _ ?vxb ?nxa
                , H : termLTS ?vx None ?vxb 
                |- _ ] => 
                inversion H; compute_tsubst
                (* ; inversion Hpre *)
              end
          ]
    end.

    (* inversion H.  *)
    inversion H0; ltr_sym. compute in H6. 

    inversion Hstr. inversion H0. 
    +   

    inversion H.
    inversion H0.


    -
    match goal with
    | [ Hx : ?tx = ?vx, Hy : ?ty = ?vy
      , Htx : @weak _ ?_M ?_A ?vx (Some ?a) ?nx
      , Hpre : silent _ ?vx ?tx'
      , Hstr : _ ?tx' (Some ?a) ?nx'
      , Hpost : silent _ ?nx' ?nx
      |- exists eny, @weak _ _ _ ?vy (Some ?a) eny /\ 
          @weak_sim _ _ _ _ _ ?nx eny
      ] => 
        destruct Hpre
        ; [ destruct Hpost; inversion Hstr
        ; match goal with  
          | [ H : termLTS ?vx (Some ?a) ?nx |- _ ] => inversion H
          end
        | ]
    end.



    { destruct Hpost. 
      - inversion Hstr. inversion H.
      - inversion Hstr. inversion H0. }



    {
      
      destruct H1. destruct H1. destruct H1. 

      (* NOTE: case where head term only has silent actions *)
      destruct pre.
      
      (* NOTE: case where no silent action before expected visible *)
      { destruct post; inversion str; [ inversion H1 | inversion H2 ]. }
      
      (* NOTE: case where some silent actions can occur before visible *)
      { inversion H1. inversion H2. 
        (* NOTE: now we can realise what the silent action yields *)
        symmetry in H8; compute in H8.

        (* rewrite H8 in pre.  *)
        inversion pre.
        { symmetry in H6. inversion str. 
          inversion H9; ltr_sym.
          Check H14.
          - l_to_r_sym.  l_to_r_sym.  l_to_r_sym.  l_to_r_sym. 
            symmetry in H13, H14, H15. admit.
          - symmetry in H13, H14, H15. admit.
          - symmetry in H13, H14, H15. admit.
          - symmetry in H14, H15, H16. admit.
          - symmetry in H13, H14, H15. 
          - symmetry in H13, H14, H15. 
          inversion H17.
        }

        inversion str. subst. 
        { inversion str. inversion H2. }
        { inversion str. inversion H2. } }
      {}
      

      destruct pre; destruct post; inversion str.
      - inversion H1.
      - inversion H2.
      - assert (forall a, termLTS x0 a x2 -> a = None).
      
      inversion H2.
      
      assert (forall a, termLTS x0 a x2 -> a = None); subst.

      - intros; inversion H1.
      - inversion H1.
      - intros. inversion H2.
      - inversion H2.
      - intros. inversion H; reflexivity.
      - inversion H2. rewrite <- H in pre. inversion pre.
      
      intros. inversion H; reflexivity.
       inversion H1; reflexivity.

      [
        
      | ].
      
      subst; 
      - apply H1 in H2; discriminate H2.
      - apply H1 in H3; discriminate H3.
      - inversion pre; subst. apply H1 in H3; discriminate H3.
      - apply H1 in H2; discriminate H2.
      - apply H1 in H2; discriminate H2.
      - apply H1 in H2; discriminate H2.

      discriminate H1.

      generalize H1.
      rewrite H2.  in H1.



      inversion H1. inversion H6.
      apply H1 in H2.
      discriminate H1.

      destruct post, pre; inversion str; subst; inversion H1; subst.

      admit. 
      admit. 
      admit. 


      - discriminate H8 

      
      inversion H1; inversion H2; inversion H3; subst.
      inversion pre; subst. 
      + inversion post; subst.
        * inversion str; subst. 
          inversion H; subst.
        * inversion H0; subst.
          -  inversion H0. inversion H4; constructor. 
      + inversion post; subst. 
        * admit.
        * admit.
    }- 
        inversion str; subst. 
          inversion H; subst.
      inversion post; subst. 
      inversion H0; subst. 
      inversion H; subst. 
      
      inversion H; subst.
      destruct H4.
      admit.
      admit.
      admit.
    - 
    
      inversion H1. inversion H2. inversion H3. 
      inversion pre. inversion post.

      rewrite <- H4 in str. 
      (* rewrite <- H5 in str.  *)
      inversion str.
      inversion H6.
      inversion H5.

    apply BISIM_WRAP in str.  



    handle_constr_x. handle_constr_y.
    
    sim_cofix.
    handle_constr_x. 
    - handle_constr_x.

      (* handle_constr_y. *)
      match goal with
      | [ Hty : ?ty = tseq ?vy1 ?vy2
        |- 
          exists eny, @weak _ _ _ (tseq ?vy1 ?vy2) (Some ?a) eny /\ 
            @weak_sim _ _ _ _ _ ?nx eny
      ] => 
        let Hty1 := fresh "Hty1" in let ty1 := fresh "ty1" in 
        remember vy1 as ty1 eqn:Hty1; rewrite Hty1; rewrite Hty1 in Hty
        (* let Hty2 := fresh "Hty2" in let ty2 := fresh "ty2" in  *)
        (* remember vy2 as ty2 eqn:Hty2 *)
    
      end.

      match goal with
      | [ Hty : ?ty = tseq ?vy1 ?vy2
        , Hty1 : ?ty1 = tpar (tact ASend ?ty1l) (tact ARecv ?ty1r)
        |- 
          exists eny, @weak _ _ _ (tseq (tpar (tact ASend ?ty1l) (tact ARecv ?ty1r)) ?vy2) (Some ?a) eny /\ 
            @weak_sim _ _ _ _ _ ?nx eny
        ] =>
          let Hny := fresh "Hny" in let ny := fresh "ny" in
          remember (tseq (tpar tend tend) vy2) as ny eqn:Hny;
          (* exists ny; rewrite Hny; split; 
          [ unfold weak; exists (tseq (tpar (tact ASend ty1l) (tact ARecv ty1r)) vy2), ny; 
            rewrite Hny; apply Pack_weak; try constructor;
            apply do_seq
            (* ; apply do_senda *)
            (* ; constructor *)
          | ] *)
          simpl

      | [ Hty : ?ty = tseq ?vy1 ?vy2
        , Hty1 : ?ty1 = tpar (tact ARecv ?ty1l) (tact ASend ?ty1r)
        |- 
          exists eny, @weak _ _ _ (tseq (tpar (tact ARecv ?ty1l) (tact ASend ?ty1r)) ?vy2) (Some ?a) eny /\ 
            @weak_sim _ _ _ _ _ ?nx eny
        ] =>
          let Hny := fresh "Hny" in let ny := fresh "ny" in
          remember (tseq (tpar (tact ASend ty1l) (tact ARecv ty1r)) vy2) as ny eqn:Hny;
          exists ny; rewrite Hny; split; 
          [ unfold weak
          ; exists (tseq (tpar (tact ARecv ty1l) (tact ASend ty1r)) vy2), (tseq (tpar (tact ASend ty1l) (tact ARecv ty1r)) vy2)
          (* , ny *)
          (* ; rewrite Hny *)
          ; apply Pack_weak
            ; try constructor
            ; apply do_seq
            (* ; apply do_senda *)
            (* ; constructor *)
          | ] 
          (* simpl *)
      end.

      constructor.

       apply do_comm.



      split. 
      unfold weak; exists ty, ny; rewrite Hty, Hny; 
      apply Pack_weak; try constructor; constructor
      
      handle_weak_transition.


          (* assert (
            (* forall ny1l ny1r,  *)
            termLTS (tpar ty1l ty1r) a (tpar ny1l ny1r) ->
            termLTS (tseq (tpar ty1l ty1r) vy2) a (tseq (tpar ny1l ny1r) vy2)
          ) ; [ intros Hy1l_par; constructor; apply Hy1l_par | ] *)


          assert (
            (* forall ny1l ny1r,  *)
            termLTS (tpar ty1l ty1r) a (tpar tend tend) ->
            termLTS (tseq (tpar ty1l ty1r) vy2) a (tseq (tpar tend tend) vy2)
          ) ; [ intros Hy1l_par; constructor; apply Hy1l_par | ]

      end.

      (* NOTE: manually handling the inner [tpar ... ...] transition.  *)
      (* NOTE: unsure how to automate this at this point in the proof. *)
      (* NOTE: am assuming that the plugin will be able to make this "jump". *)
      remember ()

      rewrite <- Hny.
      replace (weak bLTS ny (Some true)) 
        with  (termLTS ny true).
      rewrite Hny.

      apply H15.


      destruct H15.
      induction ny1l, ny1r.


      admit.
      admit.
      admit.
      admit.







      eexists.
      split. 
      (* handle_weak_transition. *)
      match goal with 
      | [ Hty : ?ty = ?vy
        , Hny : ?ny = ?nvy
          |- @weak _ _ _ ?vy _ ?ny
        ] =>
        unfold weak; exists ty, ny; rewrite Hty, Hny; 
        apply Pack_weak; try constructor; constructor
      | [ Hty : ?ty = ?vy
          |- @weak _ _ _ ?vy (Some ?a) ?ny
        ] =>
        (* simpl *)
        let ny := fresh "ny" in 
        (* assert (ny:term); [ trivial |] *)
        (* ;  *)
        assert (forall ny:term, termLTS vy a ny)
        (* ; induction (termLTS vy a ny)  *)
      | [ |- _] => fail 0
      end.

      generalize ny0.
      intros.

      constructor.

      induction (termLTS vy a ny). 



      inversion ny0; subst. 
      admit.
      admit.
      admit.
      admit.
      admit.
      +   

      apply handle_seq_termLTS.


      

      simpl in HeqP.
      compute in HeqP.



      unfold weak.

      (* destruct (exists n2, termLTS ny true n2). *)
      (* assumption *)

      (* eapply handle_weak_seq. *)


      (* remember  *)


      eexists.
      refine ?[n2].
      split; [exists ny; rewrite Hny | ]. 
      (* exists ?[n2]. *)
      econstructor.


      (* eapply handle_weak.  *)
      eapply Pack_weak; constructor.
      apply do_seq.
      trivial.  

      apply handle_weak_seq.

      (* assert (
        forall (exists n2, weak b)
      ) *)

      (* TODO: need to try and resolve the inner transition first *)

      subst.
      econstructor.
      econstructor.
      econstructor.
      econstructor.
      econstructor.
      econstructor.
      econstructor.
      apply do_seq.
      eapply do_senda.
      econstructor.
      

      admit.
      admit.
      admit.
      admit.
      admit.



      econstructor.
      econstructor.
      econstructor.
      econstructor.
      econstructor.
      econstructor.
      econstructor.
      econstructor.
      (* assert (?t' = ) *)
      refine ?[t'].
      

      econstructor.
      econstructor.
      econstructor.



      (* inversion P. *)

      { assert (ny1l : term); assert (ny1r : term); trivial.
        exists (tpar ny1l ny1r). apply do_senda.
      }
    

    eexists. split.
    (* handle_weak_transition. *)
    + unfold weak; exists ny; rewrite Hny. 
      assert (exists ny1, termLTS )
      eexists;
      constructor; constructor.

    apply do_seq. 
    (* constructor. *)
    
    apply do_senda.

    constructor.
    
    try constructor. 
    constructor.
    constructor.
    constructor.



     
    (* exists n2. *)
    (* pose (termLTS ny (true) ?n2). *)
    (* remember (transitive_closure ny). *)
    (* rewrite <- trans_step in HeqP. *)
    (* destruct HeqP. *)
    (* destruct (transitive_closure ny). *)
    (* coerce. *)
    (* apply trans_step. *)

    (* TODO: automatically figuring out what n2 will be, then handle_weak_transition *)
    



    
    (* handle_constr_x. *)
    -
    match goal with
    | [ Htr : termLTS (tpar ?vxl ?vxr) ?a ?nx
      , Ha : ?va = ?a
      , Hnx : tpar ?nxl ?nxr = ?nx
        |- 
        (* _ *)
          exists eny, @weak _ _ _ ?vy (Some ?va) eny /\ 
            @weak_sim _ _ _ _ _ _ eny
      ] => 
        symmetry in Hnx, Ha
        (* ; rewrite <- Hnx  *)
        (* simpl *)
        

        (* symmetry in Hnx, Hax; rewrite <- Hnx; compute in Hnx;
        let Hny := fresh "Hny" in let ny := fresh "ny" in 
        remember (tsubst (tfix vy) vy) as ny eqn:Hny; compute in Hny; 
        exists ny; split; 
        [ unfold weak; exists ty, ny; rewrite Hty, Hny; 
          apply Pack_weak; try constructor; constructor
        | ] *)
    end.
    
    handle_constr_y.
    (* match goal with 
    | [ CH0 : @weak_sim _ _ _ _ _ ?tx ?ty
      , Htx : ?tx = ?vx
      , Hty : ?ty = (tfix ?vy)
      , Htr : termLTS ?vx ?a ?nx
      , Hnx : ?nx = _
        |- 
          exists eny, @weak _ _ _ (tfix ?vy) (Some false) eny /\ 
            @weak_sim _ _ _ _ _ ?nx eny
      ] => 
        let Hny := fresh "Hny" in let ny := fresh "ny" in 
        remember (tsubst (tfix vy) vy) as ny eqn:Hny; compute in Hny; 
        exists ny; split; 
        [ unfold weak; exists ty, ny; rewrite Hty, Hny; 
          apply Pack_weak; try constructor; constructor
        | ]

      end. *)

  Admitted.

    sim_cofix.

    match goal with 
    | [ Htx : ?tx = tseq ?vx1 ?vx2
      , Htr : termLTS (tseq ?vx1 ?vx2) ?a ?nx
      , Htv : termLTS ?vx1 ?b ?nx1
      , Hab : ?b = ?a
      , Hnx : tseq ?nx1 ?vx2 = ?nx
      , Hty : ?ty = (tfix ?vy)
        |- 
          exists eny, @weak _ _ _ (tfix ?vy) (Some false) eny /\ 
            @weak_sim _ _ _ _ _ (tsubst (tfix ?bx) ?bx) eny
      ] => 
        symmetry in Hnx, Hax; rewrite <- Hnx; compute in Hnx;
        let Hny := fresh "Hny" in 
        remember (tsubst (tfix vy) vy) as ny eqn:Hny; compute in Hny; 
        exists ny; split; 
        [ unfold weak; exists ty, ny; rewrite Hty, Hny; 
          apply Pack_weak; try constructor; constructor
        | sim_cofix
        ]
    end.

    (* remember (tsubst (tfix ?vy) ?vy) as vy eqn:Hny. *)




    

    unfold weak. 


    (* sim_pre_cofix_unfold. *)

    intros.



  Admitted.


  (* CH : forall tx ty : term, tx = x -> ty = y -> weak_sim bLTS bLTS tx ty *)



  Theorem ws_yx : forall x0 y0, x0 = x -> y0 = y -> weak_sim bLTS bLTS y0 x0.
  Proof. 
    intros x0 y0 Hx0 Hy0; unfold x, y in Hx0, Hy0.
  Admitted.

  Theorem bs_xy : forall x0 y0, x0 = x -> y0 = y -> weak_bisim bLTS bLTS y0 x0.
  Proof. 
    intros x0 y0 Hx0 Hy0; unfold x, y in Hx0, Hy0. rewrite Hx0, Hy0.
    unfold weak_bisim; split; [apply ws_yx | apply ws_xy]; trivial.
  Qed.
End Test2.

(* TODO *)
Module Test3.
  Example x : term := tpar (tfix (tact ASend (tact BSend trec))) 
                           (tfix (tact ARecv (tact BSend trec))). 
  Example y : term := tpar (tact ASend (tfix (tact BSend (tact ASend trec)))) 
                           (tact ARecv (tfix (tact BRecv (tact ASend trec)))). 
  
  Theorem ws_xy : forall x0 y0, x0 = x -> y0 = y -> weak_sim bLTS bLTS x0 y0.
  Proof. 
    intros x0 y0 Hx0 Hy0; unfold x, y in Hx0, Hy0. 
  Admitted.

  Theorem ws_yx : forall x0 y0, x0 = x -> y0 = y -> weak_sim bLTS bLTS y0 x0.
  Proof. 
    intros x0 y0 Hx0 Hy0; unfold x, y in Hx0, Hy0.
  Admitted.

  Theorem bs_xy : forall x0 y0, x0 = x -> y0 = y -> weak_bisim bLTS bLTS y0 x0.
  Proof. 
    intros x0 y0 Hx0 Hy0; unfold x, y in Hx0, Hy0. rewrite Hx0, Hy0.
    unfold weak_bisim; split; [apply ws_yx | apply ws_xy]; trivial.
  Qed.
End Test3.

