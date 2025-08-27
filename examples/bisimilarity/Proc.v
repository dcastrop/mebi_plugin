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

(* NOTE: very basic example -- doesn't actually do anything. *)
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

(* NOTE: given up, very long. going to try make Ltac. *)
Module Test2_manual.
  Example x : term := tfix (tseq (tpar (tact ASend tend) 
                                       (tact ARecv tend)) trec).

  Example y : term := tfix (tseq (tpar (tact ASend tend) 
                                       (tact ARecv tend)) 
                                 (tseq (tpar (tact ASend tend) 
                                             (tact ARecv tend)) trec)).

  Theorem ws_xy : forall x0 y0, x0 = x -> y0 = y -> weak_sim bLTS bLTS x0 y0.
  Proof. intros x0 y0 Hx0 Hy0; unfold x, y in Hx0, Hy0. 

    cofix CH1. apply In_sim, Pack_sim; 
    [ | intros _x1 Htx; apply blts_silent1_refl in Htx; 
        rewrite <- Htx; exists y0; split; [ constructor | apply CH1] ]. 
    Guarded.

    intros x1 ax01 Htx1. 
    apply blts_weak_silent, bLTS_unwrap in Htx1. rewrite Hx0 in Htx1.
    inversion Htx1 as [ | | | | | | x1t Hx1t Hax01 Hx1 | | | ]; symmetry in Hx1.
    rewrite <- Hx1; compute in Hx1; rewrite !Hx1. 
    remember (tsubst y0 (tseq (tpar (tact ASend tend) 
                                    (tact ARecv tend)) 
                              (tseq (tpar (tact ASend tend) 
                                          (tact ARecv tend)) trec))) 
    as y1 eqn:Hy1; compute in Hy1; rewrite Hy0 in Hy1; exists y1. split.
    { unfold weak; exists y0, y1; apply Pack_weak; constructor.
      rewrite Hy1, Hy0. apply do_fix. }
    { rewrite <- Hx1. cofix CH2. apply In_sim, Pack_sim;
      [ | intros _x2 Htx; apply blts_silent1_refl in Htx; 
          rewrite <- Htx; exists y1; split; [ constructor | apply CH2] ]. 
      Guarded.

      intros x2 ax12 Htx2.
      apply blts_weak_silent, bLTS_unwrap in Htx2. rewrite Hx1 in Htx2.
      inversion Htx2 as 
        [ | | | x2s x2c x2_x0 _ax12 Htx2sc Hx2s H_ax12 Hx2 | | | | | | ];
      symmetry in Hx2; rename H into Hx2_x0. rewrite Hy1.
      assert (x0 = x2_x0) as Hx0eqx2_x0; [rewrite Hx0, Hx2_x0; reflexivity |].

      inversion Htx2sc as 
        [ x2l x2r Hx2l Hax12 Hx2c | | | | | | 
        | x2l x2r Hx2l Hax12 Hx2c | |];
      rename H into Hx2r; symmetry in Hx2c; 
      rewrite Hx2c in Hx2; rewrite Hx2c, <- Hax12 in Htx2sc.
      { remember (tseq (tpar tend tend) 
                       (tseq (tpar (tact ASend tend) (tact ARecv tend)) y0))
        as y2 eqn:Hy2; rewrite Hy0 in Hy2; exists y2. split.
        { unfold weak; exists y1, y2. 
          rewrite Hy2, Hy1; apply Pack_weak; constructor.
          apply do_seq, do_senda. }
        { rewrite <- Hx2. cofix CH3. apply In_sim, Pack_sim;
          [ | intros _x3 Htx; apply blts_silent1_refl in Htx; 
              rewrite <- Htx; exists y2; split; [ constructor | apply CH3] ].
          Guarded.

          intros x3 ax23 Htx3.
          apply blts_weak_silent, bLTS_unwrap in Htx3. rewrite Hx2 in Htx3.
          inversion Htx3 as 
            [ | | | x3s x3c x3_x0 _ax23 Htx3sc Hx3s H_ax23 Hx3 | | | | | | ];
          symmetry in Hx3; rename H into Hx3_x0. rewrite Hy2.
          assert (x0 = x3_x0) as Hx0eqx3_x0; 
            [rewrite Hx0, Hx3_x0; reflexivity |].

          inversion Htx3sc as
            [ | | | | | _H Hax23 Hx3c | | x3l x3r Hx3l Hax23 Hx3c | | ]; 
          symmetry in Hx3c; [| rename H into Hx3r ];
          rewrite Hx3c in Hx3; rewrite Hx3c, <- Hax23 in Htx3sc.
          { remember 
              (tseq tend (tseq (tpar (tact ASend tend) (tact ARecv tend)) y0))
            as y3 eqn:Hy3; rewrite Hy0 in Hy3; exists y3. split.
            { unfold weak; exists y2, y3. 
              rewrite Hy3, Hy2; apply Pack_weak; constructor.
              apply do_seq, do_par_end. }
            { rewrite <- Hx3; cofix CH4. apply In_sim, Pack_sim;
              [ | intros _x4 Htx; apply blts_silent1_refl in Htx; 
                  rewrite <- Htx; exists y3; 
                  split; [ constructor | apply CH4] ].
              Guarded.

              (* and so on ... *)
              admit.
            } }
          { remember (tseq (tpar tend tend) (tseq (tpar (tact ASend tend) (tact ARecv tend)) y0))
            as y3 eqn:Hy3; rewrite Hy0 in Hy3; exists y3. split. 
            { unfold weak; exists y2, y3. 
              rewrite Hy3, Hy2; apply Pack_weak; constructor.
              apply do_seq, do_comm. }
            { rewrite <- Hx3.
              replace x3 with x2. replace y3 with y2. apply CH3.
              { rewrite Hy2, Hy3; reflexivity. }
              { rewrite Hx2, Hx3; reflexivity. }
            } } } }
      { remember (tseq (tpar (tact ARecv tend) (tact ASend tend)) 
                       (tseq (tpar (tact ASend tend) (tact ARecv tend)) y0))
        as y2 eqn:Hy2; rewrite Hy0 in Hy2; exists y2. split.
        { unfold weak; exists y1, y2. 
          rewrite Hy2, Hy1; apply Pack_weak; constructor.
          apply do_seq, do_comm. }
        { rewrite <- Hx2. cofix CH3. 
          (* and so on ... *)
          admit.
        }
      }
    }
    
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
End Test2_manual.

(* TODO *)
Module Test2.
  Example x : term := tfix (tseq (tpar (tact ASend tend) 
                                       (tact ARecv tend)) trec).

  Example y : term := tfix (tseq (tpar (tact ARecv tend) 
                                       (tact ASend tend)) 
                                 (tseq (tpar (tact ASend tend) 
                                             (tact ARecv tend)) trec)).

  Ltac sim_silent1_refl :=
    match goal with 
    | [ Hx : ?tx = _
      , Hy : ?ty = ?vy 
      , CH : @weak_sim _ _ _ _ _ ?tx ?ty 
      |- 
        forall nx, @silent1 _ _ _ ?tx nx -> 
          exists ny, @silent _ _ _ ?ty ny /\ 
            @weak_sim _ _ _ _ _ nx ny
      ] => 
        intros _x Htx; apply blts_silent1_refl in Htx;
        rewrite <- Htx; exists vy; rewrite <- Hy; 
        split; [ constructor | apply CH ]
    end.

  Ltac handle_weak_transition :=
    match goal with 
    | [ Hty : ?ty = ?vy
      , Hny : ?ny = ?nvy
        |- @weak _ _ _ ?vy _ ?ny
      ] =>
      unfold weak; exists ty, ny; rewrite Hty, Hny; 
      apply Pack_weak; try constructor; constructor
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
          exists eny, @weak _ _ _ (tfix ?vy) (Some false) eny /\ 
            @weak_sim _ _ _ _ _ ?nx eny
      ] => 
        let Hny := fresh "Hny" in let ny := fresh "ny" in 
        remember (tsubst (tfix vy) vy) as ny eqn:Hny; compute in Hny; 
        exists ny; split; [ handle_weak_transition | ]

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

    (* do_seq_end *)
    (* | [ 
        |- _
      ] => simpl *)

    (* do_par_end *)
    (* | [ 
        |- _
      ] => simpl *)

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

    (* do_comm *)
    (* | [ 
        |- _
      ] => simpl *)

    (* do_assocl *)
    (* | [ 
        |- _
      ] => simpl *)

    (* do_assocr *)
    (* | [ 
        |- _
      ] => simpl *)
    end.

  Ltac sim_visible :=
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
    end.

  Ltac sim_cofix :=
    let CH := fresh "CH" in cofix CH; 
    apply In_sim, Pack_sim; [ sim_visible | sim_silent1_refl ].

  Ltac sim_pre_cofix_unfold :=
    intros;
    match goal with 
    | [ Hx : ?tx = ?vx
      , Hy : ?ty = ?vy |- @weak_sim _ _ _ _ _ ?tx ?ty 
      ] => unfold vx, vy in Hx, Hy; sim_cofix
    end.

  Tactic Notation "solve_sim" := 
    sim_pre_cofix_unfold.


  (* Lemma handle_seq : 
    forall tx tx1 tx2 tx1l tx1r, tx = (tseq tx1 tx2) -> (tx1 = tpar tx1l tx1r) ->
      exists nx nx1 a, nx = (tseq nx1 tx2) -> 
      termLTS tx1 a nx1 -> termLTS tx a nx.
  Proof.
    intros tx tx1 tx2 tx1l tx1r Htx Htx1.

    

    do 3 eexists.
    intros Hnx Htrx1.
    inversion Htrx1; subst.
    - inversion H1. admit.
    - admit.
    - admit.
    - admit.
    - admit.
    - admit.
    - admit.
    - admit.
     *)

  Theorem ws_xy : forall x0 y0, x0 = x -> y0 = y -> weak_sim bLTS bLTS x0 y0.
  Proof. 
    solve_sim.
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
        , Hty1 : ?ty1 = tpar ?ty1l ?ty1r
        |- 
          exists eny, @weak _ _ _ (tseq (tpar ?ty1l ?ty1r) ?vy2) (Some ?a) eny /\ 
            @weak_sim _ _ _ _ _ ?nx eny
        ] =>
          simpl
      end.

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

