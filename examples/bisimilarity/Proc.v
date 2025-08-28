(* Require Import MEBI.loader. *)
Require Coq.Program.Tactics.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.

Inductive bLTS : term -> option label -> term -> Prop :=
| BISIM_WRAP : forall t1 t2 a, termLTS t1 a t2 -> bLTS t1 a t2.

Lemma bLTS_unwrap : forall x1 x2 a, bLTS x1 a x2 -> termLTS x1 a x2.
Proof. intros x1 x2 a Ht. inversion Ht; subst. apply H. Qed.

(* TODO *)
Module Test2.
  Example x : term := tfix (tseq (tpar (tact ASend tend) 
                                       (tact ARecv tend)) trec).

  Example y : term := tfix (tseq (tpar (tact ARecv tend) 
                                       (tact ASend tend)) 
                                 (tseq (tpar (tact ASend tend) 
                                             (tact ARecv tend)) trec)).

  (* Ltac sim_silent1_refl :=
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
    end. *)

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

  (* NOTE: makes goals easier to read e.g., [H : tend = t] -> [H : t = tend] *)
  Ltac ltr_sym :=
    repeat match goal with
    | [ t : term, H : tend = ?t |- _ ] => symmetry in H
    | [ t : term, H : trec = ?t |- _ ] => symmetry in H
    | [ t : term, H : tfix _ = ?t |- _ ] => symmetry in H
    | [ t : term, H : tsubst _ _ = ?t |- _ ] => compute in H
    | [ t : term, H : tact _ _ = ?t |- _ ] => symmetry in H
    | [ t : term, H : tseq _ _ = ?t |- _ ] => symmetry in H
    | [ t : term, H : tpar _ _ = ?t |- _ ] => symmetry in H
    | [ |- _ ] => idtac
    end.

  Ltac compute_tsubst :=
    ltr_sym;
    repeat match goal with
    | [ t : term, H : tsubst _ _ = ?t |- _ ] => compute in H
    | [ |- _ ] => idtac
    end.

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

  (* NOTE: called at the beginning of each new proof that terms are wsim. *)
  Ltac wsim_cofix :=
    let CH := fresh "CH" in cofix CH; 
    apply In_sim, Pack_sim; [ wsim_weak | wsim_silent ].

  (* NOTE: entrypoint, handles first cofix before [wsim_cofix]. *)
  Ltac wsim_pre_cofix_unfold :=
    intros;
    match goal with 
    | [ _Hx : ?tx = ?vx
      , _Hy : ?ty = ?vy |- @weak_sim _ _ _ _ _ ?tx ?ty 
      ] => 
        let Hx := fresh "Hx" in rename _Hx into Hx;
        let Hy := fresh "Hy" in rename _Hy into Hy;
        unfold vx, vy in Hx, Hy; wsim_cofix
    end.

  Tactic Notation "solve_wsim" := wsim_pre_cofix_unfold.

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

