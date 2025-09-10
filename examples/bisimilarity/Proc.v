(* Require Import MEBI.loader. *)
Require Coq.Program.Tactics.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.

Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

(****************************************************************************)
Section Test1.
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
  Example wsim_pq : weak_sim termLTS termLTS p q. 
  Proof. intros; subst; unfold p, q.
  
    cofix CH0; apply In_sim, Pack_sim; intros.
    inversion H; subst; unfold tsubst in *; clear H.
    eexists; split.
    { apply wk_none; unfold silent.
      eapply rt1n_trans. do 2 constructor. unfold tsubst in *.
      eapply rt1n_trans. do 2 constructor.
      eauto with rel_db. }

    cofix CH1; apply In_sim, Pack_sim; intros. 
    inversion H; subst. inversion H4; subst; clear H H4; try clear H3.
    { eexists; split.
      { eapply wk_some; unfold silent.
        eauto with rel_db. do 2 constructor.
        eapply rt1n_trans. do 2 constructor. 
        eapply rt1n_trans. do 2 constructor. 
        eauto with rel_db. }
        
      cofix CH2; apply In_sim, Pack_sim; intros.
      inversion H; subst. inversion H4; subst; clear H H4.
      { eexists; split. eauto with rel_db. 

        cofix CH3; apply In_sim, Pack_sim; intros.
        inversion H; subst; [inversion H4|]; clear H.
        eexists; split. eauto with rel_db.

        cofix CH4; apply In_sim, Pack_sim; intros.
        inversion H; subst; unfold tsubst in *; clear H.
        eexists; split. eauto with rel_db.

        cofix CH5; apply In_sim, Pack_sim; intros.
        inversion H; subst. inversion H4; subst; clear H H4; try clear H3.
        { eexists; split.
          { eapply wk_some; unfold silent.
            eauto with rel_db. do 2 constructor.
            eapply rt1n_trans. do 2 constructor.
            eapply rt1n_trans. do 2 constructor.
            eapply rt1n_trans. do 2 constructor. unfold tsubst in *.
            eapply rt1n_trans. do 2 constructor.
            eauto with rel_db. }

          cofix CH6; apply In_sim, Pack_sim; intros.
          inversion H; subst. inversion H4; subst; clear H H4.
          { eexists; split. eauto with rel_db.

            cofix CH7; apply In_sim, Pack_sim; intros.
            inversion H; subst; [inversion H4|]; clear H.
            eexists; split. eauto with rel_db.
            
            cofix CH8; apply In_sim, Pack_sim; intros.
            inversion H; subst; unfold tsubst in *; clear H.
            eexists; split. eauto with rel_db.
            eauto with rel_db. Guarded. }
          { eexists; split. eauto with rel_db.
            eauto with rel_db. Guarded. } }
        { eexists; split. eauto with rel_db.
        
          cofix CH6; apply In_sim, Pack_sim; intros.
          inversion H; subst. inversion H4; subst; clear H H4.
          eexists; split. eauto with rel_db.
          eauto with rel_db. Guarded. } }
      { eexists; split. eauto with rel_db.
        eauto with rel_db. Guarded. } }
    { eexists; split. eauto with rel_db.

      cofix CH2; apply In_sim, Pack_sim; intros.
      inversion H; subst. inversion H4; subst; clear H H4.
      eauto with rel_db. Guarded. }
  Qed.
  
  Example wsim_qp : weak_sim termLTS termLTS q p. 
  Proof. intros; subst; unfold q, p.

    cofix CH0; apply In_sim, Pack_sim; intros.
    inversion H; subst; unfold tsubst in *; clear H.
    eexists; split.
    { eapply wk_none; unfold silent.
      eapply rt1n_trans. do 2 constructor. unfold tsubst in *.
      eauto with rel_db. }
    
    cofix CH1; apply In_sim, Pack_sim; intros.
    inversion H; subst. inversion H4; subst; clear H H4.
    eexists; split. eauto with rel_db.

    cofix CH2; apply In_sim, Pack_sim; intros.
    inversion H; subst. inversion H4; subst; clear H H4; try clear H3.
    { eexists; split.
      { eapply wk_some; unfold silent.
        eauto with rel_db. do 2 constructor.
        eapply rt1n_trans. do 2 constructor.
        eapply rt1n_trans. do 2 constructor.
        eapply rt1n_trans. do 2 constructor. unfold tsubst in *.
        eauto with rel_db. }

      cofix CH3; apply In_sim, Pack_sim; intros.
      inversion H; subst. inversion H4; subst; clear H H4.
      { eexists; split. eauto with rel_db.
        
        cofix CH4; apply In_sim, Pack_sim; intros.
        inversion H; subst; [inversion H4|]; clear H.
        eexists; split. eauto with rel_db. 
          
        cofix CH5; apply In_sim, Pack_sim; intros.
        inversion H; subst. inversion H4; subst; clear H H4; try clear H3.
        { eexists; split.
          { eapply wk_some; unfold silent.
            eauto with rel_db. do 2 constructor.
            eapply rt1n_trans. do 2 constructor.
            eapply rt1n_trans. do 2 constructor.
            eapply rt1n_trans. do 2 constructor. unfold tsubst in *.
            eauto with rel_db. }

          cofix CH6; apply In_sim, Pack_sim; intros.
          inversion H; subst. inversion H4; subst; clear H H4.
          { eexists; split. eauto with rel_db.

            cofix CH7; apply In_sim, Pack_sim; intros.
            inversion H; subst; [inversion H4|]; clear H.
            eexists; split. eauto with rel_db.

            cofix CH8; apply In_sim, Pack_sim; intros.
            inversion H; subst; unfold tsubst in *; clear H.
            eexists; split. eauto with rel_db.
            eauto with rel_db. Guarded. }
          { eexists; split. eauto with rel_db.
            eauto with rel_db. Guarded. } }
      { eexists; split. eauto with rel_db.

        cofix CH6; apply In_sim, Pack_sim; intros.
        inversion H; subst. inversion H4; subst; clear H H4.
        eexists; split. eauto with rel_db.
        eauto with rel_db. Guarded. } }
    { eexists; split. eauto with rel_db.
      eauto with rel_db. Guarded. } }
  { eexists; split. eauto with rel_db.
    eauto with rel_db. Guarded. }
  Qed.
    
  Theorem wbisim_pq : weak_bisim termLTS termLTS p q.
  Proof. unfold weak_bisim; split; [apply wsim_pq | apply wsim_qp]. Qed.

  (****************************************************************************)
  Example wsim_qr : weak_sim termLTS termLTS q r. 
  Proof. intros; subst; unfold q, r.
    
    cofix CH0; apply In_sim, Pack_sim; intros.
    inversion H; subst; unfold tsubst in *; clear H.
    eexists; split.
    { eapply wk_none; unfold silent.
      eapply rt1n_trans. do 2 constructor.
      eauto with rel_db. }
    
    cofix CH1; apply In_sim, Pack_sim; intros.
    inversion H; subst. inversion H4; subst; clear H H4.
    eexists; split. eauto with rel_db.

    cofix CH2; apply In_sim, Pack_sim; intros.
    inversion H; subst. inversion H4; subst; clear H H4; try clear H3.
    { eexists; split. 
      { eapply wk_some; unfold silent.
        eauto with rel_db. do 2 constructor.
        eapply rt1n_trans. do 2 constructor.
        eapply rt1n_trans. do 2 constructor.
        eapply rt1n_trans. do 2 constructor. unfold tsubst in *.
        eauto with rel_db. }
      
      cofix CH3; apply In_sim, Pack_sim; intros.
      inversion H; subst. inversion H4; subst; clear H H4.
      { eexists; split. eauto with rel_db.
      
        cofix CH4; apply In_sim, Pack_sim; intros.
        inversion H; subst; [inversion H4|]; clear H.
        eexists; split. eauto with rel_db.

        cofix CH5; apply In_sim, Pack_sim; intros.
        inversion H; subst. inversion H4; subst; clear H H4; try clear H3.
        { eexists; split.
          { eapply wk_some; unfold silent.
            eauto with rel_db. do 2 constructor.
            eapply rt1n_trans. do 2 constructor.
            eapply rt1n_trans. do 2 constructor.
            eapply rt1n_trans. do 2 constructor. unfold tsubst in *.
            eauto with rel_db. }

          cofix CH6; apply In_sim, Pack_sim; intros.
          inversion H; subst. inversion H4; subst; clear H H4.
          { eexists; split. eauto with rel_db.
          
            cofix CH7; apply In_sim, Pack_sim; intros.
            inversion H; subst; [inversion H4|]; clear H.
            eexists; split. eauto with rel_db.

            cofix CH8; apply In_sim, Pack_sim; intros.
            inversion H; subst. 
            eauto with rel_db. }
          { eexists; split. eauto with rel_db.
            eauto with rel_db. } }
        { eexists; split. eauto with rel_db.
        
          cofix CH6; apply In_sim, Pack_sim; intros.
          inversion H; subst. inversion H4; subst; clear H H4.
          eexists; split. eauto with rel_db.
          eauto with rel_db. } }
      { eexists; split. eauto with rel_db.
        eauto with rel_db. } }
    { eexists; split. eauto with rel_db.
      eauto with rel_db. }
  Qed.
    
  Example wsim_rq : weak_sim termLTS termLTS r q. 
  Proof. intros; subst; unfold r, q.
    
    cofix CH0; apply In_sim, Pack_sim; intros.
    inversion H; subst. inversion H4; subst; clear H H4.
    eexists; split.
    { eapply wk_none; unfold silent.
      eapply rt1n_trans. do 2 constructor. unfold tsubst in *.
      eapply rt1n_trans. do 2 constructor.
      eauto with rel_db. }
    
    cofix CH1; apply In_sim, Pack_sim; intros.
    inversion H; subst. inversion H4; subst; clear H H4; try clear H3.
    { eexists; split.
      { eapply wk_some; unfold silent.
        eauto with rel_db. do 2 constructor.
        eapply rt1n_trans. do 2 constructor.
        eapply rt1n_trans. do 2 constructor.
        eauto with rel_db. }
      
      cofix CH2; apply In_sim, Pack_sim; intros.
      inversion H; subst. inversion H4; subst; clear H H4.
      { eexists; split. eauto with rel_db.

        cofix CH3; apply In_sim, Pack_sim; intros.
        inversion H; subst; [inversion H4|]; clear H.
        eexists; split. eauto with rel_db.

        cofix CH4; apply In_sim, Pack_sim; intros.
        inversion H; subst; clear H.
        eexists; split. eauto with rel_db.
        
        cofix CH5; apply In_sim, Pack_sim; intros.
        inversion H; subst. inversion H4; subst; clear H H4; try clear H3.
        { eexists; split.
          { eapply wk_some; unfold silent.
            eauto with rel_db. do 2 constructor.
            eapply rt1n_trans. do 2 constructor.
            eapply rt1n_trans. do 2 constructor.
            eapply rt1n_trans. do 2 constructor. unfold tsubst in *.
            eapply rt1n_trans. do 2 constructor.
            eauto with rel_db. }

          cofix CH6; apply In_sim, Pack_sim; intros.
          inversion H; subst. inversion H4; subst; clear H H4.
          { eexists; split. eauto with rel_db.

            cofix CH7; apply In_sim, Pack_sim; intros.
            inversion H; subst; [inversion H4|]; clear H.
            eexists; split. eauto with rel_db. 

            cofix CH8; apply In_sim, Pack_sim; intros.
            inversion H; subst.
            eauto with rel_db. Guarded. }
          { eexists; split. eauto with rel_db.
            eauto with rel_db. Guarded. } }
        { eexists; split. eauto with rel_db.

          cofix CH6; apply In_sim, Pack_sim; intros.
          inversion H; subst. inversion H4; subst; clear H H4.
          eauto with rel_db. Guarded. } }
      { eexists; split. eauto with rel_db.
        eauto with rel_db. Guarded. } }
    { eexists; split. eauto with rel_db.

      cofix CH2; apply In_sim, Pack_sim; intros.
      inversion H; subst. inversion H4; subst; clear H H4.
      eexists; split. eauto with rel_db.
      eauto with rel_db. Guarded. }
  Qed.
  
  Theorem wbisim_qr : weak_bisim termLTS termLTS q r.
  Proof. unfold weak_bisim; split; [apply wsim_qr | apply wsim_rq]. Qed.

  (****************************************************************************)
  Example wsim_pr : weak_sim termLTS termLTS p r. 
  Proof. intros; subst; unfold p, r.
  Admitted.
  
  Example wsim_rp : weak_sim termLTS termLTS r p. 
  Proof. intros; subst; unfold r, p.
  Admitted.
    
  Theorem wbisim_pr : weak_bisim termLTS termLTS p r.
  Proof. unfold weak_bisim; split; [apply wsim_pr | apply wsim_rp]. Qed.
End Test1.

(* TODO *)
Module Test2.
  Example p : term := tpar (tfix (tact (send A) (tact (send B) trec))) 
                           (tfix (tact (recv A) (tact (recv B) trec))). 

  Example q : term := tpar (tact (send A) (tfix (tact (send B) (tact (send A) trec)))) 
                           (tact (recv A) (tfix (tact (recv B) (tact (recv A) trec)))). 

  Example r : term := tpar (tact (send A) (tfix (tact (send B) (tact (send A) trec)))) 
                           (tfix (tact (recv A) (tact (recv B) trec))). 
                           
  
  (****************************************************************************)
  Example wsim_pq : weak_sim termLTS termLTS p q. 
  Proof. intros; subst; unfold q, r.
  Admitted.
    
  Example wsim_qp : weak_sim termLTS termLTS q p. 
  Proof. intros; subst; unfold q, p.
  Admitted.
    
  Theorem wbisim_pq : weak_bisim termLTS termLTS p q.
  Proof. unfold weak_bisim; split; [apply wsim_pq | apply wsim_qp]. Qed.

  (****************************************************************************)
  Example wsim_qr : weak_sim termLTS termLTS q r. 
  Proof. intros; subst; unfold q, r.
  Admitted.
    
  Example wsim_rq : weak_sim termLTS termLTS r q. 
  Proof. intros; subst; unfold r, q.
  Admitted.
    
  Theorem wbisim_qr : weak_bisim termLTS termLTS q r.
  Proof. unfold weak_bisim; split; [apply wsim_qr | apply wsim_rq]. Qed.

  (****************************************************************************)
  Example wsim_pr : weak_sim termLTS termLTS p r. 
  Proof. intros; subst; unfold p, r.
  Admitted.
  
  Example wsim_rp : weak_sim termLTS termLTS r p. 
  Proof. intros; subst; unfold r, p.
  Admitted.
    
  Theorem wbisim_pr : weak_bisim termLTS termLTS p r.
  Proof. unfold weak_bisim; split; [apply wsim_pr | apply wsim_rp]. Qed.
End Test2.

