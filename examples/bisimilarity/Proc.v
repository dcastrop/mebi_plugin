(* Require Import MEBI.loader. *)
Require Coq.Program.Tactics.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.

Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

(****************************************************************************)
Module Flat_Tests.
Import Flat.

(****************************************************************************)
Ltac wsim_try_clear_H3 :=
  match goal with 
  | [ H : ?a = ?a |- _ ] => clear H
  | |- _ => idtac (* "no H3" *)
  end.

Ltac wsim_try_inversion_H4 :=
  match goal with 
  | [ CH : @weak_sim ?M ?N ?A ?ltsM ?ltsN ?m ?n
    , H  : ?ltsM (_ ?ml ?mr) ?a (_ ?ml' ?mr')
    , H4 : ?ltsM ?ml ?a ?ml'
    |- exists n' : ?N, 
        @weak ?N ?A ?ltsN ?n n' ?a /\ 
        @weak_sim ?M ?N ?A ?ltsM ?ltsN (_ ?ml' ?mr') n'
    ] => inversion H4; subst; clear H4
  | [ CH : @weak_sim ?M ?N ?A ?ltsM ?ltsN ?m ?n
    , H  : ?ltsM (_ ?ml ?mr) ?a (_ ?ml' ?mr')
    , H4 : ?ltsM ?mr ?a ?mr'
    |- exists n' : ?N, 
        @weak ?N ?A ?ltsN ?n n' ?a /\ 
        @weak_sim ?M ?N ?A ?ltsM ?ltsN (_ ?ml' ?mr') n'
    ] => inversion H4; subst; clear H4
  | |- _ => idtac (* "no H4" *)
  end.

Ltac wsim_inversion_H :=
  match goal with 
  | [ CH : @weak_sim ?M ?N ?A ?ltsM ?ltsN ?m ?n
    , H  : ?ltsM ?m ?a ?m'
    |- exists n' : ?N, 
        @weak ?N ?A ?ltsN ?n n' ?a /\ 
        @weak_sim ?M ?N ?A ?ltsM ?ltsN ?m' n' ] 
      => 
      inversion H; subst; wsim_try_inversion_H4; clear H; wsim_try_clear_H3
  end.

Ltac wsim_handle_tsubst :=
  match goal with 
  | |- ?n = tsubst _ _ => constructor; idtac (* "?n tsubst" *)
  | |- _ => idtac (* "no tsubst" *)
  end
  ; unfold tsubst in *.

(****************************************************************************)
Module Test1.
  (* specific variant of Proc.v *)
  Import Flat.Simple.

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

Module ManualProofs.
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
            eapply rt1n_trans. do 2 constructor.
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

      cofix CH2; apply In_sim, Pack_sim; intros.
      inversion H; subst. inversion H4; subst; clear H H4.
      eauto with rel_db. }
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
            eauto with rel_db. }
          { eexists; split. eauto with rel_db.
            eauto with rel_db. } }
        { eexists; split. eauto with rel_db.

          cofix CH6; apply In_sim, Pack_sim; intros.
          inversion H; subst. inversion H4; subst; clear H H4.
          eauto with rel_db. } }
      { eexists; split. eauto with rel_db.
        eauto with rel_db. } }
    { eexists; split. eauto with rel_db.

      cofix CH2; apply In_sim, Pack_sim; intros.
      inversion H; subst. inversion H4; subst; clear H H4.
      eexists; split. eauto with rel_db.
      eauto with rel_db. }
  Qed.

  Theorem wbisim_qr : weak_bisim termLTS termLTS q r.
  Proof. unfold weak_bisim; split; [apply wsim_qr | apply wsim_rq]. Qed.

  (****************************************************************************)
  Example wsim_pr : weak_sim termLTS termLTS p r. 
  Proof. intros; subst; unfold p, r.
    
    cofix CH0; apply In_sim, Pack_sim; intros.
    inversion H; subst; unfold tsubst in *; clear H.
    eexists; split.
    { eapply wk_none; unfold silent.
      eapply rt1n_trans. do 2 constructor.
      eauto with rel_db. }
    
    cofix CH1; apply In_sim, Pack_sim; intros.
    inversion H; subst. inversion H4; subst; clear H H4; try clear H3.
    { eexists; split. 
      { eapply wk_some; unfold silent.
        eauto with rel_db. do 2 constructor.
        eapply rt1n_trans. do 2 constructor.
        eapply rt1n_trans. do 2 constructor.
        eapply rt1n_trans. do 2 constructor. unfold tsubst in *.
        eauto with rel_db. }

      cofix CH2; apply In_sim, Pack_sim; intros.
      inversion H; subst. inversion H4; subst; clear H H4; try clear H3.
      { eexists; split. eauto with rel_db.
      
        cofix CH3; apply In_sim, Pack_sim; intros.
        inversion H; subst; [inversion H4|]; clear H.
        eexists; split. eauto with rel_db.

        cofix CH4; apply In_sim, Pack_sim; intros.
        inversion H; subst. 
        eauto with rel_db. }
      { eexists; split. eauto with rel_db.
        eauto with rel_db. } }
    { eexists; split. eauto with rel_db.
      
      cofix CH2; apply In_sim, Pack_sim; intros.
      inversion H; subst. inversion H4; subst; clear H H4.
      eauto with rel_db. }
  Qed.
  
  Example wsim_rp : weak_sim termLTS termLTS r p. 
  Proof. intros; subst; unfold r, p.
    
    cofix CH0; apply In_sim, Pack_sim; intros.
    inversion H; subst. inversion H4; subst; clear H H4.
    eexists; split.
    { eapply wk_none; unfold silent.
      eapply rt1n_trans. do 2 constructor. unfold tsubst in *.
      eauto with rel_db. }

    eauto with rel_db. 
  Qed.
    
  Theorem wbisim_pr : weak_bisim termLTS termLTS p r.
  Proof. unfold weak_bisim; split; [apply wsim_pr | apply wsim_rp]. Qed.
End ManualProofs.

Module LtacProofs.
  (****************************************************************************)
  Ltac wsim_handle_wk_trans :=
    match goal with
    | |- ?ltsN ?n (Some ?a) ?n' => eauto with rel_db; do 2 constructor
                                  (* ; idtac "wk some1" *)
    | |- ?ltsN ?n  None     ?n' => eauto with rel_db; do 2 constructor
                                  (* ; idtac "wk none" *)
    | |- @clos_refl_trans_1n ?N ?tauN ?n ?n' => 
        match n with
        | tpar (tact (send ?a) _) (tact (recv ?b) _) => 
            tryif progress eauto with rel_db; do 3 constructor
            then eauto with rel_db (* ; idtac "wk some2" *)
            else idtac (* "wk tpar fail" *)
        | tseq (tpar (tact (send ?a) _) (tact (recv ?b) _)) _ => 
            tryif progress eauto with rel_db; do 3 constructor
            then eauto with rel_db (* ; idtac "wk some3" *)
            else idtac (* "wk tseq tpar fail" *)
        | _ => 
            eapply rt1n_trans; [do 3 constructor|]
            ; unfold tsubst in * (* ; idtac "wk cont" *)
            ; wsim_handle_wk_trans
        end
    end.

  Ltac wsim_handle_weak := 
    match goal with
    | |- @weak ?N ?A ?ltsN ?n ?n' (Some ?a) => eapply wk_some (* ; idtac "wk some" *)
    | |- @weak ?N ?A ?ltsN ?n ?n'  None     => eapply wk_none (* ; idtac "wk none" *)
    end
    ; unfold silent
    ; wsim_handle_wk_trans.

  Ltac wsim_cofix := 
    let CH := fresh "CH0" in cofix CH
    ; apply In_sim, Pack_sim
    ; intros
    ; wsim_inversion_H
    ; wsim_handle_tsubst
    ; (eexists; split; [wsim_handle_weak|]).

  Ltac wsim_next_case :=
    tryif solve [eauto with rel_db] 
    then idtac "solved case" else wsim_cofix.

  Ltac wsim_begin := 
    intros; subst;
    match goal with 
    | |- @weak_sim _ _ _ _ _ ?x ?y => unfold x, y; wsim_cofix
    end.

  (****************************************************************************)
  Example wsim_pq : weak_sim termLTS termLTS p q. 
  Proof. wsim_begin; do 9 wsim_next_case. Qed. 

  Example wsim_qp : weak_sim termLTS termLTS q p. 
  Proof. wsim_begin; do 9 wsim_next_case. Qed. 

  Theorem wbisim_pq : weak_bisim termLTS termLTS p q.
  Proof. unfold weak_bisim; split; [apply wsim_pq | apply wsim_qp]. Qed.

  (****************************************************************************)
  Example wsim_qr : weak_sim termLTS termLTS q r. 
  Proof. wsim_begin; do 9 wsim_next_case. Qed. 

  Example wsim_rq : weak_sim termLTS termLTS r q. 
  Proof. wsim_begin; do 9 wsim_next_case. Qed. 

  Theorem wbisim_qr : weak_bisim termLTS termLTS q r.
  Proof. unfold weak_bisim; split; [apply wsim_qr | apply wsim_rq]. Qed.

  (****************************************************************************)
  Example wsim_pr : weak_sim termLTS termLTS p r. 
  Proof. wsim_begin; do 1 wsim_next_case. Qed. 

  Example wsim_rp : weak_sim termLTS termLTS r p. 
  Proof. wsim_begin; do 1 wsim_next_case. Qed. 

  Theorem wbisim_pr : weak_bisim termLTS termLTS p r.
  Proof. unfold weak_bisim; split; [apply wsim_pr | apply wsim_rp]. Qed.
End LtacProofs.
End Test1.

Module Test2.
  (* specific variant of Proc.v *)
  Import Flat.Complex.

  Example p : term := tpar (tfix (tact (send A) (tact (send B) trec))) 
                           (tfix (tact (recv A) (tact (recv B) trec))). 

  Example q : term := tpar (tact (send A) (tfix (tact (send B) (tact (send A) trec)))) 
                           (tact (recv A) (tfix (tact (recv B) (tact (recv A) trec)))). 

  Example r : term := tpar (tact (send A) (tfix (tact (send B) (tact (send A) trec)))) 
                           (tfix (tact (recv A) (tact (recv B) trec))). 
                           
  
  (****************************************************************************)
  Example wsim_pq_MANUAL : weak_sim termLTS termLTS p q. 
  Proof. intros; subst; unfold q, r.

    cofix CH0; apply In_sim, Pack_sim; intros.
    inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
    { eexists; split. eauto with rel_db.

      cofix CH1; apply In_sim, Pack_sim; intros.
      inversion H; subst; try (inversion H4; subst; clear H4); clear H; unfold tsubst in *.
      { eexists; split. eauto with rel_db. 

        cofix CH2; apply In_sim, Pack_sim; intros.
        inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
        { eexists; split.
          { eapply wk_some; unfold silent. 
            eauto with rel_db. do 2 constructor.
            eauto with rel_db. }

          cofix CH3; apply In_sim, Pack_sim; intros.
          inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
          { eexists; split.
            { eapply wk_some; unfold silent.
              eapply rt1n_trans. do 3 constructor. unfold tsubst in *.
              eapply rt1n_trans. do 3 constructor. unfold tsubst in *.
              eauto with rel_db. do 2 constructor.
              eauto with rel_db. }
            
            cofix CH4; apply In_sim, Pack_sim; intros.
            inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
            { eexists; split. eauto with rel_db.
              eauto with rel_db. }
            { eexists; split. eauto with rel_db.

              cofix CH5; apply In_sim, Pack_sim; intros.
              inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
              { eexists; split. eauto with rel_db.
                eauto with rel_db. }
              { eexists; split. eauto with rel_db.

                cofix CH6; apply In_sim, Pack_sim; intros.
                inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
                { eexists; split. eauto with rel_db.

                  cofix CH7; apply In_sim, Pack_sim; intros.
                  inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
                  eexists; split. eauto with rel_db.
                  eauto with rel_db. }
                { eexists; split. eauto with rel_db.

                  cofix CH7; apply In_sim, Pack_sim; intros.
                  inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
                  { eexists; split. eauto with rel_db.
                    eauto with rel_db. }
                  { eexists; split. eauto with rel_db.
                    eauto with rel_db. } } } }
            { eexists; split. eauto with rel_db.

              cofix CH5; apply In_sim, Pack_sim; intros.
              inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
              { eexists; split. eauto with rel_db.

                cofix CH6; apply In_sim, Pack_sim; intros.
                inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
                { eexists; split. eauto with rel_db.

                  cofix CH7; apply In_sim, Pack_sim; intros.
                  inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
                  eexists; split. eauto with rel_db.
                  eauto with rel_db. }
                { eexists; split. eauto with rel_db.

                  cofix CH7; apply In_sim, Pack_sim; intros.
                  inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
                  { eexists; split. eauto with rel_db.
                    eauto with rel_db. }
                  { eexists; split. eauto with rel_db.
                    eauto with rel_db. } } }
              { eexists; split. eauto with rel_db.
              
                cofix CH6; apply In_sim, Pack_sim; intros.
                inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
                { eexists; split. eauto with rel_db.

                  cofix CH7; apply In_sim, Pack_sim; intros.
                  inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
                  eexists; split. eauto with rel_db.
                  eauto with rel_db. }
                { eexists; split. eauto with rel_db.
                  eauto with rel_db. } }
              { eexists; split. eauto with rel_db.
                eauto with rel_db. } } }
          { eexists; split. eauto with rel_db.

            cofix CH4; apply In_sim, Pack_sim; intros.
            inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
            eexists; split. eauto with rel_db.
            eauto with rel_db. } }
        { eexists; split. eauto with rel_db.
        
          cofix CH3; apply In_sim, Pack_sim; intros.
          inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
          eauto with rel_db. } }
      { eexists; split. eauto with rel_db.
      
        cofix CH2; apply In_sim, Pack_sim; intros.
        inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
        { eexists; split. eauto with rel_db.
        
          cofix CH3; apply In_sim, Pack_sim; intros.
          inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
          eexists; split. eauto with rel_db.
          
          cofix CH4; apply In_sim, Pack_sim; intros.
          inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
          { eexists; split.
            { eapply wk_some; unfold silent.
              eauto with rel_db. do 2 constructor.
              eapply rt1n_trans. do 3 constructor. unfold tsubst in *.
              eapply rt1n_trans. do 3 constructor. unfold tsubst in *.
              eauto with rel_db. }

            cofix CH5; apply In_sim, Pack_sim; intros.
            inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
            { eexists; split. 
              { eapply wk_some; unfold silent. 
                eauto with rel_db. do 2 constructor.
                eauto with rel_db. }

              eauto with rel_db. }
            { eexists; split. eauto with rel_db.
            
              cofix CH6; apply In_sim, Pack_sim; intros.
              inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
              eexists; split. eauto with rel_db.
              eauto with rel_db. } }
          { eexists; split. eauto with rel_db.
            eauto with rel_db. } }
        { eexists; split. eauto with rel_db.
          eauto with rel_db. } } }
    { eexists; split. eauto with rel_db.

      cofix CH1; apply In_sim, Pack_sim; intros.
      inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
      { eexists; split. eauto with rel_db.

        cofix CH2; apply In_sim, Pack_sim; intros.
        inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
        { eexists; split.
          { eapply wk_some; unfold silent.
            eauto with rel_db. do 2 constructor.
            eauto with rel_db. }
          
          cofix CH3; apply In_sim, Pack_sim; intros.
          inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
          { eexists; split.
            { eapply wk_some; unfold silent.
              eapply rt1n_trans. do 3 constructor. unfold tsubst in *.
              eapply rt1n_trans. do 3 constructor. unfold tsubst in *.
              eauto with rel_db. do 2 constructor.
              eauto with rel_db. }
            
            cofix CH4; apply In_sim, Pack_sim; intros.
            inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
            { eexists; split. eauto with rel_db.

              cofix CH5; apply In_sim, Pack_sim; intros.
              inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
              { eexists; split. eauto with rel_db.
                eauto with rel_db. }
              { eexists; split. eauto with rel_db.
              
                cofix CH6; apply In_sim, Pack_sim; intros.
                inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
                { eexists; split. eauto with rel_db.
                  
                  cofix CH7; apply In_sim, Pack_sim; intros.
                  inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
                  
                  eexists; split. eauto with rel_db.
                  eauto with rel_db. }
                { eexists; split. eauto with rel_db.
                  eauto with rel_db. } } }
            { eexists; split. eauto with rel_db.
              eauto with rel_db. }
            { eexists; split. eauto with rel_db.

              cofix CH5; apply In_sim, Pack_sim; intros.
              inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
              { eexists; split. eauto with rel_db.

                cofix CH6; apply In_sim, Pack_sim; intros.
                inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
                { eexists; split. eauto with rel_db.

                  cofix CH7; apply In_sim, Pack_sim; intros.
                  inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
                  eexists; split. eauto with rel_db.
                  eauto with rel_db. } 
                { eexists; split. eauto with rel_db.
                  eauto with rel_db. } }
              { eexists; split. eauto with rel_db.

                cofix CH6; apply In_sim, Pack_sim; intros.
                  inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
                  { eexists; split. eauto with rel_db.

                    cofix CH7; apply In_sim, Pack_sim; intros.
                    inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
                    eexists; split. eauto with rel_db.
                    eauto with rel_db. } 
                  { eexists; split. eauto with rel_db.

                    cofix CH7; apply In_sim, Pack_sim; intros.
                    inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
                    { eexists; split. eauto with rel_db.
                      eauto with rel_db. }
                    { eexists; split. eauto with rel_db.
                      eauto with rel_db. } } }
              { eexists; split. eauto with rel_db.
                eauto with rel_db. } } }
          { eexists; split.
            { eapply wk_none; unfold silent.
              eapply rt1n_trans. do 3 constructor. unfold tsubst in *.
              eapply rt1n_trans. do 3 constructor. unfold tsubst in *.
              eauto with rel_db. }
            
            cofix CH4; apply In_sim, Pack_sim; intros.
            inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
            eexists; split. eauto with rel_db.
            
            cofix CH5; apply In_sim, Pack_sim; intros.
            inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
            { eexists; split.
              { eapply wk_some; unfold silent.
                eauto with rel_db. do 2 constructor.
                eauto with rel_db. }
              eauto with rel_db. }
            { eexists; split. eauto with rel_db.
              eauto with rel_db. } } }
        { eexists; split. eauto with rel_db. 

          cofix CH3; apply In_sim, Pack_sim; intros.
          inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
          eexists; split. eauto with rel_db.
          eauto with rel_db. } }
      { eexists; split. eauto with rel_db.
       
        cofix CH2; apply In_sim, Pack_sim; intros.
        inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
        { eexists; split. eauto with rel_db.
    
          cofix CH3; apply In_sim, Pack_sim; intros.
          inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
          eexists; split. eauto with rel_db.
    
          cofix CH4; apply In_sim, Pack_sim; intros.
          inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
          { eexists; split.
            { eapply wk_some; unfold silent.
              eauto with rel_db. do 2 constructor.
              eapply rt1n_trans. do 3 constructor. unfold tsubst in *.
              eapply rt1n_trans. do 3 constructor. unfold tsubst in *.
              eauto with rel_db. }
            
            cofix CH5; apply In_sim, Pack_sim; intros.
            inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
            { eexists; split.
              { eapply wk_some; unfold silent.
                eauto with rel_db. do 2 constructor.
                eauto with rel_db. }
              eauto with rel_db. } 
            { eexists; split. eauto with rel_db. 

              cofix CH6; apply In_sim, Pack_sim; intros.
              inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
              eexists; split. eauto with rel_db.
              eauto with rel_db. } }
          { eexists; split. eauto with rel_db.
            eauto with rel_db. } }
        { eexists; split. eauto with rel_db.
          eauto with rel_db. } } }
    { eexists; split. eauto with rel_db.

      cofix CH1; apply In_sim, Pack_sim; intros.
      inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
      { eexists; split. eauto with rel_db.

        cofix CH2; apply In_sim, Pack_sim; intros.
        inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
        { eexists; split. eauto with rel_db.
    
          cofix CH3; apply In_sim, Pack_sim; intros.
          inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
          eexists; split. eauto with rel_db.
    
          cofix CH4; apply In_sim, Pack_sim; intros.
          inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
          { eexists; split.
            { eapply wk_some; unfold silent.
              eauto with rel_db. do 2 constructor.
              eapply rt1n_trans. do 3 constructor. unfold tsubst in *.
              eapply rt1n_trans. do 3 constructor. unfold tsubst in *.
              eauto with rel_db. }

            cofix CH5; apply In_sim, Pack_sim; intros.
            inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
            { eexists; split.
              { eapply wk_some; unfold silent.
                eauto with rel_db. do 2 constructor.
                eauto with rel_db. }
              eauto with rel_db. } 
            { eexists; split. eauto with rel_db. 

              cofix CH6; apply In_sim, Pack_sim; intros.
              inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
              eexists; split. eauto with rel_db.
              eauto with rel_db. } }
          { eexists; split. eauto with rel_db.
            eauto with rel_db. } }
        { eexists; split. eauto with rel_db.
    
          cofix CH3; apply In_sim, Pack_sim; intros.
          inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
          { eexists; split. eauto with rel_db.
    
            cofix CH4; apply In_sim, Pack_sim; intros.
            inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
            { eexists; split. 
              { eapply wk_some; unfold silent.
                eauto with rel_db. do 2 constructor.
                eapply rt1n_trans. do 3 constructor. unfold tsubst in *.
                eapply rt1n_trans. do 3 constructor. unfold tsubst in *.
                eauto with rel_db. }
                
              cofix CH5; apply In_sim, Pack_sim; intros.
              inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
              { eexists; split.
                { eapply wk_some; unfold silent.
                  eauto with rel_db. do 2 constructor.
                  eauto with rel_db. }
                eauto with rel_db. } 
              { eexists; split. eauto with rel_db. 

                cofix CH6; apply In_sim, Pack_sim; intros.
                inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
                eexists; split. eauto with rel_db.
                eauto with rel_db. } }
            { eexists; split. eauto with rel_db.

              cofix CH5; apply In_sim, Pack_sim; intros.
              inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
              eexists; split. eauto with rel_db.
              eauto with rel_db. } }
          { eexists; split. eauto with rel_db.
            eauto with rel_db. } } }
      { eexists; split. eauto with rel_db.

        cofix CH2; apply In_sim, Pack_sim; intros.
        inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
        { eexists; split. eauto with rel_db.
    
          cofix CH3; apply In_sim, Pack_sim; intros.
          inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
          eexists; split. eauto with rel_db.
    
          cofix CH4; apply In_sim, Pack_sim; intros.
          inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
          { eexists; split. 
            { eapply wk_some; unfold silent.
              eauto with rel_db. do 2 constructor.
              eapply rt1n_trans. do 3 constructor. unfold tsubst in *.
              eapply rt1n_trans. do 3 constructor. unfold tsubst in *.
              eauto with rel_db. }
              
            cofix CH5; apply In_sim, Pack_sim; intros.
            inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
            { eexists; split.
              { eapply wk_some; unfold silent.
                eauto with rel_db. do 2 constructor.
                eauto with rel_db. }
              eauto with rel_db. } 
            { eexists; split. eauto with rel_db.

              cofix CH6; apply In_sim, Pack_sim; intros.
              inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
              eexists; split. eauto with rel_db.
              eauto with rel_db. } }
          { eexists; split. eauto with rel_db.
            eauto with rel_db. } }
        { eexists; split. eauto with rel_db.
    
          cofix CH3; apply In_sim, Pack_sim; intros.
          inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
          { eexists; split. eauto with rel_db.

            cofix CH4; apply In_sim, Pack_sim; intros.
            inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
            { eexists; split. 
              { eapply wk_some; unfold silent.
                eauto with rel_db. do 2 constructor.
                eapply rt1n_trans. do 3 constructor. unfold tsubst in *.
                eapply rt1n_trans. do 3 constructor. unfold tsubst in *.
                eauto with rel_db. }
                
              cofix CH5; apply In_sim, Pack_sim; intros.
              inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
              { eexists; split.
                { eapply wk_some; unfold silent.
                  eauto with rel_db. do 2 constructor.
                  eauto with rel_db. }
                eauto with rel_db. } 
              { eexists; split. eauto with rel_db. 

                cofix CH6; apply In_sim, Pack_sim; intros.
                inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
                eexists; split. eauto with rel_db.
                eauto with rel_db. } }
            { eexists; split. eauto with rel_db.

              cofix CH5; apply In_sim, Pack_sim; intros.
              inversion H; subst; try (inversion H4; subst; clear H4); clear H; try clear H3; unfold tsubst in *.
              eexists; split. eauto with rel_db.
              eauto with rel_db. } } 
          { eexists; split. eauto with rel_db.
            eauto with rel_db. } } }
      { eexists; split. eauto with rel_db.
        eauto with rel_db. } }
  Qed.

  (****************************************************************************)
  Ltac wsim_handle_wk_trans :=
    match goal with
    | |- ?ltsN ?n (Some ?a) ?n' => eauto with rel_db; do 2 constructor
                                  (* ; idtac "wk some1" *)
    | |- ?ltsN ?n  None     ?n' => eauto with rel_db; do 2 constructor
                                  (* ; idtac "wk none" *)
    | |- @clos_refl_trans_1n ?N ?tauN ?n ?n' => 
        match n with
        | tpar (tact (send ?a) _) (tact (recv ?b) _) => 
            tryif progress eauto with rel_db; do 3 constructor
            then eauto with rel_db (* ; idtac "wk some2" *)
            else idtac (* "wk tpar fail" *)
        | _ => 
            eapply rt1n_trans; [do 3 constructor|]
            ; unfold tsubst in * (* ; idtac "wk cont" *)
            ; wsim_handle_wk_trans
        end
    end.

  Ltac wsim_handle_weak := 
    match goal with
    | |- @weak ?N ?A ?ltsN ?n ?n' (Some ?a) => eapply wk_some (* ; idtac "wk some" *)
    | |- @weak ?N ?A ?ltsN ?n ?n'  None     => eapply wk_none (* ; idtac "wk none" *)
    end
    ; unfold silent
    ; wsim_handle_wk_trans.

  Ltac wsim_cofix := 
    let CH := fresh "CH0" in cofix CH
    ; apply In_sim, Pack_sim
    ; intros
    ; wsim_inversion_H
    ; wsim_handle_tsubst
    ; (eexists; split; [wsim_handle_weak|]).

  Ltac wsim_next_case :=
    tryif solve [eauto with rel_db] 
    then idtac "solved case" else wsim_cofix.

  Ltac wsim_begin := 
    intros; subst;
    match goal with 
    | |- @weak_sim _ _ _ _ _ ?x ?y => unfold x, y; wsim_cofix
    end.

  (****************************************************************************)
  Example wsim_pq : weak_sim termLTS termLTS p q. 
  Proof. wsim_begin; do 7 wsim_next_case. Qed. 

  Example wsim_qp : weak_sim termLTS termLTS q p. 
  Proof. wsim_begin; do 9 wsim_next_case. Qed. 
    
  Theorem wbisim_pq : weak_bisim termLTS termLTS p q.
  Proof. unfold weak_bisim; split; [apply wsim_pq | apply wsim_qp]. Qed.

  (****************************************************************************)
  Example wsim_qr : weak_sim termLTS termLTS q r. 
  Proof. wsim_begin; do 9 wsim_next_case. Qed. 
    
  Example wsim_rq : weak_sim termLTS termLTS r q. 
  Proof. wsim_begin; do 8 wsim_next_case. Qed. 
    
  Theorem wbisim_qr : weak_bisim termLTS termLTS q r.
  Proof. unfold weak_bisim; split; [apply wsim_qr | apply wsim_rq]. Qed.

  (****************************************************************************)
  Example wsim_pr : weak_sim termLTS termLTS p r. 
  Proof. wsim_begin; do 10 wsim_next_case. Qed. 
  
  Example wsim_rp : weak_sim termLTS termLTS r p. 
  Proof. wsim_begin; do 9 wsim_next_case. Qed. 
    
  Theorem wbisim_pr : weak_bisim termLTS termLTS p r.
  Proof. unfold weak_bisim; split; [apply wsim_pr | apply wsim_rp]. Qed.
End Test2.
End Flat_Tests.