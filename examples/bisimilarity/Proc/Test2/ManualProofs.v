(* Require Import MEBI.loader. *)
Require Coq.Program.Tactics.

Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.
Import Flat.
Import Flat.Complex.

Require Import MEBI.Examples.bisimilarity.Proc.Test2.Terms.

Example wsim_pq : weak_sim termLTS termLTS p q. 
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
