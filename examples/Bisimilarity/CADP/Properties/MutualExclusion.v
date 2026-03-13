Require Import MEBI.Examples.CADP.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Bisimilarity.

(* TODO: just copied from paper *)
(* Inductive act' : Type := | ENTER : nat -> label | LEAVE : nat -> label. *)

Inductive spec_state : Type :=
| Free : spec_state
| Held : nat -> spec_state
| Pid : nat -> spec_state -> spec_state
.

Inductive spec_lts : spec_state -> option label -> spec_state -> Prop :=
| SVC_ENTER : forall i, spec_lts (Pid i Free) (Some (ENTER, i)) (Held i)
| SVC_LEAVE : forall i, spec_lts (Held i) (Some (LEAVE, i)) (Pid i Free)
| SVC_PID : forall i j x y a, spec_lts x (Some (a, j)) y ->
                              spec_lts (Pid i x) (Some (a, j)) (Pid i y)
| SVC_SWAP : forall i j x, spec_lts (Pid i (Pid j x)) None (Pid j (Pid i x))
.

Fixpoint make_spec (n:nat) : spec_state :=
  match n with 
  | 0 => Free
  | S n => Pid n (make_spec n)
  end.





(* Example p1 : tm * env := (Protocol.P, Env.initial 1).

Example c1 : composition := compose (create 1 Protocol.P).

Example d1 : spec_state := Free.

Example wsim_bigstep_spec_lts : weak_sim lts spec_lts c1 d1. 
Proof. intros. unfold c1, d1. simpl in *. 
  unfold Resource.initial, State.create.  
  unfold Vars.initial, Memory.create, Lock.initial. simpl in *.
  unfold Qnode.initial, Index.initial. 
  
  cofix CH0; apply In_sim, Pack_sim; intros.
  inversion H; subst.
  inversion H5; subst.
  simpl in *. clear H H5.
  exists Free. split; eauto with rel_db.

  cofix CH1; apply In_sim, Pack_sim; intros.
  inversion H; subst.
  inversion H5; subst.
  inversion H4; subst.
  inversion H6; subst. 
  simpl in *. clear H H5 H4 H6.
  exists Free. split; eauto with rel_db.
  
  cofix CH2; apply In_sim, Pack_sim; intros.
  inversion H; subst.
  inversion H5; subst.
  inversion H4; subst.
  - simpl in *. clear H H5 H4.
    exists Free. split; eauto with rel_db.

    cofix CH3; apply In_sim, Pack_sim; intros.
    inversion H; subst.
    inversion H5; subst.
    inversion H4; subst.
    inversion H6; subst. 
    simpl in *. clear H H5 H4 H6.
    exists Free. split; eauto with rel_db.

    cofix CH4; apply In_sim, Pack_sim; intros.
    inversion H; subst.
    inversion H5; subst.
    inversion H4; subst.
    * simpl in *. clear H H5 H4.
      exists Free. split; eauto with rel_db.

      cofix CH5; apply In_sim, Pack_sim; intros.
      inversion H; subst.
      inversion H5; subst.
      inversion H4; subst.
      simpl in *. clear H H5 H4.
      exists Free. split; eauto with rel_db.
      
      cofix CH6; apply In_sim, Pack_sim; intros.
      inversion H; subst.
      inversion H5; subst.
      simpl in *. clear H H5.
      + exists Free. split; eauto with rel_db.
      
      cofix CH7; apply In_sim, Pack_sim; intros.
      inversion H; subst.
      inversion H5; subst.
      inversion H4; subst.
      simpl in *. clear H H5 H4.
      eexists. split.
      { eapply wk_some; unfold silent.
        eauto with rel_db. constructor 1.
        eauto with rel_db. }
      { 
        
      }




Admitted.
  (* MeBi Sim Begin bigstep c1 And spec_lts Free Using step.
  (* Iteration History: _ <- _ <- _ <- _ *) 
  MeBi Sim Solve 1000. Qed. *)
 *)
