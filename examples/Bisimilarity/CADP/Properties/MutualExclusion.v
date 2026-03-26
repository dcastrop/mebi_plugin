Require Import MEBI.Examples.CADP.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Bisimilarity.

(* TODO: just copied from paper *)
(* Inductive act' : Type := | ENTER : nat -> label | LEAVE : nat -> label. *)


Inductive spec_status : Type :=
| Free : spec_status
| Held : nat -> spec_status.

Inductive spec_pid : Type :=
| Pid : nat -> spec_pid -> spec_pid
| Nil : spec_pid.

Inductive spec_state : Type :=
| State : spec_status -> spec_pid -> spec_state.

Inductive spec_lts : spec_state -> option label -> spec_state -> Prop :=
| SVC_ENTER : forall i x, 
  spec_lts (State Free (Pid i x)) (Some (ENTER, i)) (State (Held i) (Pid i x))
| SVC_FREE : forall i j x y, 
  spec_lts (State Free x) (Some (ENTER, j)) (State (Held j) y) ->
  spec_lts (State Free (Pid i x)) (Some (ENTER, j)) (State (Held j) (Pid i y))
| SVC_LEAVE : forall i x, 
  spec_lts (State (Held i) (Pid i x)) (Some (LEAVE, i)) (State Free (Pid i x))
| SVC_HELD : forall i j x y, 
  spec_lts (State (Held j) x) (Some (LEAVE, j)) (State Free y) ->
  spec_lts (State (Held j) (Pid i x)) (Some (LEAVE, j)) (State Free (Pid i y)).

Fixpoint make_spec_pid (n:nat) : spec_pid :=
  match n with 
  | 0 => Pid 0 Nil
  | S m => Pid n (make_spec_pid m)
  end.

Definition make_spec (n:nat) : spec_state := State Free (make_spec_pid n).





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
