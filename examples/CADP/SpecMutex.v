Require Import MEBI.Examples.CADP.Model.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Traces.
From Stdlib Require Import List.
Import ListNotations.

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

(* Mutual exclusion as a property of traces (2026-10-13): from [Free],
   the visible actions alternate [ENTER i] and [LEAVE i], for the same
   [i] each time. [MEBI.Traces] carries it across a simulation
   ([weak_sim_safe]); [spec_lts_mutex] is the characterisation lemma that
   links it to the lock server. *)
Inductive mutex_trace : spec_status -> list label -> Prop :=
| mt_nil : forall s, mutex_trace s []
| mt_enter : forall i t, mutex_trace (Held i) t -> mutex_trace Free ((ENTER, i) :: t)
| mt_leave : forall i t, mutex_trace Free t -> mutex_trace (Held i) ((LEAVE, i) :: t).

Definition spec_status_of (p : spec_state) : spec_status :=
  match p with State s _ => s end.

(* Each step of the server enters from [Free] or leaves from [Held i]. *)
Lemma spec_lts_step : forall p l q, spec_lts p l q ->
  (spec_status_of p = Free /\
   exists i, l = Some (ENTER, i) /\ spec_status_of q = Held i) \/
  (exists i, spec_status_of p = Held i /\ l = Some (LEAVE, i) /\
             spec_status_of q = Free).
Proof.
  intros p l q H; destruct H; simpl; eauto 10.
Qed.

Lemma spec_lts_no_silent : forall p q, silent spec_lts p q -> p = q.
Proof.
  intros p q H; destruct H as [|p' q' T _]; [reflexivity|].
  unfold tau in T; destruct (spec_lts_step _ _ _ T)
    as [[_ [i [E _]]] | [i [_ [E _]]]]; discriminate.
Qed.

Lemma spec_lts_mutex : forall p t q, weak_path spec_lts p t q ->
  mutex_trace (spec_status_of p) t.
Proof.
  intros p t q P; induction P as [x y _ | x y z a t W _ IH].
  - apply mt_nil.
  - inversion W as [a' z' t' S1 T S2|]; subst.
    apply spec_lts_no_silent in S1; apply spec_lts_no_silent in S2; subst.
    destruct (spec_lts_step _ _ _ T)
      as [[Hx [i [E Hy]]] | [i [Hx [E Hy]]]];
      injection E as ->; rewrite Hx; rewrite Hy in IH.
    + apply mt_enter, IH.
    + apply mt_leave, IH.
Qed.

Lemma make_spec_mutex : forall n t,
  weak_trace spec_lts (make_spec n) t -> mutex_trace Free t.
Proof. intros n t [q P]; exact (spec_lts_mutex _ _ _ P). Qed.





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
