Require Import MEBI.loader.
Require Coq.Program.Tactics.

Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.
Require Import MEBI.Examples.CADP_Glued.

Require Import MEBI.Examples.bisimilarity.CADP.Size1.Terms.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.PluginProofs".
MeBi Config Reset.
MeBi Config Output Enable.
MeBi Config Output Notice Enable.
MeBi Config Output Debug Enable.
MeBi Config Output Info Enable.
MeBi Config Output Results Enable.
MeBi Config Output Warning Enable.
MeBi Config Bound 400.
MeBi Config WeakMode Enable.
MeBi Config Weak As Option label.
MeBi Config Fail If Incomplete True.
MeBi Config Fail If NotBisim True.
(* MeBi See All.  *)
(* MeBi Config Output Disable. *)

Require Import Logic.


MeBi Divider "Examples.Bisimilarity.CADP.Size1.PluginProofs.bigstep_lts".
Example wsim_bigstep_lts : weak_sim bigstep lts c1 c1. 
Proof. MeBiSim Begin bigstep c1 And lts c1 Using step.
  (* MeBiSim Solve 10.
  MeBiSim Solve 10.
  MeBiSim Solve 10.
  MeBiSim Solve 10.

  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step. *)




















  (* MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step. *)

  (* simpl. *)
  (* eapply rt1n_trans. *)
  (* constructor 1. *)
  (* constructor 3. *)
  (* constructor 3. *)

  (* TODO: issue is that the rocq-LTS inductive definition uses functions on the labels and destination state -- and we cannot unify on these ? *)
  (* eapply STEP_ACT. *)
  (* constructor 1 with (a:=(WRITE_NEXT THE_PID NIL)) (e:=(0, {| var_predecessor := None; var_locked := false; var_next := None; var_swap := false |}, None, ({| mem_next := None; mem_locked := false; qnodes := {| next := None; locked := false |} :: nil |}, None))). *)
  (* simpl. *)

  (* MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.

  (* constructor 1. *)
  (* eauto with rel_db. *)
  (* constructor. *)

  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.


  MeBiSim Solve 10.
  MeBiSim Solve 10.
  MeBiSim Solve 10.
  MeBiSim Solve 10.
  MeBiSim Solve 10. *)
  (* MeBiSim Solve 20. *)
  (* simpl in *.
  (* unfold * in * *)
  unfold Resource.initial in *. *)
  (* MeBiSim Step. *)
  (* MeBiSim Step. *)
Admitted.

(* MeBi Divider "Examples.Bisimilarity.CADP.Size1.PluginProofs.lts_bigstep".
Example wsim_lts_bigstep : weak_sim lts bigstep c1 c1. 
Proof. MeBiSim Begin lts c1 And bigstep c1 Using step. 
  MeBiSim Solve 900.
Qed. *)
