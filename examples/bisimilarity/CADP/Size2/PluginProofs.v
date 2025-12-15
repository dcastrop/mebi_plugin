Require Import MEBI.loader.
Require Coq.Program.Tactics.

Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.

Require Import MEBI.Examples.bisimilarity.CADP.Size2.Terms.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.PluginProofs".
MeBi Reset All.
MeBi Set ShowAny      True.
MeBi Set ShowNotices  True.
(* MeBi Set ShowDebug    True. *)
(* MeBi Set ShowDetails  True. *)
MeBi Set ShowResults  True.
MeBi Set ShowWarnings True.
(* MeBi Set Bound 100. *)
MeBi Set WeakMode     True.
MeBi Set Weak Option label.
MeBi Set FailIfIncomplete True.
MeBi Set FailIfNotBisim True.
(* MeBi See All.  *)
MeBi Set ShowAny False.

Require Import Logic.


MeBi Divider "Examples.Bisimilarity.CADP.Size2.PluginProofs.bigstep_lts".
Example wsim_bigstep_lts : weak_sim bigstep lts c1 c1. 
Proof. MeBiSim Begin bigstep c1 And lts c1 Using step. 
  MeBiSim Solve 900.
Qed.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.PluginProofs.lts_bigstep".
Example wsim_lts_bigstep : weak_sim lts bigstep c1 c1. 
Proof. MeBiSim Begin lts c1 And bigstep c1 Using step. 
  MeBiSim Solve 900.
Qed.
