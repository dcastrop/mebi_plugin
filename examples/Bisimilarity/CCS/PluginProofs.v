Require Import MEBI.loader.

MeBi Config Output "Debug" False.
MeBi Config Output "Info" False.
MeBi Config Output "Notice" True.
MeBi Config Output "Warning" True.
MeBi Config Output "Error" True.
MeBi Config Output "Trace" False.
MeBi Config Output "Result" False.
MeBi Config Output "Show" False.
MeBi Config Output "DecodeResults" False.
MeBi Config Output "DumpResults" False.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CCS.

MeBi Divider "Examples.Bisimilarity.CCS.PluginProofs".
MeBi Config Weak As Option act.

(* Textbook pairs, each stating exactly what holds: [weak_sim] each way,
   [mutual_sim], [weak_bisimilar], and refusals where a relation does not
   hold ([Begin] checks first, so a [Fail] there is a decided "no"). *)

Definition A_ := ? a . pnil.
Definition B_ := ? b . pnil.
Definition C_ := ? c . pnil.

(* 1. Milner: [tau.a + b] and [a + b] simulate each other but are not
   weakly bisimilar: after the tau the left has silently given up [b]. *)
MeBi Divider "Examples.Bisimilarity.CCS.PluginProofs.milner".
Definition milner_l := sum (tau. A_) B_.
Definition milner_r := sum A_ B_.
Example wsim_milner_lr : weak_sim step step milner_l milner_r.
Proof. MeBi Sim Begin step milner_l And step milner_r Using step. MeBi Sim Solve 100. Qed.
Example wsim_milner_rl : weak_sim step step milner_r milner_l.
Proof. MeBi Sim Begin step milner_r And step milner_l Using step. MeBi Sim Solve 100. Qed.
Example msim_milner : mutual_sim step step milner_l milner_r.
Proof. split; [exact wsim_milner_lr | exact wsim_milner_rl]. Qed.
Example wbis_milner : weak_bisimilar step step milner_l milner_r.
Proof. Fail MeBi Sim Begin step milner_l And step milner_r Using step. Abort.

(* 2. [a.b + a.c] is simulated by [a.(b + c)], not conversely (same traces,
   different branching). *)
MeBi Divider "Examples.Bisimilarity.CCS.PluginProofs.branching".
Definition early := sum (? a . B_) (? a . C_).
Definition late := ? a . sum B_ C_.
Example wsim_early_late : weak_sim step step early late.
Proof. MeBi Sim Begin step early And step late Using step. MeBi Sim Solve 100. Qed.
Example wsim_late_early : weak_sim step step late early.
Proof. Fail MeBi Sim Begin step late And step early Using step. Abort.

(* 3. Vending machines: VM2 (which commits at the coin) is simulated by
   VM1 (which lets the customer choose), not conversely. *)
MeBi Divider "Examples.Bisimilarity.CCS.PluginProofs.vending".
Example wsim_vm2_vm1 : weak_sim step step (var 1) (var 0).
Proof. MeBi Sim Begin step (var 1) And step (var 0) Using step. MeBi Sim Solve 200. Qed.
Example wsim_vm1_vm2 : weak_sim step step (var 0) (var 1).
Proof. Fail MeBi Sim Begin step (var 0) And step (var 1) Using step. Abort.

(* 4. Two one-place buffers chained through a restricted [m] are a
   two-place buffer, weakly (the hand-over on [m] is a tau). *)
MeBi Divider "Examples.Bisimilarity.CCS.PluginProofs.buffers".
Definition chained := res (cons m nil) (par (var 2) (var 3)).
Example wbis_buffers : weak_bisimilar step step chained (var 4).
Proof. MeBi Sim Begin step chained And step (var 4) Using step. MeBi Sim Solve 1000. Qed.

(* 5. The Alternating Bit Protocol is weakly bisimilar to a one-place buffer
   (Milner 1989), and the check says so. One direction of the proof goes
   through. The other, [abp <= spec], does not terminate: inverting an ABP
   step whose label is still open lets the solver re-invert one kept
   hypothesis forever (backlog Step 0, note 7), so it is left as a decided
   check plus one proved direction until that is fixed. See
   ASSISTED-CHANGES.md, 2026-10-02. *)
MeBi Divider "Examples.Bisimilarity.CCS.PluginProofs.abp".
MeBi Config Bounds As Num States 1000.
MeBi Run Bisim abp With step And (var 20) With step Using step.
Example wsim_spec_abp : weak_sim step step (var 20) abp.
Proof. MeBi Sim Begin step (var 20) And step abp Using step. MeBi Sim Solve 1000. Qed.
MeBi Config Reset Bounds.
