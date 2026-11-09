Require Import MEBI.loader.


Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CCS.

MeBi Divider "Examples.Bisimilarity.CCS.PluginProofs".

(* Textbook pairs, each stating exactly what holds: [weak_sim] each way,
   [mutual_sim], [weak_bisimilar], and refusals where a relation does not
   hold (the tactic decides first, so a [Fail] there is a decided "no"). *)

Definition A_ := ? a . pnil.
Definition B_ := ? b . pnil.
Definition C_ := ? c . pnil.

(* 1. Milner: [tau.a + b] and [a + b] simulate each other but are not
   weakly bisimilar: after the tau the left has silently given up [b]. *)
MeBi Divider "Examples.Bisimilarity.CCS.PluginProofs.milner".
Definition milner_l := sum (tau. A_) B_.
Definition milner_r := sum A_ B_.
Example wsim_milner_lr : weak_sim step step milner_l milner_r.
Proof. weak_sim. Qed.
Example wsim_milner_rl : weak_sim step step milner_r milner_l.
Proof. weak_sim. Qed.
Example wbis_milner : weak_bisimilar step step milner_l milner_r.
Proof. Fail weak_bisim. Abort.

(* 2. [a.b + a.c] is simulated by [a.(b + c)], not conversely (same traces,
   different branching). *)
MeBi Divider "Examples.Bisimilarity.CCS.PluginProofs.branching".
Definition early := sum (? a . B_) (? a . C_).
Definition late := ? a . sum B_ C_.
Example wsim_early_late : weak_sim step step early late.
Proof. weak_sim. Qed.
Example wsim_late_early : weak_sim step step late early.
Proof. Fail weak_sim. Abort.

(* 3. Vending machines: VM2 (which commits at the coin) is simulated by
   VM1 (which lets the customer choose), not conversely. *)
MeBi Divider "Examples.Bisimilarity.CCS.PluginProofs.vending".
Example wsim_vm2_vm1 : weak_sim step step (var 1) (var 0).
Proof. weak_sim. Qed.
Example wsim_vm1_vm2 : weak_sim step step (var 0) (var 1).
Proof. Fail weak_sim. Abort.

(* 4. Two one-place buffers chained through a restricted [m] are a
   two-place buffer, weakly (the hand-over on [m] is a tau). *)
MeBi Divider "Examples.Bisimilarity.CCS.PluginProofs.buffers".
Definition chained := res (cons m nil) (par (var 2) (var 3)).
Example wbis_buffers : weak_bisimilar step step chained (var 4).
Proof. weak_bisim. Qed.

(* 5. The Alternating Bit Protocol is weakly bisimilar to a one-place buffer
   (Milner 1989): the buffer simulates the protocol, then the
   bisimilarity. *)
MeBi Divider "Examples.Bisimilarity.CCS.PluginProofs.abp".
Example wsim_abp_spec : weak_sim step step abp (var 20).
Proof. weak_sim. Qed.
Example wbis_abp_spec : weak_bisimilar step step abp (var 20).
Proof. weak_bisim. Qed.
