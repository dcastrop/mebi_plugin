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

Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.
Require Import MEBI.Examples.CADP_Glued.
Require Import MEBI.Examples.CADP_Norm.

Require Import MEBI.Examples.Bisimilarity.CADP.Size3.Terms.
Require Import MEBI.Examples.Bisimilarity.CADP.Properties.MutualExclusion.

MeBi Divider "Examples.Bisimilarity.CADP.Size3.Norm.TermTests".
MeBi Config Weak As Option label.
MeBi Config Bounds As Num States 1000.

(* Measured 2026-10-11: [bigstep_norm c3] has 644 states. [bigstep c3] has
   128044 (~19 min, ~4.7GB to extract, not done here), so erasing dead data
   ([CADP_Norm.v]) is what makes three processes tractable. *)
MeBi Run FSM c3 Using bigstep_norm bigstep.

(* As with two processes, the system and [spec_lts] are not bisimilar: the
   MCS queue fixes the order of entry silently. Decided in ~4s. *)
Fail MeBi Run Bisim c3 With bigstep_norm And (make_spec 2) With spec_lts
  Using bigstep.

Require Import MEBI.Examples.CADP_Obs.
Require Import MEBI.Examples.Bisimilarity.CADP.Properties.Fifo.

MeBi Divider "Examples.Bisimilarity.CADP.Size3.Norm.TermTests.fifo".
(* With joining the queue visible ([bigstep_obs]), the system is weakly
   bisimilar to the FIFO lock server ([Properties/Fifo.v]). Decided in ~4s;
   for one and two processes it is also proved ([Size1/Norm/Fifo],
   [Size2/Norm/Fifo]). *)
MeBi Run Bisim c3 With bigstep_obs_norm And (make_fifo 2) With spec_fifo
  Using bigstep_obs.
