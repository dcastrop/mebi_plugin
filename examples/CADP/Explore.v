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
Require Import MEBI.Examples.CADP.Model.
Import Protocol.

Require Import MEBI.Examples.CADP.Glued.
Require Import MEBI.Examples.CADP.Norm.
Require Import MEBI.Examples.CADP.Obs.
Require Import MEBI.Examples.CADP.SpecMutex.
Require Import MEBI.Examples.CADP.SpecFifo.
Require Import MEBI.Examples.CADP.Terms.

(* The CADP example's state spaces, and what the plugin decides about them
   without a proof: FSMs, saturation, minimisation and [Run Bisim]. One
   section per size. The two-process layered model (12027 states) is in
   [ExploreLarge.v], which needs a memory cap. *)

MeBi Divider "Examples.CADP.Explore".
MeBi Config Weak As Option label.

(**************************************************)
(* One process. *)

MeBi Config Output "Info" True.
MeBi Config Output "DecodeResults" True.

MeBi Divider "Examples.CADP.Explore.Size1.step".
MeBi Run FSM p1 Using step.
MeBi Run Saturate p1 Using step.
MeBi Run Minimize p1 Using step.

MeBi Divider "Examples.CADP.Explore.Size1.lts".
MeBi Run FSM c1 Using lts step.
MeBi Run Saturate c1 Using lts step.
MeBi Run Minimize c1 Using lts step.

MeBi Divider "Examples.CADP.Explore.Size1.lts.extra".
MeBi Run FSM c1b Using lts step.
MeBi Run Saturate c1b Using lts step.
MeBi Run Minimize c1b Using lts step.

MeBi Divider "Examples.CADP.Explore.Size1.bigstep".
MeBi Run FSM c1 Using bigstep lts step.
MeBi Run Saturate c1 Using bigstep lts step.
MeBi Run Minimize c1 Using bigstep lts step.
MeBi Run Bisim c1 With bigstep And c1 With lts Using step.

(**************************************************)
(* Two processes, glued. *)

MeBi Divider "Examples.CADP.Explore.Size2".
MeBi Config Bounds As Num States 5000.

MeBi Run Minimize (make_spec 1) Using spec_lts.

MeBi Divider "Examples.CADP.Explore.Size2.bigstep".
MeBi Run FSM c2 Using bigstep.

(* !! takes very long time (4-12 hours) *)
(* MeBi Run Saturate c2 Using bigstep. *)
(* MeBi Run Minimize c2 Using bigstep. *)

(* Glued against layered: decided in [ExploreLarge.v], on demand. *)

(**************************************************)
(* Three processes, normalised. *)

MeBi Config Output "Info" False.
MeBi Config Output "DecodeResults" False.

MeBi Divider "Examples.CADP.Explore.Size3".
MeBi Config Bounds As Num States 1000.

(* Measured 2026-10-11: [bigstep_norm c3] has 644 states. [bigstep c3] has
   128044 (~19 min, ~4.7GB to extract, not done here), so erasing dead data
   ([Norm.v]) is what makes three processes tractable. *)
MeBi Run FSM c3 Using bigstep_norm bigstep.

(* As with two processes, the system and [spec_lts] are not bisimilar: the
   MCS queue fixes the order of entry silently. Decided in ~4s. *)
Fail MeBi Run Bisim c3 With bigstep_norm And (make_spec 2) With spec_lts
  Using bigstep.

MeBi Divider "Examples.CADP.Explore.Size3.fifo".
(* With joining the queue visible ([bigstep_obs]), the system is weakly
   bisimilar to the FIFO lock server ([SpecFifo.v]). Decided in ~4s; for
   one and two processes it is also proved ([Fifo.v]). *)
MeBi Run Bisim c3 With bigstep_obs_norm And (make_fifo 2) With spec_fifo
  Using bigstep_obs.
