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
Require Import MEBI.Examples.CADP.Terms.

(* [Explore.v] for the two-process layered model, kept apart because it
   is large: run it under a memory cap. *)

MeBi Divider "Examples.CADP.ExploreLarge".
MeBi Config Weak As Option label.
(* Measured 2026-10-11: [lts step] on [c2] has 12027 states and extracts
   in ~32s (~0.65GB). Keep "Result"/"DecodeResults"/"DumpResults" off: they
   add ~0.65MB per state. *)
MeBi Config Bounds As Num States 15000.

MeBi Divider "Examples.CADP.ExploreLarge.Size2.step".
MeBi Run FSM c2 Using lts step.
(* MeBi Run Saturate c2 Using lts step. *)
(* MeBi Run Minimize c2 Using lts step. *)

MeBi Divider "Examples.CADP.ExploreLarge.Size2.glued".
(* The glued semantics (772 states) against the layered one (12027): weakly
   bisimilar. The layered side would saturate to ~1.09M weak actions, above
   the default bound, so this decides on demand over its 10083 silent SCCs
   (~35s, ~1GB). Decided only: the proof is out of reach (see
   [GluedLayered.v]). *)
MeBi Run Bisim c2 With bigstep And c2 With lts Using step.
