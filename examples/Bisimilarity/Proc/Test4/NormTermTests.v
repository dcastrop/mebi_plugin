(* Test4 under the normalised semantics ([ProcCongruence.Normalised.nLTS]):
   structural congruence as an explicit relation with canonical
   representatives, rather than as silent steps. The same p, q and r, from
   9720 states under [compLTS] to 82 here (81 normal forms and p itself);
   bisimilarity decided in seconds. Fast, so built by default. The proof is
   in [NormProofs.v] (not built by default: ~7 min, ~3.8GB). See notes/13
   and ASSISTED-CHANGES.md, 2026-10-03. *)

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
Require Import MEBI.Examples.Proc.
Import Layered.
Require Import MEBI.Examples.ProcCongruence.
Import Normalised.
Require Import MEBI.Examples.Bisimilarity.Proc.Test4.Terms.

MeBi Divider "Examples.Bisimilarity.Proc.Test4.NormTermTests".
MeBi Config Weak As Option label.

(* 82 states from each of p, q, r: the least bound that completes *)
MeBi Config Bounds As Num States 82.
MeBi Run LTS p Using nLTS core termLTS.
MeBi Run LTS q Using nLTS core termLTS.
MeBi Run LTS r Using nLTS core termLTS.
MeBi Config Bounds As Num States 81.
Fail MeBi Run LTS p Using nLTS core termLTS.

MeBi Config Bounds As Num States 200.
MeBi Run Bisim p With nLTS And q With nLTS Using nLTS core termLTS.
MeBi Run Bisim q With nLTS And r With nLTS Using nLTS core termLTS.
MeBi Run Bisim p With nLTS And r With nLTS Using nLTS core termLTS.
MeBi Config Reset Bounds.
MeBi Config Reset Weak.
