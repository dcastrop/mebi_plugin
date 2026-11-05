(* Evaluation series, scaled up: nesting depth ([Depth.v]) at K = 12 and
   16, [2K + 1] states each. Not built by default (not in [examples/dune]);
   run it with [bench/proofs.sh -s eval-large]. Bounds as in [Width.v]. *)
Require Import MEBI.loader.
MeBi Config Output "Info" False.
MeBi Config Output "Notice" True.
MeBi Config Output "Result" False.
Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Evaluation.Depth.

MeBi Divider "Examples.Evaluation.DepthLarge".
MeBi Config Weak As Option action.

Module K12 <: BOUND. Definition K := 12. End K12.
Module G12 := Guarded K12.
Set MeBi MaxStates 25.
MeBi Run LTS (tfix X) Using G12.termLTS.
Set MeBi MaxStates 24.
Fail MeBi Run LTS (tfix X) Using G12.termLTS.
MeBi Config Reset Bounds.
Set MeBi MaxSteps 6302.
Example wbis_k12 : weak_bisimilar G12.termLTS G12.termLTS (tfix X) (tfix (tfix X)).
Proof. bisim. Qed.

(* [fix_depth t < 16] is an [lt] the premise search proves to the depth of
   about 16, the default ([MeBi Config Premise Depth]): left undecided, the
   LTS would be an approximation. [Reset Bounds] also resets the depth. *)
Module K16 <: BOUND. Definition K := 16. End K16.
Module G16 := Guarded K16.
Set MeBi PremiseDepth 32.
Set MeBi MaxStates 33.
MeBi Run LTS (tfix X) Using G16.termLTS.
Set MeBi MaxStates 32.
Fail MeBi Run LTS (tfix X) Using G16.termLTS.
MeBi Config Reset Bounds.
Set MeBi PremiseDepth 32.
Set MeBi MaxSteps 13826.
Example wbis_k16 : weak_bisimilar G16.termLTS G16.termLTS (tfix X) (tfix (tfix X)).
Proof. bisim. Qed.
MeBi Config Reset Bounds.

MeBi Config Reset Weak.
