(* Evaluation series, scaled up: nesting depth ([Depth.v]) at K = 12 and
   16, [2K + 1] states each. Not built by default (see [_CoqProject]);
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
MeBi Config Bounds As Num States 25.
MeBi Run LTS (tfix X) Using G12.termLTS.
MeBi Config Bounds As Num States 24.
Fail MeBi Run LTS (tfix X) Using G12.termLTS.
MeBi Config Reset Bounds.
Example wbis_k12 : weak_bisimilar G12.termLTS G12.termLTS (tfix X) (tfix (tfix X)).
Proof. MeBi Sim Begin G12.termLTS (tfix X) And G12.termLTS (tfix (tfix X)) Using G12.termLTS.
  MeBi Sim Solve 15167. Qed.

(* [fix_depth t < 16] is an [lt] the premise search proves to the depth of
   about 16, the default ([MeBi Config Premise Depth]): left undecided, the
   LTS would be an approximation. [Reset Bounds] also resets the depth. *)
Module K16 <: BOUND. Definition K := 16. End K16.
Module G16 := Guarded K16.
MeBi Config Premise Depth 32.
MeBi Config Bounds As Num States 33.
MeBi Run LTS (tfix X) Using G16.termLTS.
MeBi Config Bounds As Num States 32.
Fail MeBi Run LTS (tfix X) Using G16.termLTS.
MeBi Config Reset Bounds.
MeBi Config Premise Depth 32.
Example wbis_k16 : weak_bisimilar G16.termLTS G16.termLTS (tfix X) (tfix (tfix X)).
Proof. MeBi Sim Begin G16.termLTS (tfix X) And G16.termLTS (tfix (tfix X)) Using G16.termLTS.
  MeBi Sim Solve 35135. Qed.
MeBi Config Reset Bounds.

MeBi Config Reset Weak.
