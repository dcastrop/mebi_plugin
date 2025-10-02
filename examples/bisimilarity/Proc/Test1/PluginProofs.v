Require Import MEBI.loader.
Require Coq.Program.Tactics.

Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.
Import Flat.
Import Flat.Simple.

Require Import MEBI.Examples.bisimilarity.Proc.Test1.Terms.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs".
MeBi Reset All.
MeBi Set ShowAny      True.
(* MeBi Set ShowNotices  True. *)
MeBi Set ShowDebug    True.
(* MeBi Set ShowDetails  True. *)
MeBi Set ShowDetails  False.
MeBi Set ShowResults  True.
(* MeBi Set ShowWarnings True. *)
(* MeBi Set Bound 100. *)
MeBi Set WeakMode     True.
MeBi Set Weak Option label.
(* MeBi See All.  *)
(* MeBi Set ShowAny False. *)

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest".
Example wsim_pq : weak_sim termLTS termLTS p q. 
Proof.
  MeBi_Bisimilarity p With termLTS And q With termLTS Using termLTS.
  
  MeBi_Debug ProofNames.

  MeBi_Begin p With termLTS And q With termLTS Using termLTS.

  MeBi_Debug ProofNames.


  (* MeBi_Bisimilarity p With termLTS And q With termLTS Using termLTS.
  MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.cofix".
  MeBi_cofix.
  MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.cofix".
  (* MeBi_cofix. *)
  Fail MeBi_cofix. *)
  
  (* MeBi_ExploreProof.
  MeBi_ExploreProof.
  MeBi_unfold p.
  MeBi_ExploreProof.
  MeBi_ExploreProof.
  MeBi_unfold q.
  MeBi_ExploreProof.
  MeBi_ExploreProof. *)

  (* unfold p.  *)

  (* MeBi_unfold p. *)
  (* MeBi_intro. *)
  (* remember 3 as x. *)
  (* MeBi ProofTest1. *)
Admitted.