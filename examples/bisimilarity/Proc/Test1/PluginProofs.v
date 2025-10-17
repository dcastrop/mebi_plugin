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

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.Bisim".
MeBi Set ShowAny True. 
MeBi Set ShowNotices  True.
MeBi Set ShowDebug    True.
MeBi Set ShowDetails  True.
MeBi Set ShowResults  True.
MeBi Set ShowWarnings True.
MeBi Set WeakMode     True.
MeBi Set Weak Option label.
MeBi Bisim p With termLTS And q With termLTS Using termLTS.

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
MeBi Set FailIfIncomplete True.
MeBi Set FailIfNotBisim True.
(* MeBi See All.  *)
(* MeBi Set ShowAny False. *)



MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest".
Example wsim_pq : weak_sim termLTS termLTS p q. 
Proof.
  MeBiSim Begin termLTS p And termLTS q Using termLTS. (* unfold p, q. *)
  MeBiSim ProofStep. (* cofix Cofix0; apply In_sim, Pack_sim; intros. *) 
  MeBiSim ProofStep. (* inversion H; simpl in *. *)
  MeBiSim ProofStep. (* eexists. -> but we want -> exists n2 *)
  (* MeBiSim ProofStep. *) (* NOTE: breaks -- fix previous step *)
  (* MeBiSim ProofStep.
  MeBiSim ProofStep.
  MeBiSim ProofStep.
  MeBiSim ProofStep. *)

  (* MeBiSim ProofStep. *)
  (* MeBiSim ProofStep. *)
  (* MeBiSim ProofStep. *)
  (* MeBiSim ProofStep. *)
  (* MeBiSim ProofStep. *)



  (* MeBiSim Cofix. *)
  (* MeBiSim Intros. *)
  (* MeBi Set ShowDetails True. *)
  (* MeBiSim FocusTest.  *)

  (* MeBiSim GoalTest. *)
  (* inversion H; subst. 
  simpl in *.
  eexists; split. *)

  (* MeBi Set ShowDetails True. *)
  (* MeBiSim FocusTest.  *)
  (* MeBi Set ShowDetails True. *)


  (* MeBiSim GoalTest. *)
  (* MeBiSim WeakNone. *)
  (* MeBiSim GoalTest. *)


  (* admit. admit. *)
  (* MeBiSim GoalTest. *)
   
  (* MeBi Set ShowDetails True. *)
  (* MeBiSim FocusTest.  *)


  (* MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest". *)

  (* Check H.
  About H.
  Print H.
  Compute H. *)

  (* MeBiSim GoalTest. *)

  (* do 2 constructor. unfold tsubst in *.
  eapply rt1n_trans. do 2 constructor.
  eauto with rel_db.  *)

  (* MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.Focus". *)
  (* NOTE: the below reveals a change in the Proof.data.stack (focus stack) *)
  (* - 
MeBi Set ShowDetails  True.
  MeBiSim FocusTest.  *)
  (* MeBiSim GoalTest. *)

  (* MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.END".
  MeBi_Debug ThisProof. 
  MeBi_Debug ProofNames. *)


  (* MeBiSim WeakNone.  *)
  (* MeBiSim FocusTest.  *)
  (* shelve. shelve. Unshelve. *)

  (* MeBi FSM p Using termLTS. *)


  (* eapply rt1n_trans. *)

  (* MeBi_Debug ThisProof. *)


(* {  *)
  (* apply wk_none; unfold silent. *)
    (* eapply rt1n_trans. 
    do 2 constructor. unfold tsubst in *.
    eapply rt1n_trans. do 2 constructor.
    eauto with rel_db.  *)
    (* } *)



  (* TODO: use the plugin info next... *)

  (* apply In_sim, Pack_sim. *)
  (* MeBi_unfold weak_sim. *)

  (* MeBi_Debug ProofNames. *)


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