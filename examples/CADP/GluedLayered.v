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
Require Import MEBI.Examples.CADP.Glued.

Require Import MEBI.Examples.CADP.Terms.

(* The glued semantics ([bigstep], [Glued.v]) against the layered one
   ([lts] over [step], [Model.v]): the first link in carrying a property
   proved for glued over to layered ([LayeredTransfer.v]). Proved for one
   process; for two it is only decided (see the end of this file). *)

MeBi Divider "Examples.CADP.GluedLayered".

MeBi Config Weak As Option label.
MeBi Config Bounds As Num States 100.

Require Import Logic.


(**************************************************)
(* One process. *)

MeBi Divider "Examples.CADP.GluedLayered.Size1.bigstep".
Example wsim_bigstep_1 : weak_sim bigstep lts c1 c1. 
Proof. MeBi Sim Begin bigstep c1 And lts c1 Using step.
  (* Iteration History: 267 <- 300 <- _ <- _ *) 
  MeBi Sim Solve 267. Qed.


MeBi Divider "Examples.CADP.GluedLayered.Size1.lts".
Example wsim_lts_1 : weak_sim lts bigstep c1 c1. 
Proof. MeBi Sim Begin lts c1 And bigstep c1 Using step. 
  (* Iteration History: 395 <- _ <- _ <- _  *) 
  MeBi Sim Solve 395. Qed.

MeBi Divider "Examples.CADP.GluedLayered.Size1.mutual".
(* Mutual similarity: the two [weak_sim]s above, one each way. *)
Example msim_bigstep_1 : mutual_sim bigstep lts c1 c1.
Proof. split; [exact wsim_bigstep_1 | exact wsim_lts_1]. Qed.

(* Weak bisimilarity: one relation, both directions, in one proof
   ([weak_bisimilar]) -- stronger than the [mutual_sim] above. It needs the
   mutual cofix, which [Auto] chooses for it; forced to the nested
   strategy it is unfinished after 20000 steps, which is why it comes last:
   a run with the strategy forced still reaches every [weak_sim] above. *)

MeBi Divider "Examples.CADP.GluedLayered.Size1.bisimilar".
Example wbis_bigstep_1 : weak_bisimilar bigstep lts c1 c1.
Proof. MeBi Sim Begin bigstep c1 And lts c1 Using step.
  MeBi Sim Solve 2874. Qed.


(**************************************************)
(* Two processes: NOT PROVED.

   Every proof below runs out of memory in [Solve]. Glued and layered are
   *decided* bisimilar at two processes ([Run Bisim] in [ExploreLarge.v],
   ~35s, ~1GB); proving it is out of reach today.

   Measured 2026-10-12 against the dune build, one proof per process under
   [systemd-run --user --scope -p MemoryMax=<cap> -p MemorySwapMax=0], with
   [Solve 1000000] and [Bounds As Num States 15000]. In each, [Begin] takes
   ~33s, saturating layered on demand (1,091,776 weak actions over 10,083
   silent SCCs, above the 1M bound), and [Auto] then takes the nested cofix
   without estimating:

     proof              7GB cap           10GB cap
     wsim_bigstep_lts   OOM, 16m53s       OOM, 25m25s
     wsim_lts_bigstep   OOM, 35m24s       OOM, 52m01s
     wbis_bigstep_lts   -                 OOM, 28m28s

   Memory grew to each cap (0.3-0.47GB a minute for [wbis_bigstep_lts],
   falling slowly; the rate per step is unmeasured). With
   [MeBi Config Bounds Game 100000000], [Auto] estimates instead (Begin
   ~90s) and picks the mutual cofix: 514,020 pairs and 2,056,080 moves,
   against over 10,280,400 goals nested. That run ([wbis_bigstep_lts],
   10GB cap, [Solve] in 2000-step chunks) did not finish its first chunk
   in 2h28m of [Solve] at full CPU, with memory flat at ~2.2GB; it was
   stopped then. Where that time goes (setting up the mutual cofix over
   514,020 pairs is the likely place) is unmeasured.

   See OUTSTANDING.md (CADP) and doc/future-work.md ("Proofs split into
   separately checked lemmas"). *)

(*
MeBi Config Bounds As Num States 15000.

MeBi Divider "Examples.CADP.GluedLayered.Size2.bigstep".
Example wsim_bigstep_lts : weak_sim bigstep lts c2 c2. 
Proof. MeBi Sim Begin bigstep c2 And lts c2 Using step.
  MeBi Sim Solve 1000000. Qed.

MeBi Divider "Examples.CADP.GluedLayered.Size2.lts".
Example wsim_lts_bigstep : weak_sim lts bigstep c2 c2. 
Proof. MeBi Sim Begin lts c2 And bigstep c2 Using step. 
  MeBi Sim Solve 1000000. Qed.

MeBi Divider "Examples.CADP.GluedLayered.Size2.bisimilar".
Example wbis_bigstep_lts : weak_bisimilar bigstep lts c2 c2.
Proof. MeBi Sim Begin bigstep c2 And lts c2 Using step.
  MeBi Sim Solve 1000000. Qed.
*)
