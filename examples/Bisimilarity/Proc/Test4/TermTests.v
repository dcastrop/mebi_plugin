(* KNOWN TECHNICAL LIMIT (backlog item B3, recorded 2026-10-01).

   This file does not compile, and Test4 has no PluginProofs.v, on purpose:
   it is the example that marks where the plugin currently stops scaling.

   - The reachable LTS of p (and of q and r -- it is the same set) has 9720
     states and 87,480 transitions, 85% silent: 3^4 local configurations x
     120 tree shapes, because do_comm/do_assocl/do_assocr can rebracket and
     reorder the four parallel components at any depth. In general
     3^k * (2k-2)!/(k-1)! for k components: 18, 324, 9720, 408240.
   - At the default bound of 100 states, the first command fails with
     LTS_Incomplete. With [MeBi Config Bounds As Num States 12000.] the full
     LTS extracts in ~25s.
   - Saturation is the blocker. The 120 tree shapes of each local
     configuration form one silent strongly connected component (81 of
     them), so the saturated LTS has ~112M weak transitions -- tens of GB
     in the plugin's representation. [MeBi Run Saturate p] exhausted a 15GB
     machine. Never run this file without a memory cap: systemd-run --user
     --scope -p MemoryMax=6G -p MemorySwapMax=0 (ulimit -v does not work --
     OCaml 5 cannot then reserve its heaps).

   Getting past it means saturating over the silent-SCC quotient (81 nodes)
   rather than over states, which changes what minimisation, bisimilarity
   and the proof solver receive. That is design work, deferred until the
   other outstanding backlog items are resolved. See ASSISTED-CHANGES.md,
   2026-10-01. *)

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
Require Import MEBI.Examples.Proc.
Import Layered.

Require Import MEBI.Examples.Bisimilarity.Proc.Test4.Terms.

MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests".
MeBi Config Weak As Option label.

MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.p".
MeBi Run FSM p Using compLTS termLTS. 
MeBi Run Saturate p Using compLTS termLTS.

MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.q".
MeBi Run FSM q Using compLTS termLTS. 
MeBi Run Saturate q Using compLTS termLTS.

MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.r".
MeBi Run FSM r Using compLTS termLTS. 
MeBi Run Saturate r Using compLTS termLTS.

MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.Bisim.pq".
MeBi Run Bisim p With compLTS And q With compLTS Using compLTS termLTS.
MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.Bisim.qp".
MeBi Run Bisim q With compLTS And p With compLTS Using compLTS termLTS.


MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.Bisim.qr".
MeBi Run Bisim q With compLTS And r With compLTS Using compLTS termLTS.
MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.Bisim.rq".
MeBi Run Bisim r With compLTS And q With compLTS Using compLTS termLTS.


MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.Bisim.pr".
MeBi Run Bisim p With compLTS And r With compLTS Using compLTS termLTS.
MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.Bisim.rp".
MeBi Run Bisim r With compLTS And p With compLTS Using compLTS termLTS.
