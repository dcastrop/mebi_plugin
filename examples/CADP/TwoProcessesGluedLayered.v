(* Two processes, glued ~ layered: [weak_bisimilar bigstep lts c2 c2].
   NOT PROVED: every proof of it ran out of memory in [Solve]. It is
   decided bisimilar ([ExploreLarge.v], ~35s, ~1GB). This is the
   [wbis_bigstep_lts] attempt of [GluedLayered.v], alone, so that it is its
   own process.

   Measured 2026-10-12 on a 14GB machine, before [Answers Auto] was the
   default, with [Solve 1000000] and 15000 states: OOM at a 10GB cap after
   28 min. [Begin] takes ~33s, saturating the layered side on demand
   (1,091,776 weak actions over 10,083 silent SCCs, above the 1M bound), and
   [Auto] then takes the nested cofix without estimating. With
   [MeBi Config Bounds Game 100000000] it estimates instead and picks the
   mutual cofix (514,020 pairs, 2,056,080 moves); that run did not finish
   its first 2000-step chunk in 2h28m, with memory flat at ~2.2GB.

   Repeated 2026-10-15/16 on a 31GB machine under [Answers Auto], 24GiB
   cap, [Solve 1000000]: still NOT PROVED. Nested: OOM in [Solve] after
   2h05m ([Begin] 30.6s). With [Bounds Game 100000000]: [Begin] took
   4,489s, then the mutual cofix over 78,202 pairs and 312,808 moves (not
   514,020 and 2,056,080: unexplained, see doc/measurements/cadp-large.md),
   and OOM in [Solve] after 16h00m in all. Memory rose to 12.5GiB in 3h,
   sat at 16.7GiB for about 4.6h, then reached the cap. Not run: this
   proof under [Answers Default] with [Bounds Game], or above 24GiB.

   Run it under a memory cap, as [bench/proofs.sh -s cadp-huge -f
   TwoProcessesGluedLayered.v -M <cap>]. To try the mutual cofix, put
   [MeBi Config Bounds Game 100000000.] before the proof. See
   [doc/future-work.md], "Re-running the large CADP proofs". *)

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
Require Import MEBI.Examples.CADP.Model.
Require Import MEBI.Examples.CADP.Glued.
Require Import MEBI.Examples.CADP.Terms.

MeBi Config Weak As Option label.
MeBi Config Bounds As Num States 15000.

MeBi Divider "Examples.CADP.TwoProcessesGluedLayered".
Example wbis_bigstep_lts : weak_bisimilar bigstep lts c2 c2.
Proof. MeBi Sim Begin bigstep c2 And lts c2 Using step.
  MeBi Sim Solve 1000000. Qed.
