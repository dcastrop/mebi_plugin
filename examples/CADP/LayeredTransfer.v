Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.Model.
Require Import MEBI.Examples.CADP.Glued.

Require Import MEBI.Examples.CADP.Terms.
Require Import MEBI.Examples.CADP.SpecFifo.
Require Import MEBI.Examples.CADP.SpecMutex.
Require Import MEBI.Examples.CADP.Hidden.
Require MEBI.Examples.CADP.GluedLayered.
Require MEBI.Examples.CADP.Fifo.

(* The paper's method, end to end, for one process (added 2026-10-13):
   properties proved for the glued semantics ([bigstep]), carried to the
   faithful layered one ([lts] over [step]). No plugin calls: each result
   chains two proved bisimilarities with the generic lemmas of
   [MEBI.Bisimilarity].

   - [GluedLayered.wbis_bigstep_1]: glued ≈ layered;
   - [Fifo.wbis_bigstep_hidden_1]: glued ≈ the FIFO lock server with
     joining the queue hidden ([spec_fifo_hidden], [Hidden.v]).

   So layered ≈ [spec_fifo_hidden]: the layered system serves one process
   at a time, in the order the server's hidden queue allows. Mutual
   exclusion for layered then follows from [Fifo.wsim_hidden_spec_1],
   independently of the direct proof in [MutualExclusion.v].

   Only one process: glued ≈ layered is not proved for two
   ([GluedLayered.v]). *)

Example wbis_lts_fifo_hidden_1 :
  weak_bisimilar lts spec_fifo_hidden c1 (make_fifo 0).
Proof.
  apply (weak_bisimilar_trans lts bigstep spec_fifo_hidden c1 c1 (make_fifo 0)).
  - apply weak_bisimilar_sym, GluedLayered.wbis_bigstep_1.
  - exact Fifo.wbis_bigstep_hidden_1.
Qed.

Example wsim_lts_mutex_1 : weak_sim lts spec_lts c1 (make_spec 0).
Proof.
  apply (weak_sim_trans lts spec_fifo_hidden spec_lts c1 (make_fifo 0) (make_spec 0)).
  - apply weak_bisimilar_sim, wbis_lts_fifo_hidden_1.
  - exact Fifo.wsim_hidden_spec_1.
Qed.
