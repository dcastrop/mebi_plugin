Require Import MEBI.Bisimilarity.
Require Import MEBI.Traces.
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

(* The same results as properties of behaviour ([MEBI.Traces],
   2026-10-13). The layered system has exactly the weak traces of the FIFO
   server with joining hidden, and after any trace it can do next what
   that server can ([weak_bisimilar_ready]). Every one of its traces
   alternates [ENTER i] and [LEAVE i]: mutual exclusion as a safety
   property, through [SpecMutex.spec_lts_mutex]. *)

Example traces_lts_fifo_hidden_1 :
  forall t, weak_trace lts c1 t <-> weak_trace spec_fifo_hidden (make_fifo 0) t.
Proof. exact (weak_bisimilar_traces _ _ wbis_lts_fifo_hidden_1). Qed.

Example traces_lts_mutex_1 : forall t, weak_trace lts c1 t -> mutex_trace Free t.
Proof.
  exact (weak_sim_safe (mutex_trace Free) c1 (make_spec 0) wsim_lts_mutex_1
           (make_spec_mutex 0)).
Qed.
