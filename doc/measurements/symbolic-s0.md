# Symbolic plan, Phase S0: does the encoding compress?

Recorded 2026-11-05, with the prototype in
[`bench/symbolic/`](../../bench/symbolic/) (its README says what it does).
- **Host:** 14GB.
- **Caps:** every run under `systemd-run`, `MemoryMax=2G`,
  `MemorySwapMax=0`.
- **Toolchain:** OCaml 5.4.1; `mlcuddidl` 3.0.8.

## Correctness first

On every system small enough for the explicit oracle (the plugin's own
`Model.Bisimilarity.fsm`), the symbolic verdicts and reachable-state
counts agreed:
- `proc` and `proc-bad`, `n` = 2–4 (Test4 itself at `n` = 4: 81 states a
  side, bisimilar);
- `buffers` and `buffers-bad`, `n` = 2–10;
- `abp`: 108 states against 2, bisimilar, as `CCS/ABPBisimProofs.v`
  proves.

The bad variants come out not bisimilar on both routes.

## Scale (CUDD backend unless noted)

| system | states a side | verdict | R's nodes | peak nodes | iterations | seconds |
|---|---|---|---|---|---|---|
| `abp` | 108 / 2 | bisim | 51 | 10k | 2 | 0.00 |
| `proc-16` | 4.3×10⁷ | bisim | 65 | 97k | 1 | 0.06 |
| `proc-32` | 1.9×10¹⁵ | bisim | 129 | 652k | 1 | 0.90 |
| `buffers-16` | 65,536 / 17 | bisim | 172 | 386k | 17 | 0.22 |
| `buffers-20` | 1.0×10⁶ / 21 | bisim | 253 | 1.1M | 21 | 1.2 |
| `buffers-24` | 1.7×10⁷ / 25 | bisim | 351 | 2.7M | 25 | 4.4 |
| `buffers-28` | 2.7×10⁸ / 29 | bisim | 464 | 5.0M | 29 | 15.7 |
| `buffers-32` | 4.3×10⁹ / 33 | bisim | 597 | 8.5M | 33 | 56.9 |
| `buffers-20-vs-19` | 1.0×10⁶ / 20 | not | – | 1.4M | 40 | 1.9 |

(`proc` under `zip`, the rest under `blocks`. `zip` was 1.3–1.9× slower, with 1.3–2.4× the peak, on
`buffers`. On `abp`, and on `proc` up to `n` = 4, the two orders were
within noise; `proc` above 4 was run under `zip` only.)

**Against the explicit route on the same systems:**
- `buffers-10`: 0.02s symbolic, 1.65s explicit.
- `buffers-12` (4,096 states against 13): the explicit check hit the
  model's memory guard at 1.9GB (`Memory_guard.Exceeded "saturation"`).
  Saturating the chain is what blows up: every weak step is stored with
  its witness. The symbolic route does 32 cells (4.3×10⁹ states) in 57s
  under the same cap.

(A simple BDD written in OCaml was also tried as a backend, and dropped:
CUDD was an order of magnitude faster and collects dead nodes.)

## What it says

1. **The encodings compress, by orders of magnitude.**
   - **The relation is tiny:** `R`, the largest weak bisimulation, is
     under 600 nodes for 4.3×10⁹ pairs. That relation is what Phase S4
     would export as a certificate. At this size, exporting it to Rocq is
     plausible.
   - **The cost is in intermediate results:** the peak, 8.5M nodes at 32
     cells, about 0.3GB, comes from the preimages inside the τ\*
     fixpoints, not from `R`.
2. **Normalisation does most of the work for Test4.** Normalised, it is
   81 states a side and instant. Generalised to 32 components, it stays
   seconds.
   - **Caveat:** in `proc` every reachable pair is bisimilar, so `R` is all
     of `reach × reach`, an easy case.
   - **The real test is `buffers`:** `R` there is a non-trivial relation,
     "the chain holds as many items as the counter".
3. **BDD library: CUDD**, through `mlcuddidl`, which builds and runs on
   OCaml 5.4. Its OCaml 5 port "removes some modules" (its install note);
   none that the prototype needs. Dynamic reordering is off, so the variable
   order is fixed and results are reproducible.
4. **Variable order matters, but not dramatically, on these families.**
   - `blocks` beat `zip` on `buffers`. The counter is one component, so
     `zip` only interleaves it with the chain's first cell.
   - A system whose bisimulation pairs components one to one should favour
     `zip`. S2 has to choose the order per system.

## Not done in this round

- **CADP glued `Size2`**, which the plan lists for S0, is not ported. Its
  semantics (`examples/CADP/Model.v`, with memory and expressions) does not
  fit the two front ends here. It needs a data-carrying component, which
  is a step towards S2 and S6, not S0.
- **No certificate** is produced. S0 decides only.
- **The property-initialised query** (`I = φ(x) ∧ ψ(y)`) is not exercised.
  The fixpoint already computes `R` over all reachable pairs, so `I ⊆ R`
  is one inclusion test away. S5 needs a syntax for `φ` and `ψ`.
