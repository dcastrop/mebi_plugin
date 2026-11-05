# Bitboard plan, Phase 0: where the model's memory goes

Recorded 2026-11-05, for `BITBOARD_LIKE_OPTIMISATION.md` ("Phase 0 —
measure"). Built from `main` at `f5069de` plus a census hook, kept locally
as `notes/tools/census.diff` (not committed).

## Method

When `MEBI_CENSUS` is set, the hook runs right after the bisimilarity
check that `Run Bisim` and `Sim Begin` share (`bisimilarity_of`,
`src/wrapper.ml`). Saturation is finished at that point, and the proof
solver has not started. It prints to stderr:
- for each FSM (each side, original and saturated, and the merge):
  states, labels, edges per label, weak actions, and witness steps;
- the words that `Obj.reachable_words` finds:
  - **edges**: the whole `EdgeMap`;
  - **witness_excl**: what only the witnesses (`Annotation`) hold;
  - **trees_excl**: what only the actions' derivation trees hold;
  - **skeleton**: the rest of the edges, meaning hash tables, `Set`s,
    state and label records;
- the encoding table (`Bi_encoding`, both directions);
- `Gc.stat`'s live, heap and top-heap words.

The exclusive figures are computed by inclusion and exclusion, against the
states and labels, so a value shared with them is not counted twice.

Each file ran through `bench/proofs.sh`, or, for Test4, through the same
copy-and-cap helpers (`bench/lib.sh`, 6G cap).
- **Files:** `CADP/MutualExclusion.v`, `CCS/ABPProofs.v`, and Test4's
  `Run Bisim p … q` under `compLTS`, bound 12000 states (the first
  `Run Bisim` of `Proc/Test4/TermTests.v`).
- **Unit:** one word is 8 bytes.

## Results

### At `Sim Begin`/`Run Bisim`

| file | largest FSM (states, edges) | model, all FSMs | encoding table | live heap | model share | process peak |
|---|---|---|---|---|---|---|
| CADP MutualExclusion (1st command) | 33, 122 weak | 15.7k words (125KB) | 85k (0.7MB) | 8.7M (70MB) | 0.2% | 2.9GB (in later proofs) |
| CCS ABP (`abp <= spec`) | 108, 2,758 weak | 180k (1.4MB) | 263k (2.1MB) | 2.3M (18MB) | 8% | >6GB: OOM-killed in `Qed`, see below |
| Proc Test4 (`Run Bisim`, on demand) | 9,720, 87,480 strong | 12.9M (103MB) | 2.6M (20MB) | 14.6M (116MB) | **89%** | 0.71GB, 29s |

### Inside the model

| FSM | edges | witnesses | trees | skeleton |
|---|---|---|---|---|
| MutualExclusion, saturated (122 weak, 1,089 witness steps) | 10.9k | **8.1k (75%)** | 0 | 2.7k |
| ABP, saturated (2,758 weak, 18,716 witness steps) | 167k | **123k (73%)** | 0 | 44k |
| Test4, original (87,480 strong) | 4.54M | 0 | **2.51M (55%)** | 2.03M (45%) |

Test4's saturated FSMs are empty. The on-demand route decides on the
silent-SCC quotient and never fills the cache, so no witness was built.

Labels: MutualExclusion and ABP have one silent label and two visible ones.
Test4's 87,480 edges are 74,520 silent (85%), and 6,480 for each of the two
visible labels.

## What it says

1. **For proofs, the model is not where the memory goes.**
   - Proof-file peaks (2.9GB, over 6GB) arise in `Solve` and `Qed`. The
     model is at most a few MB of them.
   - In MutualExclusion, the top heap jumps from 9.3M to 340M words (2.7GB)
     during the first proof, with the model at 15.7k words.
   - Shrinking the model's containers (Phases 2 and 3) cannot move a proof
     file's peak memory.
2. **Inside a saturated model, witnesses dominate (73–75%)**, as the plan
   expected. That makes Phase 4 ("witnesses on demand") the phase with an
   effect, but only on large saturated FSMs.
3. **On a large strong FSM, the derivation trees dominate (55%)**, not the
   witnesses. Test4's `Action` records each carry a tree set, and the
   trees are 29 words per edge.
4. **The encoding table is 20MB on Test4** (2.6M words for 11,157
   entries, ~230 words each): the Rocq terms themselves. It is the same
   order as the model's skeleton.
5. **Full saturation of Test4** would be 74,649,600 weak actions, which
   `TermTests.v`'s header estimates at 34–67GB, or 450–900 bytes per weak
   action. As one bit-row per state for each of the two visible labels, it
   is 2 × 9,720² / 8 bytes = **23.6MB**. Here the bitboard representation
   wins by three orders of magnitude, and it is the only place measured
   where it wins at all.

## Side observation: ABP at its suite cap

`CCS/ABPProofs.v` under `bench/proofs.sh -s abp` was OOM-killed at its 6G
cap during `Qed`, after `Solve` finished at the usual 6,494 steps (~110s).
`CLAUDE.md` gives ~4.2GB as its peak. A second run, without `MEBI_CENSUS`
(the hook inactive), was killed in the same place. So the census is not
the cause; the file no longer fits the suite's cap on this host (14GB, OCaml
5.4.1, Rocq 9.2). Not investigated further: `OUTSTANDING.md` lists it.
