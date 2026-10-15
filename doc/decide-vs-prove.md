# Deciding vs proving: what each costs

MeBi answers a bisimilarity question in two ways:
- **Deciding.** `MeBi Run Bisim` extracts both LTSs, saturates them and
  refines a partition. It gives a verdict but no Rocq proof.
- **Proving.** `MeBi Sim Begin` runs the same check. `MeBi Sim Solve`
  then builds a coinductive proof one tactic step at a time, and `Qed`
  checks it.

The two share the check, so their difference is the cost of the proof.
This note puts the two side by side on the checked-in examples. It is
evaluation data for the paper: where a proof is affordable, what it costs
relative to the verdict, and where it stops.

## How it was measured

- **Machine and build:** an Intel Core Ultra 5 125U with 14GB of RAM,
  against the dune build of `main` at `1514bf1` (2026-10-13).
- **Decisions:** each `Run Bisim` ran alone in its own process, under a
  6GB cap, from a copy of `examples/`. *Time* is that command's own time
  (`rocq compile -time`). *Memory* is the process's peak, which includes
  Rocq's baseline of ~0.3GB.
- **Proofs:** the per-command times of each proof's `Begin`, `Solve` and
  `Qed`, summed, taken from the same day's compile of its checked-in file.
  The peak memory of a proof is the peak of the whole file. Most files
  hold several proofs, so that figure is an upper bound for any one of
  them.
- **Older figures:** rows marked † were not rerun. They give the date and
  source of their figures.
- **Counts:** step counts are `Solved after N`, as checked in
  (`CLAUDE.md` holds the baseline).

## The table

"≈" is weak bisimilarity (`weak_bisimilar`), "≤" a weak simulation
(`weak_sim`). States are the larger side's.

| system pair | states | decide | prove | ratio |
|---|---|---|---|---|
| **CADP, one process** | | | | |
| glued ≈ layered | 33 | 0.14s, 0.40GB | 2,760 steps, 39s | ~290× |
| layered ≈ `spec_lts` | 33 | 0.13s, 0.40GB | 1,442 steps, 6.2s | ~50× |
| glued ≈ `spec_lts` | 10 | 0.04s, 0.33GB | 185 steps, 1.7s | ~45× |
| normalised (observing) ≈ `spec_fifo` | 5 | 0.03s, 0.32GB | 119 steps, 1.1s | ~40× |
| **CADP, two processes** | | | | |
| glued ≈ layered | 12,027 | 45s, 1.48GB | **not proved**: OOM at 10GB (nested, 25-52 min); mutual unfinished after 2.5h † | — |
| normalised (observing) ≈ `spec_fifo` | 48 | 0.24s, 0.40GB | 2,228 steps, 24.5s | ~100× |
| glued vs `spec_lts` | 772 | not bisimilar, 2.1s, 0.47GB | ≤ both ways: 11,128 steps, 9.9 min, ≤4.4GB; and 405 steps, 4.9s | ~280× |
| normalised vs `spec_lts` | 48 | not bisimilar, 0.25s, 0.40GB | ≤ both ways: 1,038 steps, 14.5s; and 153 steps, 1.6s | ~65× |
| **CADP, three processes** | | | | |
| normalised (observing) ≈ `spec_fifo` | 644 | 4.8s, 0.64GB | **not proved**: OOM at 9GB after ~15 min † | — |
| normalised vs `spec_lts` | 644 | not bisimilar, 4.6s, 0.61GB | spec ≤ system: 326 steps, 9.2s. System ≤ spec: **not proved**, OOM at 8-9GB after 11-13 min † | — |
| **Proc `Test4`** (four components) | | | | |
| `p` ≈ `q`, direct | 9,720 | 42s, 0.98GB (saturated on demand) | not attempted: ~525k steps predicted † | — |
| `p` ≈ `q`, normalised | 82 | 0.45s, 0.36GB | 61,161 steps, ~4 min, ~2.8GB (`Answers Minimal`) †; `weak_sim` alone 48,821 steps, ~7 min, ~3.8GB † | ~500× |
| **CCS, Alternating Bit Protocol** | | | | |
| `abp` ≈ one-place buffer | 108 | 3.4s, 0.36GB | 9,914 steps, ~3.8 min, ~4.2GB †; `abp` ≤ buffer alone 6,494 steps, ~3.7 min † | ~65× |

The figures marked † are recorded in:
- the header of `examples/CADP/GluedLayered.v` (2026-10-12);
- `examples/CADP/MutualExclusionLarge.v`, and `examples/README.md`'s
  "No starvation" table, for three processes (2026-10-11);
- `examples/Bisimilarity/Proc/Test4/TermTests.v`, `NormProofs.v` and
  `NormBisimProofs.v`;
- `_CoqProject`, for the ABP.

The state counts are from the decision runs' own output. For
CADP with one process, "33" is the layered side. The glued side and the
spec are smaller.

## What it shows

1. **Proving costs one to three orders of magnitude more than deciding,**
   even where both are cheap. Every proved row takes 40-500× its
   verdict. The verdict is a partition refinement over states. The proof
   replays a move-by-move argument over the pairs it visits, as Rocq
   tactic steps, and `Qed` re-checks the whole term. `Qed` alone is
   often comparable to `Solve`: 7.5s of the 39s for one-process glued ≈
   layered, and 9.6s against `Solve`'s 4.5s for its `lts` ≤ `bigstep`
   direction.

2. **Where a proof stops, the verdict usually still comes cheaply.**
   - Two-process glued ≈ layered is decided in 45s and 1.5GB, but no
     proof finished within 10GB or 2.5 hours.
   - Three-process FIFO order is decided in 4.8s, but its proof ran out
     of memory at 9GB.

   So for these sizes, the plugin's result is a decision, not a proof.
   `OUTSTANDING.md` records this as a limit, and `future-work.md`
   ("Proofs split into separately checked lemmas") a possible way round.

3. **A proof's cost follows the game, not the state count.**
   - Two-process glued vs `spec_lts` (772 states) is proved in 10 min.
   - Three-process normalised vs `spec_lts` (644 states) is not, in the
     system ≤ spec direction.
   - The direction matters as much as the size: at three processes,
     spec ≤ system takes 326 steps, while system ≤ spec ran out of
     memory.

   What grows is the number of pairs the proof visits and the moves it
   answers. `Auto` estimates both before choosing a strategy
   (`Model.Product.estimate`).

4. **Shrinking the model pays more for proofs than for decisions.**
   Normalising erases dead data (CADP) or makes structural congruence
   explicit (`Test4`), and a hand-proved theorem carries results back:
   - CADP, two processes, mutual exclusion: the decision gets 8× faster
     (2.1s to 0.25s), the proof ~40× (9.9 min to 14.5s).
   - `Test4`: the decision gets ~90× faster (42s to 0.45s), and the proof
     goes from out of reach to ~4 min.

   This is the method `doc/structural-congruence.md` describes, and the
   reason the CADP example has three semantics.

## Inside deciding: extraction

Most of a decision's time is extraction. Its cost per state is not
constant. On the ABP with a data domain of n values (a local series; n = 1
is `CCS.v`'s ABP, 108 states), states grow linearly (108, 220, 344, 480,
628 for n = 1..5), and so does the time per state: 26, 37, 49, 62, 77ms.
Total time is therefore roughly quadratic in n.

It does not grow with the states already seen. Extraction finds each
state's transitions by trying every constructor and every branch of the
source term by unification: 1,753 attempts a state at n = 1, about 600
more for each extra datum, for ~2.7 transitions found. The slope is the
number of alternatives in the term (`sums n`), not a cost that compounds.
Cutting the attempts is future work (`future-work.md`, "Extraction's
derivation search is unindexed").

## Not measured

- **ABP and `Test4` proofs:** not rerun on this build. The step counts
  are pinned by their checked-in bounds, and the times are from when
  those files were last run.
- **The two-process glued ≈ layered proof:** not rerun either. A rerun
  costs hours and up to 10GB, and nothing in the solver has changed since
  2026-10-12.
- **Variation:** each figure is a single run. Timings of a second or
  less vary by tens of percent between runs, so the small ratios are only
  orders of magnitude.
- **Cost per pair or per step:** not separated here. `future-work.md`
  ("The solver's own per-step cost") has the split of solver time
  between deciding the next step and running
  its tactic.
