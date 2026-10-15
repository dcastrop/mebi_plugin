# Future work

Work that is understood well enough to describe, but not under way.

- **Three sections are about cost:** "The solver's own per-step cost",
  "Memory of the saturation estimate" and "Extraction's derivation
  search is unindexed". Jonah chose on 2026-10-11 to accept such costs
  as current limits, and to report a limit they cause rather than chase
  it: like reformulating CADP to reach larger sizes,
  they are orthogonal to the research question (what the plugin can
  decide and prove). None is a bug. Verdicts and proofs are correct
  whenever they finish, and `Qed` checks every proof. "The solver's own
  per-step cost" and "Memory of the saturation estimate" come first: the
  first touches every proof a user runs, the second only very large
  FSMs.
- **"Proofs split into separately checked lemmas"** is a capability
  (@dcastrop's decision 4, `decisions.md`), the one that would reach
  proofs the plugin cannot produce today.
- **The rest** lists smaller possible work, and what was set aside on
  purpose.

`OUTSTANDING.md` lists each item in one line.

## The solver's own per-step cost

**Standing.** Measured, cause unknown. CPU timers around each solver
step (2026-10-11; patch kept locally as `notes/tools/solver-profile.diff`)
separate the plugin *deciding* the next step (building its tactic) from
Rocq *running* that tactic:

| proofs | decision share of `Solve` | decision per step |
|---|---|---|
| `Proc/Test1`..`Test3`, `CCS` | 20-30% | 0.4-0.8ms |
| `Evaluation/Width`, `Layers` | 30-40% | 0.55-0.67ms |
| `Evaluation/Depth` | 65% | 1.5ms |
| CADP `Size1` | 40-80% | 2-8ms |

So deciding is cheap on small terms and ~10× dearer per step on CADP,
whose states carry the whole program text.

**Why it matters.** It is in every `MeBi Sim Solve`, so it would speed
up every proof, including the checked-in ones. It may share a cause with
CADP `Size3`'s proof memory, which is also unexplained: a proof over
`bigstep_norm`'s 644 states was OOM-killed at 8-9GB
(`examples/CADP/MutualExclusionLarge.v`), while `Size2`'s over
`bigstep`'s 772 peaks at 4.4GB. One hypothesis, unprobed: each state
carries the whole program text, so the proof term grows with term size
as well as with steps.

**Not known.** Whether the cost is a constant per term node (then it
only scales with model size) or something that grows within a proof
(lookups or conversions on states, say). That decides whether a fix is
small.

**Where to start.** Rerun the patch on CADP with finer timers inside
`Proof_solver_step` (state lookup, transition lookup, the choice of
answer) and see which one grows with term size. Then compare one
glued `Size1` proof against its normalised counterpart, which has
smaller states.

**Relation to other items.** Replaying a derivation tree in one step
(below) would still make each decision, so this is the larger lever of
the two.

## Memory of the saturation estimate

**Standing.** Cause found; not fixed. `Model.SaturationEstimate` keeps,
for each of an FSM's K silent SCCs, a dense bitset over all K
(`tau_reach`), and another K² bits per label (`weak_of`). `Run Bisim`
and `Sim Begin` compute it after extraction, to choose between whole
and on-demand saturation. `Run FSM` does not. Measured in pure OCaml on
a silent DAG: heap +27, +101 and +402MB at K = 10k, 20k and 40k (×4 per
doubling).

**Where it bites.** Glued CADP has one silent SCC per state (`Size2`:
772 of 772), so glued `Size3` (128,044 states) needs ~4GB for the
estimate on top of the ~4.7GB extracted FSM, and was OOM-killed at 9GB.
CADP sidesteps it with `bigstep_norm` (644 states). Any user model with
~100k or more silent SCCs needs gigabytes for it (`limitations.md`,
"Many silent SCCs").

**What a fix involves.**
- The estimate only needs counts, and the guard only "above the bound
  or not", so it can run in memory linear in K, for example by stopping
  once the bound is passed. That part is small.
- The on-demand bisimilarity check (`scc_moves`, `refine_blocks`) reads
  the same `reach` and `weak_of` sets. A fix to the estimate alone may
  move the OOM there. Sparse sets help when the weak moves are few. When
  they are many, they *are* the saturation's size, which no
  representation avoids.
- It touches the model, so it needs the full proof-suite runs and the
  saturation golden test (`satdiff`).

## Proofs split into separately checked lemmas

**Kind:** *capability* (large). A new proof back end, not a heuristic.

**Standing.** Not started; discussed 2026-10-12. Motivated by CADP
`Size2` glued ≈ layered (`examples/CADP/GluedLayered.v`). It is decided in ~35s, but every proof
attempt ran out of memory *during `Solve`*, before `Qed` was reached.
Both `weak_sim`s and the `weak_bisimilar`, on the nested path, were
OOM-killed at 7GB and at 10GB, and memory grew steadily to the cap. The
mutual path's game has 514,020 pairs and 2,056,080 moves.

**Why one proof cannot simply be cut up.** Today a proof is one `cofix`.
A pair that reaches a pair already under proof closes the cycle with the
coinduction hypothesis, which exists only inside that term. The product
has cycles, so lemmas per state would have to use each other, and Rocq
has no mutual dependency between separately proved lemmas.

**The way round.** Prove bisimilarity through an explicit relation:
- Define R, the product's pairs, as a finite relation. Name each state
  once (`s1 := ...`), so that R is written over names. CADP states carry
  the whole program text.
- In `theories/`, prove once that a weak bisimulation (or simulation)
  containing `(p, q)` gives `weak_bisimilar p q` (or `weak_sim`). It is
  a small fixed `cofix`. `theories/Bisimilarity.v` has no such lemma
  today.
- Show R is a bisimulation, one obligation per pair: each move has an
  answer that lands back in R. Landing in R is a membership check, not
  the coinduction hypothesis, so the obligations are independent. Group
  them into lemmas by block (for example, per glued state) and blocks
  into files.

A `Qed`'d lemma is opaque. Later proofs use only its statement, and
`Require` does not load its body. So peak memory is about one block, and
files compile in parallel. The plugin already computes what this needs:
the partition, and the pairs and answers that the mutual path uses.

**What it does not fix.** The total work is still one derivation per
move (about 2M at `Size2`), so time stays large, although it is spread
over files. The statement of R and the per-block lemmas add to the
environment.

**Relation to other items.** It is complementary to "The solver's own
per-step cost", which makes each step cheaper. It is independent of
"Proving one state per silent SCC" (set aside, below): R relates states,
not SCCs.

## Extraction's derivation search is unindexed

**Standing.** Cause found 2026-10-13; not fixed. On the ABP with a data
domain of n values, extraction's time per state grows with n: 26, 37, 49,
62 and 77ms for n = 1..5. Within one run it does *not* grow with the
states already seen (at n = 5 it falls from 101ms a state in the first
fifth to 59ms in the last), and no state is visited twice. So it is not
the bug the earlier question feared.

**Cause.** All the time is in finding a state's transitions
(`collect_valid_constructors`). For each state it tries every
constructor and every branch of the source term by unification: 1,753
`w_unify` attempts a state at n = 1, about 600 more for each extra datum,
to find ~2.7 transitions. Roughly half fail outright, and most successes
belong to derivations that fail further down. Each attempt costs about the
same (6-9µs in `w_unify`, ~45% of the time; the rest scales with the
attempts too). `sums n` gives the sender, receiver and media n
alternatives each, hence the slope.

**What a fix involves.** Fewer attempts: skip a constructor whose
conclusion's head cannot match the source term's, before unifying (an
index on head symbols), and prune a premise's branches the same way. It
touches the proof solver's view of derivations, so it needs the full
proof-suite runs. How much it would save is unmeasured. The measurement
patch is kept locally (`notes/tools/extraction-profile.diff`).

## Smaller possible work

**Evaluation** (detail in Jonah's local notes 10 and 12)
- **Compare with CADP and mCRL2**: verdicts and cost on the same models.
  Neither tool is installed; CADP needs a licence.
- **Scale the evaluation levers further** (`bench/proofs.sh -s
  eval-large`). Stopped at 128 layers (Qed alone would take on the order
  of 15 min) and at depth K = 32 (~260k steps extrapolated).

**Optimization**
- **Replay a derivation tree in one solver step** (*optimization*,
  measure first). Today each step applies one constructor of a
  transition's derivation tree, in preorder
  (`Proof_solver_step.handle_appconstrs_update_args`), so a layer adds 14
  steps (`Layers.v`). Why it is not simple batching:
  - each application reads the goal the previous one left;
  - premises open sibling goals, which a step cannot see, because steps
    run focused on the first goal.

  So it would be one tactic inspecting goals as it runs. It would save only
  the solver's per-step overhead: the proof term, and so `Qed`, would be
  unchanged, and at depth `Qed` dominates (`limitations.md`, "Deep
  derivations"). Every checked-in bound
  would change.

  Measured 2026-10-11 (CPU time inside `Solve`, default and `eval`
  suites): the plugin's own decision of what to do next is 20-30% on
  `Proc` and the CCS laws, 40-80% on CADP and 65% on `Depth`. Rocq
  running the chosen tactic (with `Declare.Proof.by`'s bookkeeping) is the
  rest. Replay would still make each decision, so it could save at most
  part of the tactic share. The decision cost ("The solver's own
  per-step cost", above) looks like the larger lever.
- **`Auto` mis-estimate** (*optimization*, small). On `LawProofs.tau1`,
  `Auto` picks nested (42 steps) where mutual takes 29. A data point for
  `Product.estimate`, if it is revisited.

**Reusable theory** (agreed 2026-10-11: machinery a user could reuse
goes into `theories/` once two examples use it, which keeps `examples/`
minimal): nothing open. The last item, trace preservation, became
`theories/Traces.v` on 2026-10-13.

**Examples**
- `examples/Bisimilarity/_nat_streams.v`: unfinished, kept as real work,
  status unclear.
- CADP `Size2`/`Size3` proofs: see `OUTSTANDING.md`.

## Set aside (deliberately)

Kept so they are not rediscovered as new. Each could be reopened, with the
reason in mind.

- **Proving one state per silent SCC** (abandoned 2026-10-03): unsound
  inside a coinduction, counterexample `CircularTransfer`.
- **`ReModel` unification instead of lookup** (A1): measured, cannot miss
  in current usage. Its value would be goals with an unknown state, closed
  as "give the witness by hand" (`limitations.md`, "Goal shapes").
- **Inversion tie-break options B and C**: closed unbuilt; residual
  sterile steps measured at zero after options A and D′.
- **Divergence-sensitive equivalences**: out of scope for the current
  definitions.
- **Module lists** (`_CoqProject`, `.mlpack`, dune) are still maintained
  by hand. A drift is caught by `scripts/check_module_lists.py` (CI)
  rather than prevented.
- **odoc warnings**: 72 remain, all `@raise` tags naming stdlib or Rocq
  exceptions, which odoc cannot resolve. Accepted.
