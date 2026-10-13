# Outstanding

Everything still open about `mebi_plugin` in one place: decisions, known
limits, possible work, and what was set aside on purpose. This file should
**only shrink**. When an item is resolved:
- delete it here;
- record how it was resolved in `ASSISTED-CHANGES.md`.

Add an item only when something new is found, with a pointer to its
detail.

It replaces `TODO.md` (deleted 2026-10-10; the old list is `git show
a1b1470:TODO.md`) and the open-item lists in `ASSISTED-CHANGES.md`
("Outstanding") and Jonah's local planning notes (`notes/`, not in the
repository). `ASSISTED-CHANGES.md` keeps the history; this file holds
what is still open.
Started 2026-10-10. Each item names its kind:
- *decision*: someone has to choose;
- *limit*: what the tool cannot do today;
- *bug?*: suspected, not yet confirmed;
- *capability*, *optimization*, *evaluation*: possible work, sized
  roughly;
- *future work*: understood well enough to describe, accepted for now as
  a limit, and worth pursuing later (§5);
- *set aside*: decided against, or out of scope, with the reason.

## 1. To discuss with @dcastrop

His repository and project, so his decisions. Merged changes went in one
merge commit per branch, and `ASSISTED-CHANGES.md` gives each one's revert
command, so any of them can be backed out if he disagrees.

**Repository**
- **LICENSE** (*decision*). There is none, and `dune-project`'s
  `(license ...)` is commented out, so the opam file has no licence
  either. Needed before any release.
- **`paper/`** (*decision*). 73 tracked PDFs, ~36MB, 96% of the
  repository, untouched for ~18 months.
  - Deleting it from `HEAD` does not shrink existing clones; that needs
    `git filter-repo` and a coordinated force-push.
  - Redistributing third-party papers from a public repository raises a
    licensing question.
  - Small related item: `paper/references/to check/Affeldt.pdf` has a
    ligature (U+FB00) in its name and a space in its directory, both
    portability hazards.
- **Upstreaming** (*decision*). All work so far is on `thecathe/
  mebi_plugin` (PRs #1-#60, each merged separately, each with a revert
  recipe in `ASSISTED-CHANGES.md`). It is meant to reach `dcastrop/
  mebi_plugin` as one PR, best after the decisions below, so that bounds
  are not re-measured after review.

**Defaults** (each changes what a checked-in `MeBi Sim Solve N` bound
means, so deciding one way means re-measuring every bound)
- `MeBi Config Solver MutualCofix Auto` as the default (*decision*;
  `ASSISTED-CHANGES.md` 2026-09-29). The solver can introduce coinduction
  hypotheses as a nested `cofix` per new pair (what it always did) or as
  one mutual `cofix` over the precomputed product. `Auto` costs both on the
  model and picks one, with a `Notice` when it goes mutual. Over the 18
  cheap proofs it takes 2565 steps, against 3794 always-nested and 2654
  always-mutual, with no regression anywhere. Against it: it can silently
  change a proof's iteration count. Two sub-questions:
  - whether to tighten `Proc/Test2`'s deliberately loose bounds;
  - whether `Proc/Test3` should drop its explicit `MutualCofix True`.
- `MeBi Config Solver Answers Auto` as the default (*decision*; PR #9,
  `ASSISTED-CHANGES.md` 2026-10-02). It plans every answer at `Sim Begin`
  and keeps the cheapest of three policies. On all 41 checked-in proofs it
  was never slower than today's `Default`. It cut `weak_sim` proofs by 17%
  and `weak_bisimilar` proofs by 60% (Test3 `wbis_p3`: 14,427 → 3,579). It
  was not measured with `MutualCofix` forced.

**Semantics** (Jonah's decisions of 2026-10-02, for review)
- Classical, divergence-insensitive weak bisimilarity is the target
  notion. A τ-loop and a stuck state are therefore equivalent (see §3).
  The checker decides it properly since PR #1 (rooted verdict, `=ε⇒`
  split). Before, `Run Bisim` could call `a.b.x` and `b.a.y` bisimilar.
- `weak_bisim` in `theories/Bisimilarity.v` is mutual similarity, which is
  strictly weaker than bisimilarity: `a.b + a` and `a.b` satisfy it but
  are not bisimilar (proved in `Test.v`). `mutual_sim` (its honest name) and
  `weak_bisimilar` (a real bisimulation) were added beside it. *Decision:*
  deprecate or rename `weak_bisim`, and whether the `ManualProofs.v`/
  `LtacProofs.v` theorems should be restated.
- `MeBi Sim Begin` accepts `weak_sim` goals between similar but not
  bisimilar states, and proves `weak_bisimilar` goals.

**Provenance**
- `rocq-sims` (N. Chappe, POPL'26) overlaps in subject, not in method. An
  audit of the assisted commits found no copying; the 13 earliest assisted
  commits have no session transcript.

## 2. To write together (Jonah, fresh session)

### CADP "no starvation"

**Kind:** example/property. Designed with Jonah on 2026-10-11; the design,
and why mutual similarity is not enough, is in
[`examples/README.md`](examples/README.md#cadp) ("CADP").

**Done (2026-10-11):** FIFO order is stated as weak bisimilarity with
`examples/CADP/SpecFifo.v`'s `spec_fifo`, with joining the queue visible
(`Obs.bigstep_obs`). It is proved for one and two processes (119
and 2,228 steps, over the normalised semantics, transferred by
`wbis_transfer_obs`) and decided for three (~4s). No starvation then
follows under a fair scheduler, which is stated, not proved.

**Still open:**
- **The layered semantics** (the paper's central CADP claim). Glued ≈
  layered is *proved* only at `Size1`. At `Size2` it is decided (~35s,
  bisimilar) but not proved (772, or 48 normalised, against 12,027
  states). The proof was attempted on 2026-10-12 and is a *limit*. Both
  `weak_sim`s and the `weak_bisimilar` ran out of memory in `Solve`, at
  7GB and at 10GB, after 25-52 min on the nested path. On the mutual path (514,020 pairs,
  2.06M moves), `Solve` did not finish its first 2,000 steps in 2.5 hours,
  with memory flat at ~2.2GB. Where that time goes is unmeasured.
  The measurements are in `examples/CADP/GluedLayered.v`, and a way
  round is §5.3. The transfer corollary is stated at `Size1` only
  (`LayeredTransfer.v`: layered ≈ FIFO with joining hidden, and layered
  mutual exclusion from it). So FIFO and `Size2` mutual exclusion are,
  today, results about the glued model. To do: the same observing label
  in `step` (a model change, so ask first), for FIFO order proper on
  layered.
- **`Size3` proofs.** Both `weak_bisimilar … spec_fifo` and mutual
  exclusion over `bigstep_norm` (644 states) run out of memory; see
  below.

**Size (measured 2026-10-11):**
- `Size2` (two processes): layered 12,027 states (~32s), glued 772.
  Glued `Size2` (`MutualExclusionLarge.v`) is mutually similar to `spec_lts`, not
  bisimilar, and is now proved both ways (11,128 and 405 steps; the first
  ~10 min, ~4.4GB).
- `Size3` (three processes, where FIFO becomes interesting): glued
  128,044 states (extraction ~19 min, ~4.7GB), normalised
  (`Norm.bigstep_norm`) **644**. Through `bigstep_norm`, spec ≤
  system is proved (326 steps, ~7s). Mutual exclusion proper (system ≤
  spec) is not proved: its proof was OOM-killed at 8GB (~11 min) and,
  with `Answers Minimal`, at 9GB (~13 min). It is decided indirectly,
  through the FIFO bisimilarity above. Why a proof
  over 644 states needs so much more memory than `Size2`'s over 772
  (`bigstep`, 4.4GB) is unmeasured; one hypothesis is that each state
  carries the whole program text, so the proof term grows with term size
  as well as steps (*limit*/*optimization*, unprobed).
- `Run Bisim` on glued `Size3` against `spec_lts` was OOM-killed at 9GB
  twice, after extraction. `bigstep_norm` sidesteps it for CADP. The
  cause, found 2026-10-11, is in §5 ("Memory of the saturation estimate").
- Glued ≈ normalised is proved by hand for every size
  (`Norm.link`); the observing variant should be built over
  `bigstep_norm` too.

## 3. Limitations of the tool

Each is current behaviour. Where pinned, a test in `theories/Test.v`
fails on purpose (`KNOWN LIMIT` / `KNOWN WRONG`), so lifting the limit
turns the pin into a positive test.

**Extraction (building the LTS)**
- **Undecidable premises are assumed** (*limit*, pinned `KNOWN WRONG`:
  `UndecidedPremise`, `GeneralPremises`). Examples: an equation MeBi
  cannot decide, an opaque function or axiom with no user tactic, or a
  proof search cut off by `MeBi Config Premise Depth` (default 16). The
  constructor is then applied as if the premise held, with a warning. A
  `Run Bisim` verdict on such an LTS may be wrong; a proof cannot be,
  because `Qed` checks every premise. Seen 2026-10-10:
  `DepthLarge.v` at K = 16 needs the depth raised.
- **LTS premises whose source nothing determines** find only some of their
  steps (*limit*, warned; pinned `OutputPremises.open_u`/`open_ur`).
- **Self-referential "collapsing" definitions** with a silent collapse
  (B1) are infinite with no finite quotient, and are refused at the state
  bound (*limit*, pinned `Collapsing`). Guarded variants work.
- **LTSs with parameters** are refused (`LTS_Has_Parameters`; *limit*,
  pinned `ParameterisedLTS`). This rules out an LTS generic in its label
  type, or one defined in a `Section`. Workaround: fix the values in a
  module (`examples/Evaluation/Depth.v`). Making this work would be a new
  *capability* (medium). Sketch:
  - accept a term (`constr`) where an LTS is named;
  - instantiate the parameters in the constructor types (`mind_nf_lc`);
  - apply them wherever extraction and the proof solver rebuild the LTS
    term.
- **Size.** Extraction costs ~0.01-0.07MB per state, and is roughly
  quadratic in time on the ABP-with-data series. Whole saturation is
  refused above `Bounds Saturation` (default 1M weak actions;
  `Saturation_Too_Large`). Above that, `Run Bisim` and `Sim Begin`
  saturate on demand and decide on the silent-SCC quotient.
- **Many silent SCCs.** `Run Bisim` and `Sim Begin` need memory
  quadratic in an FSM's silent SCCs, whatever the saturation bound
  (~0.6GB at 50k SCCs, ~4GB at 128k; glued CADP `Size3` was OOM-killed
  at 9GB). `Run FSM` is not affected. Accepted as a limit for now (§5).

**Goals and proofs**
- **Goal shapes** (*limit*, pinned `GoalShapes`). Only `weak_sim` and
  `weak_bisimilar` goals between closed terms are accepted.
  - **Open terms** are refused: a law for every `p` cannot be started.
  - **Local names** are not found, because `Begin` reads terms in the
    global environment.
  - **An `exists` goal** needs its witness given by hand (`exists t.`).

  Witness search and open terms would be new *capabilities*.
- **One state per silent SCC cannot be proved inside a coinduction**
  (*limit*; counterexample pinned as `CircularTransfer`). Very large
  systems (`Proc/Test4`, 9720 states) are therefore *decided* on demand
  but not *proved* directly. `Test4` is proved through an explicit
  structural congruence instead (`doc/structural-congruence.md`). That is
  why `Test4` has no `PluginProofs.v`.
- **Divergence.** `weak_sim` and `weak_bisimilar` ignore divergence, as
  their definitions do. Divergence-sensitive notions (as in `rocq-sims`)
  are out of scope (§6) unless @dcastrop wants them.

**Cost**
- **Proofs cost far more than deciding.** At width n = 3 (162 states a
  side), `Bisimilarity.fsm` takes 19ms and the proof 7 minutes (56k
  steps, 3.9GB). CADP `Size2` glued ≈ layered is decided in ~35s and
  ~1GB, but its proof runs out of memory at 10GB (§2).
- **`Auto` goes nested on an FSM saturated on demand** unless `MeBi
  Config Bounds Game` is set, because estimating would walk the whole
  game. It says so in a notice. For a `weak_bisimilar` goal the nested
  path is the one that does not finish on the checked-in proofs. At CADP
  `Size2` the estimate (~1 min) put nested at over 10.3M goals, against
  514,020 pairs and 2.06M moves mutual. Setting the bound by hand avoids
  it. Whether `Auto` should estimate by default there is an open
  *optimization* question.
- **Heavy proofs need a memory cap**, one per process. The ABP and
  `Test4` proofs peak at ~3.8-4.3GB; two such proofs in one file were
  OOM-killed at 6GB.
- **Deep derivations make `Qed` dominate.** At 64 layers, `Qed` takes
  71s against 7s of solving, and `Qed` grows ~13× per doubling of the
  layers.

## 4. Possible work

**Profiling and evaluation** (*evaluation*; detail in Jonah's notes 10
and 12)
- **Time per state in extraction** grows with size (27ms → 60ms per state,
  ABP-with-data n = 1..4). Cause unknown: term size, label count, or
  something linear in the states seen so far (which would be a bug).
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
  unchanged, and at depth `Qed` dominates (§3). Every checked-in bound
  would change.

  Measured 2026-10-11 (CPU time inside `Solve`, default and `eval`
  suites): the plugin's own decision of what to do next is 20-30% on
  `Proc` and the CCS laws, 40-80% on CADP and 65% on `Depth`. Rocq
  running the chosen tactic (with `Declare.Proof.by`'s bookkeeping) is the
  rest. Replay would still make each decision, so it could save at most
  part of the tactic share. The decision cost (§5) looks like the
  larger lever.
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
- CADP `Size2`/`Size3`: see §2.

## 5. Future work

Optimizations Jonah chose on 2026-10-11 to accept as current limits, and
one capability (§5.3, added 2026-10-12). None is a bug. Verdicts and
proofs are correct whenever they finish, and `Qed` checks every proof.
All are about cost. Like reformulating CADP to reach larger sizes, they
are orthogonal to the research question (what the plugin can decide and
prove), so a limit they cause is reported, not chased. §5.1 and §5.2 are
in priority order: the first touches every proof a user runs, the second
only very large FSMs. §5.3 is the one that would reach proofs the plugin
cannot produce today.

### 5.1 The solver's own per-step cost

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
`Size3`'s proof memory (§2), which is also unexplained.

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
(§4) would still make each decision, so this is the larger lever of the
two.

### 5.2 Memory of the saturation estimate

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
~100k or more silent SCCs needs gigabytes for it (§3, "Many silent SCCs").

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

### 5.3 Proofs split into separately checked lemmas

**Kind:** *capability* (large). A new proof back end, not a heuristic.

**Standing.** Not started; discussed 2026-10-12. Motivated by CADP
`Size2` glued ≈ layered (§2). It is decided in ~35s, but every proof
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

**Relation to other items.** It is complementary to §5.1, which makes
each step cheaper. It is independent of the "one state per silent SCC"
item set aside in §6: R relates states, not SCCs.

## 6. Set aside (deliberately)

Kept so they are not rediscovered as new. Each could be reopened, with the
reason in mind.

- **Proving one state per silent SCC** (abandoned 2026-10-03): unsound
  inside a coinduction, counterexample `CircularTransfer`.
- **`ReModel` unification instead of lookup** (A1): measured, cannot miss
  in current usage. Its value would be goals with an unknown state, closed
  as "give the witness by hand" (above).
- **Inversion tie-break options B and C**: closed unbuilt; residual
  sterile steps measured at zero after options A and D′.
- **Divergence-sensitive equivalences**: out of scope for the current
  definitions.
- **Module lists** (`_CoqProject`, `.mlpack`, dune) are still maintained
  by hand. A drift is caught by `scripts/check_module_lists.py` (CI)
  rather than prevented.
- **odoc warnings**: 72 remain, all `@raise` tags naming stdlib or Rocq
  exceptions, which odoc cannot resolve. Accepted.
