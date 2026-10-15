# Limitations

What MeBi cannot do today, as it behaves now. None is a bug: a verdict
or proof the plugin gives is correct, and `Qed` checks every proof. Where
a limit is pinned, a test in `test/rocq/Test.v` fails on purpose (`KNOWN
LIMIT` / `KNOWN WRONG`), so lifting the limit turns the pin into a
positive test. What could be done about the cost limits is in
`future-work.md`.

Each is current behaviour. Where pinned, a test in `test/rocq/Test.v`
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
- **Size.** Extraction costs ~0.01-0.07MB per state. Its time per state
  grows with the number of alternatives in the source term, because it
  tries each by unification (`future-work.md`, "Extraction's derivation
  search is unindexed"); on the ABP-with-data series total time is
  roughly quadratic in the data domain. Whole saturation is
  refused above `Bounds Saturation` (default 1M weak actions;
  `Saturation_Too_Large`). Above that, `Run Bisim` and `Sim Begin`
  saturate on demand and decide on the silent-SCC quotient.
- **Many silent SCCs.** `Run Bisim` and `Sim Begin` need memory
  quadratic in an FSM's silent SCCs, whatever the saturation bound
  (~0.6GB at 50k SCCs, ~4GB at 128k; glued CADP `Size3` was OOM-killed
  at 9GB). `Run FSM` is not affected. Accepted as a limit for now
  (`future-work.md`, "Memory of the saturation estimate").

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
  are out of scope (`future-work.md`, "Set aside") unless @dcastrop
  wants them.

**Cost**
- **Proofs cost far more than deciding.** At width n = 3 (162 states a
  side), `Bisimilarity.fsm` takes 19ms and the proof 7 minutes (55k
  steps, 4.0GB). CADP `Size2` glued ≈ layered is decided in ~45s and
  ~1.5GB, but its proof runs out of memory at 10GB. `decide-vs-prove.md`
  has the full comparison.
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
