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
`Properties/Fifo.v`'s `spec_fifo`, with joining the queue visible
(`CADP_Obs.bigstep_obs`). It is proved for one and two processes (119
and 2,228 steps, over the normalised semantics, transferred by
`wbis_transfer_obs`) and decided for three (~4s). No starvation then
follows under a fair scheduler, which is stated, not proved.

**Still open:**
- **The layered semantics** (the paper's central CADP claim). Glued ≈
  layered is *proved* only at `Size1`. At `Size2` it is decided (~35s,
  bisimilar) but not proved (772, or 48 normalised, against 12,027
  states). No file yet states a transfer corollary ("the layered system
  satisfies X"). So FIFO and `Size2` mutual exclusion are, today, results
  about the glued model. To do: the same observing label in `step`, the
  `Size2` proof (an out-of-memory failure is a reportable limit, not a
  reason to reformulate the model), then the corollaries.
- **Restructure `examples/CADP`** (*decision*; discussed 2026-10-11, not
  decided). The tree is a size × semantics × property matrix, about 16
  directories of mostly one file each. One option is to organise it
  around the narrative: the model, its semantics and the specs in
  `examples/CADP/`; one file per claim (mutual exclusion, FIFO) with a
  section per size; separate files only for proofs too heavy to build
  by default. Constraints: heavy proofs need their own process and
  memory cap, and `_CoqProject` toggles builds per file.
- **`Size3` proofs.** Both `weak_bisimilar … spec_fifo` and mutual
  exclusion over `bigstep_norm` (644 states) run out of memory; see
  below.

**Size (measured 2026-10-11):**
- `Size2` (two processes): layered 12,027 states (~32s), glued 772.
  `Size2/Glued/MutualExclusion` is mutually similar to `spec_lts`, not
  bisimilar, and is now proved both ways (11,128 and 405 steps; the first
  ~10 min, ~4.4GB).
- `Size3` (three processes, where FIFO becomes interesting): glued
  128,044 states (extraction ~19 min, ~4.7GB), normalised
  (`CADP_Norm.bigstep_norm`) **644**. Through `bigstep_norm`, spec ≤
  system is proved (326 steps, ~7s). Mutual exclusion proper (system ≤
  spec) is not proved: its proof was OOM-killed at 8GB (~11 min) and,
  with `Answers Minimal`, at 9GB (~13 min). It is decided indirectly,
  through the FIFO bisimilarity above. Why a proof
  over 644 states needs so much more memory than `Size2`'s over 772
  (`bigstep`, 4.4GB) is unmeasured; one hypothesis is that each state
  carries the whole program text, so the proof term grows with term size
  as well as steps (*limit*/*optimization*, unprobed).
- `Run Bisim` on glued `Size3` against `spec_lts` was OOM-killed at 9GB
  twice, the second time before `Run FSM`'s extraction time had even
  passed. So `Run Bisim` seems to need far more memory than `Run FSM` for
  the same extraction (*bug?*, unprobed). `bigstep_norm` sidesteps it for
  CADP.
- Glued ≈ normalised is proved by hand for every size
  (`CADP_Norm.link`); the observing variant should be built over
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

**Goals and proofs**
- **Goal shapes** (*limit*, pinned `GoalShapes`). Only `weak_sim` and
  `weak_bisimilar` goals between closed terms are accepted.
  - **Open terms** are refused: a law for every `p` cannot be started.
  - **Local names** are not found, because `Begin` reads terms in the
    global environment.
  - **An `exists` goal** needs its witness given by hand (`exists t.`).

  Witness search and open terms would be new *capabilities*.
- **`Begin` does not check its LTSs against the goal's** (*bug?*;
  `Begin`'s terms are checked since PR #57). To probe: a goal over one LTS
  with `Begin` given another. If that ends in an internal error, it is a
  bug fix like #57's.
- **One state per silent SCC cannot be proved inside a coinduction**
  (*limit*; counterexample pinned as `CircularTransfer`). Very large
  systems (`Proc/Test4`, 9720 states) are therefore *decided* on demand
  but not *proved* directly. `Test4` is proved through an explicit
  structural congruence instead (`doc/structural-congruence.md`). That is
  why `Test4` has no `PluginProofs.v`.
- **Divergence.** `weak_sim` and `weak_bisimilar` ignore divergence, as
  their definitions do. Divergence-sensitive notions (as in `rocq-sims`)
  are out of scope (§5) unless @dcastrop wants them.

**Cost**
- **Proofs cost far more than deciding.** At width n = 3 (162 states a
  side), `Bisimilarity.fsm` takes 19ms and the proof 7 minutes (56k
  steps, 3.9GB).
- **Heavy proofs need a memory cap**, one per process. The ABP and
  `Test4` proofs peak at ~3.8-4.3GB; two such proofs in one file were
  OOM-killed at 6GB.
- **Deep derivations make `Qed` dominate.** At 64 layers, `Qed` takes
  71s against 7s of solving, and `Qed` grows ~13× per doubling of the
  layers.

## 4. Possible work

**Profiling and evaluation** (*evaluation*; detail in Jonah's notes 10
and 12)
- **Split the proof solver's own overhead from its tactics' time.** This
  decides the derivation-tree idea below. `perf` is installed but blocked
  for unprivileged users (`kernel.perf_event_paranoid` = 4); a package
  from nix or opam cannot change that. Either:
  - run `sudo sysctl kernel.perf_event_paranoid=1` once (it lasts until
    reboot), then `perf record --call-graph dwarf` (the switch has no
    frame pointers); or
  - add timers inside the plugin around the solver step and its tactic
    calls, which needs no privileges and measures exactly that split.
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
- **`Auto` mis-estimate** (*optimization*, small). On `LawProofs.tau1`,
  `Auto` picks nested (42 steps) where mutual takes 29. A data point for
  `Product.estimate`, if it is revisited.

**Reusable theory** (agreed 2026-10-11: machinery a user could reuse
goes into `theories/` once two examples use it, which keeps `examples/`
minimal)
- **The normalised-semantics transfer** (*refactor*, medium). An
  equivalence that steps preserve, a normalising function and an
  invariant together give `lts ≈ normalised lts`, with `weak_sim` and
  `weak_bisimilar` transfer theorems. `examples/ProcCongruence.v`,
  `CADP_Norm.v` and `CADP_Obs.v` each prove an instance of it by hand,
  and `CADP_Norm.link`/`CADP_Obs.link_obs` duplicate each other. After
  the lift, each keeps only its model-specific obligations.
- **Hiding (relabelling)** (*capability* of `theories/`, not of the
  plugin; small). A relabelling operator and the lemma that it preserves
  weak bisimilarity. For CADP it turns "plain `bigstep` is `bigstep_obs`
  with joining the queue hidden" into a theorem, which also gives
  `Size3` mutual exclusion for `bigstep`. CCS's ABP (hiding channels)
  could be a second user later.
- **Preservation of trace properties** (*capability* of `theories/`;
  small to medium). Weak bisimilarity preserves weak traces and what can
  happen next ignoring silent steps, but no lemma in `theories/` says so.
  These would state what "carries over" from glued to layered. Divergence
  ("eventually"), internal states and step counts do not carry over.

  Constraint on all three: the plugin cannot explore an LTS with
  parameters (§3), so `theories/` can hold the generic *theorems*, but
  each example still states its concrete relation (a couple of lines).

**Examples**
- `examples/Bisimilarity/_nat_streams.v`: unfinished, kept as real work,
  status unclear.
- CADP `Size2`/`Size3`: see §2.

## 5. Set aside (deliberately)

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
