# Outstanding

Everything still open about `mebi_plugin` in one place: decisions, known
limits, possible work, and what was set aside on purpose. This file should
**only shrink**. When an item is resolved:
- delete it here;
- record how it was resolved in `ASSISTED-CHANGES.md`.

Add an item only when something new is found, with a pointer to its
detail.

It supersedes the open-item lists in `TODO.md` ("To discuss with
@dcastrop", "Known limits and ideas"), `ASSISTED-CHANGES.md`
("Outstanding") and Jonah's local planning notes (`notes/`, not in the
repository). Those keep the history; this file holds what is still open.
Started 2026-10-10. Each item names its kind:
- *decision*: someone has to choose;
- *limit*: what the tool cannot do today;
- *bug?*: suspected, not yet confirmed;
- *capability*, *optimization*, *evaluation*: possible work, sized
  roughly;
- *set aside*: decided against, or out of scope, with the reason.

## 1. To discuss with @dcastrop

His repository and project, so his decisions. Detail for each is in
`TODO.md` (same headings) and the `ASSISTED-CHANGES.md` entries named.

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
- `MeBi Config Solver MutualCofix Auto` as the default (*decision*). It is
  correct on all checked-in proofs, and fewer steps than either fixed
  strategy, but it can silently change a proof's iteration count. Two
  sub-questions:
  - whether to tighten `Proc/Test2`'s deliberately loose bounds;
  - whether `Proc/Test3` should drop its explicit `MutualCofix True`.
- `MeBi Config Solver Answers Auto` as the default (*decision*). It is
  never slower than today's default on the 41 proofs, and has 60% fewer
  steps on `weak_bisimilar`; it was not measured with `MutualCofix`
  forced.

**Semantics** (Jonah's decisions of 2026-10-02, for review)
- Classical, divergence-insensitive weak bisimilarity is the target
  notion. A τ-loop and a stuck state are therefore equivalent (see §3).
- `weak_bisim` in `theories/Bisimilarity.v` is mutual similarity, which is
  strictly weaker than bisimilarity. `mutual_sim` (its honest name) and
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

**Kind:** example/property; possibly *limit*. Moved here from "the
authors' call" (2026-10-10) to be designed with Jonah.

**The property.** No process waiting to enter the critical section is
overtaken forever. `examples/Bisimilarity/CADP/Properties/
_no_starvation.v` quotes CADP's MCL formula (`[ true* ] forall i ... ]
-|`) and sketches a Rocq monitor (`NoStarvation.lts`). The monitor tracks,
per process, which others have acted since it last did. It allows a step
only while no process has been overtaken more than allowed, and only for
`ENTER`/`LEAVE` steps in the right order. The draft paper holds the
intended formulation.

**What the checked-in pieces can and cannot say:**
- `weak_sim` and `weak_bisimilar` are divergence-insensitive, and
  simulation preserves (weak) traces, not liveness. A true "eventually
  enters" cannot be stated as a simulation into a spec LTS without
  fairness.
- What a simulation *can* state is a **safety approximation**: bounded
  overtaking ("no process is bypassed more than k times while waiting").
  Under fair scheduling, that implies freedom from starvation. The draft
  monitor is shaped like this.
- So the likely form is `weak_sim system monitor`: every behaviour of the
  system is allowed by the monitor. Mutual exclusion is proved the same
  way, against `spec_lts`.

**Obstacles to settle first:**
- **Size.** CADP `Size1` composes **one** process (`composition_create
  0`), for which no-starvation is vacuous. It needs at least two, i.e.
  `Size2`, which is a known size limit: the plain LTS is still incomplete
  at 5000 states. The glued semantics extracts at `Size2`, but its
  `PluginProofs.v` fails and its `MutualExclusion` is marked "WIP, not
  bisim" in `_CoqProject`. Getting `Size2` (glued) to prove mutual
  exclusion first is probably the real first step.
- **Labels.** The monitor must see the system's labels (`act * pid`) and
  treat the rest as silent. Its premise `do_action a t1 = Some t2` is an
  equation computing the target, which MeBi supports.
- **Monitor state.** The history is lists of pids, so it is finite for a
  fixed N, but its size grows with N and k.
- **Waiting.** Which events mean "waiting" in this protocol: requesting
  the lock (`WRITE_NEXT`/`READ_LOCKED`), or only `ENTER`/`LEAVE`? This
  decides which labels stay visible.

**Inputs for the session:**
- the draft paper's formulation;
- `_no_starvation.v`;
- `Properties/MutualExclusion.v` (`spec_lts`) as the template;
- the `Size2` files.

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
  *capability* (medium); a sketch is in `TODO.md`.
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
  measure first). Today each constructor application is one solver step,
  so a layer adds 14 steps (`Layers.v`). It would only save solver
  overhead: the proof term, and so `Qed`, would be unchanged. At depth,
  `Qed` dominates (§3). Every checked-in bound would change. Detail in
  `TODO.md`.
- **`Auto` mis-estimate** (*optimization*, small). On `LawProofs.tau1`,
  `Auto` picks nested (42 steps) where mutual takes 29. A data point for
  `Product.estimate`, if it is revisited.

**Examples**
- `examples/Bisimilarity/_nat_streams.v`: unfinished, kept as real work,
  status unclear.
- CADP `Size2`: see §2.

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
- **Both directions of a bisimilarity in one command**: overtaken by
  `weak_bisimilar` (PR #3) and `MeBi Run Bisim ... As` (PR #34). *To
  confirm with Jonah* that nothing is left.
- **Module lists** (`_CoqProject`, `.mlpack`, dune) are still maintained
  by hand. A drift is caught by `scripts/check_module_lists.py` (CI)
  rather than prevented.
- **odoc warnings**: 72 remain, all `@raise` tags naming stdlib or Rocq
  exceptions, which odoc cannot resolve. Accepted.
