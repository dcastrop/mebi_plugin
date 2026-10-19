# Outstanding

The index of what is still open about `mebi_plugin`: one line per item,
saying who it waits on and where its detail is.
- **Detail** lives in `doc/` (or in the example's own files), never here.
- **History** lives in `ASSISTED-CHANGES.md`.

This file should **only shrink**. When an item is resolved, delete its
line, and record the resolution in `ASSISTED-CHANGES.md`. When something
new is found, add one line, and put its detail in the right `doc/` file.

(Until 2026-10-13 this file held the detail as well, and had grown to
438 lines: `git show 6b9b38f:OUTSTANDING.md`. It replaced `TODO.md`,
deleted 2026-10-10: `git show cbbdd39:TODO.md`.)

## Waiting on @dcastrop

One brief each, in [`doc/decisions.md`](doc/decisions.md), in this
order:
1. How the fork's work reaches his repository: one PR or a series.
2. Whether `MeBi Config Solver MutualCofix Auto` stays the default.
3. Whether to build proofs split into separately checked lemmas.
4. For review: choices made while he was away (the target notion,
   `weak_bisim` removed, `Sim Begin` on similar states, provenance,
   `Answers Auto` as the default, the licence split).

Decision 2 would change every checked-in proof bound if it went against
`Auto`, so it comes before the upstream PR.

## Waiting on Jonah

- **An observing label in layered `step`** (a model change). It would
  carry FIFO order with joining *visible* to the layered CADP model. Today
  only the hidden form carries over, at one process (`examples/README.md`,
  "No starvation").
- **How to compact the `Result` output** (`doc/future-work.md`,
  "Compact output").
- **Whether a CADP/mCRL2 comparison is in scope** (`doc/future-work.md`,
  "Smaller possible work"). Neither tool is installed, and CADP needs a
  licence.
- **Whether `theories/Traces.v` may keep one example user.** The rule of
  2026-10-11 puts machinery in `theories/` once two examples use it, and
  only CADP does yet. Otherwise, it moves to `examples/CADP/`.

## CADP, the running example

- **Two processes, glued ≈ layered:** decided, not proved (*limit*): out
  of memory at 24GiB on a 31GB host (nested, and mutual under `Auto`), and
  unfinished after 24h under `Default` at 5GiB. The
  measurements are in `doc/measurements/cadp-large.md`; a way round is
  decision 3. A longer `Default` run is not done (`doc/future-work.md`, "Re-running
  the large CADP proofs").
- **Three processes:** FIFO order, and mutual exclusion (system ≤ spec),
  are proved over the normalised semantics, but only on a 31GB host (13.4
  and 17.8GiB peak; `examples/CADP/ThreeProcesses*.v`, not built by
  default). Why 644 states need so much more memory than 48 is
  unexplained (`doc/future-work.md`, "The solver's own per-step cost").
  The FIFO one is not carried back to `bigstep_obs`.

## Bugs

Found through external feedback; detail in
[`doc/feedback-2026-10-15.md`](doc/feedback-2026-10-15.md).
- **Extraction runs away on a step rule whose premise is the relation
  itself** (on a congruent copy of the source): no bound stops it, not
  even `Bounds Memory` (item 1).
- **A proof through a congruence premise crashes** ("Tried to normalize
  ill-typed term"; known-wrong pin `ExternalFeedback`). Possibly the
  feedback's proof that never closed (item 4).
- **A warning blames the target for an unknown in the label** (item 3;
  small).

## Limits of the tool

Detail, with the pinning tests, is in
[`doc/limitations.md`](doc/limitations.md). Accepted as they stand
unless noted.
- **Extraction:**
  - undecidable premises are assumed true, with a warning;
  - LTS premises with an undetermined source find only some steps;
  - collapsing definitions are refused;
  - LTSs with parameters are refused (lifting this would be a
    *capability*);
  - a premise that matches on an argument nothing fixes is undecided
    (enumerating it would be a *capability*);
  - size;
  - memory quadratic in the number of silent SCCs.
- **Goals and proofs:**
  - goal shapes: closed `weak_sim`/`weak_bisimilar` only (open terms or
    witness search would be *capabilities*);
  - one state per silent SCC cannot be proved;
  - divergence is ignored.
- **Cost:**
  - proofs cost far more than decisions (`doc/decide-vs-prove.md`);
  - `Auto` goes nested on an FSM saturated on demand unless
    `Bounds Game` is set (an open *optimization* question);
  - heavy proofs need a memory cap (`Bounds Memory` is a soft one);
  - `Qed` dominates on deep derivations.

## Future work

Detail is in [`doc/future-work.md`](doc/future-work.md). None is under
way.
- **Cost, accepted as limits:**
  - the solver's own per-step cost;
  - memory of the saturation estimate;
  - extraction's unindexed derivation search.
- **Capability:** proofs split into separately checked lemmas
  (decision 3).
- **Smaller:**
  - replay a derivation tree in one solver step;
  - `Auto`'s mis-estimate on `LawProofs.tau1`;
  - scale the evaluation levers further;
  - the CADP/mCRL2 comparison (see above);
  - `examples/Bisimilarity/_nat_streams.v`, unfinished.
- **Set aside on purpose**, so they are not rediscovered as new: listed
  at the end of `doc/future-work.md`.
