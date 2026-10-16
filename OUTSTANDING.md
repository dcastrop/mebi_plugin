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

- **Re-run the large CADP proofs on the 32GB host** (`doc/future-work.md`,
  "Re-running the large CADP proofs"). Three proofs ran out of memory at
  7-10GB under `Answers Default`; `bench/proofs.sh -s cadp-large` and
  `-s cadp-huge` (with `-M`) run them. Needs this on `origin/main`
  (PR #53).
- **An observing label in layered `step`** (a model change). It would
  carry FIFO order with joining *visible* to the layered CADP model. Today
  only the hidden form carries over, at one process (`examples/README.md`,
  "No starvation").
- **Whether a CADP/mCRL2 comparison is in scope** (`doc/future-work.md`,
  "Smaller possible work"). Neither tool is installed, and CADP needs a
  licence.
- **Whether `theories/Traces.v` may keep one example user.** The rule of
  2026-10-11 puts machinery in `theories/` once two examples use it, and
  only CADP does yet. Otherwise, it moves to `examples/CADP/`.

## CADP, the running example

- **Two processes, glued ≈ layered:** decided, not proved (*limit*). The
  measurements are in `examples/CADP/GluedLayered.v`. A way round is
  decision 3.
- **Three processes:** FIFO order, and mutual exclusion (system ≤ spec),
  are decided but not proved: out of memory at 8-9GB (*limit*;
  `examples/CADP/MutualExclusionLarge.v`, `examples/README.md`). Why they
  need so much more memory than two processes is unexplained
  (`doc/future-work.md`, "The solver's own per-step cost").

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
