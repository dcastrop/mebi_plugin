# To-Do

## Features

- [ ] **Benchmarking Tools** -- using the ocaml `benchmark` package. 
  - [x] LTS Graph extraction
  - [ ] Algorithms
    - [ ] Saturation
    - [ ] Minimization
    - [ ] Bisimilarity
  - [ ] Proof Solving algorithm
- [ ] Implement Similarity algorithm (`lib/model/algorithms/similarity`)
- [ ] OCaml examples -- possibly aligned with json-dumped rocq-examples
- [ ] Plugin help commands

### Automatically Solve Proofs of Bisimilarity

- [x] Solve each direction bisimilarity in separate proofs for each direction
  - [x] `examples/Proc.v`
  - [ ] `examples/CADP.v`
    - [ ] *Size 1*
      - [x] Original vs Glued (`examples/CADP_Glued.v`)
      - [ ] Properties (E.g., mutual exclusion, no starvation -- ***see example in draft-paper***)
    - [ ] ~~***Size 2***~~ *(this may be infeasible -- state explosion)*
- [ ] Solve both directions in main bisimilarity proof

## Documenting (`odoc`)
- [ ] `lib/model/...`
  - [ ] `lib/model/`
  
## Optimizations & Fixes

- [x] ~~***Optimize Saturation algorithm***~~ (`lib/model/algorithms/saturation`) -- **resolved 2026-09-28.** The cause was not tuning but complexity class: `check_from` pruned only against `d.visited`, which `update_visited` copies rather than shares, so `States.fold (check_from d)` handed every sibling the same set. The traversal therefore enumerated simple *paths*, not states, and path count is exponential in state count on any cyclic branching graph. Measured with `test/satscale.ml` on a k x k silent grid (the shape parallel interleaving produces): 101 states took **436.72s**, against 0.005s for a silent chain of the same state count. Replaced with a breadth-first silent closure -- same specification, since `ActionPair.try_update` keeps `Annotation.shorter` and so only the shortest witness per destination ever survived the old enumeration anyway. 101 states now takes **0.0009s**, and 170 states (2.7M simple paths) 0.0025s. The rewrite also fixed a latent under-approximation: the old `visited` pruning discarded valid weak transitions whose witness revisits a state, 73 of them across 200 generated LTSs, with 0 lost and 0 annotations made longer. All 18 proof-suite counts unchanged. The `Traces`/`WIP` memo machinery (`lib/model/wip/`) existed only to curb the old re-exploration and is deleted. See `ASSISTED-CHANGES.md`, 2026-09-28.
- [ ] ***Fix duplicate unfolding tactics*** (`src/proof_solver`) -- mechanism for creating unfolding tactic appears to not check for duplicates.
- [x] ~~***Need an example that exercises the "multiple actionpairs" case in `Proof_solver_step.transition`***~~ (`src/proof_solver_step.ml`) -- **resolved 2026-10-01.** The 2026-09-27/28 investigations looked in the wrong place: the lookup reads FSM a's *unsaturated* edges (`Hyps.get_transition (W.get_fsm_a ())`, `saturated` defaults to `false`), where `EdgeMap.of_transitions` keeps one `Action.t` per derivation tree. So the branch fires whenever one strong step has two derivations -- saturation's `try_update` and the cross-FSM merge never come into it. `theories/Test.v`'s new `MultipleDerivations` module (two constructors coinciding on `tpar A A t -A-> tact A t`) hits it twice and proves; with the pre-fix raising behaviour patched back in, the same proof fails at that branch. Compiled on every `dune build`, so CI covers it. See `ASSISTED-CHANGES.md`, 2026-10-01.
- [x] ~~**Saturation silently drops destinations when a single action has more than one**~~ (`lib/model/algorithms/saturation.ml:376`, `edge_action_destinations`) -- **real correctness bug, found 2026-09-27, fixed same day.** `States.fold (fun y acc -> check_from d y ActionPairs.empty) ys ActionPairs.empty` explored every destination `y` with a *fresh* empty accumulator instead of threading the fold's own `acc`, so only the last-visited destination's results survived saturation -- every other destination was silently discarded, not merely deprioritized. Fixed to `States.fold (check_from d) ys ActionPairs.empty`, matching `check_destinations` three functions above, which already threads the accumulator correctly. Regression test added: `test_saturate_multi_destination_action` in `test/tests.ml` (confirmed to genuinely catch the bug by reverting the fix and rerunning). Full five-file `PluginProofs.v` baseline re-verified -- all 18 counts unchanged, as expected since the bug was inert for every existing example (Proc.v's structural-congruence rules are all deterministic, one destination per action). See `ASSISTED-CHANGES.md`'s 2026-09-27 "Fix: saturation dropped destinations" entry.

## Project Structure & Tooling

*Meta/structural -- none of these concern the plugin's behaviour. Noted while porting to `rocq 9.2` and pinning the toolchain.*

- [x] ~~**Add CI**~~ -- resolved, 2026-09-27: `.github/workflows/ci.yml` runs `nix develop` -> `opam switch create . --locked --deps-only` (cached on `rocq-mebi.opam.locked`'s hash) -> `dune build` -> `dune exec test/tests.exe` -> `make -j$(nproc)`, on push to `main` and on every PR.
- [ ] ***Move `paper/` out of this repository*** -- 73 tracked PDFs, ~36MB, is 96% of the repo (the pack is 38.8MiB; all of `lib/ src/ theories/ examples/ test/` together is ~1.2MB). Untouched for ~18 months. Note that deleting it from `HEAD` will ***not*** shrink anyone's clone -- that needs `git filter-repo` and a force-push, so it has to be coordinated with @dcastrop. There is also a licensing question in redistributing third-party papers from a public repo. A separate repo or a reference manager is the usual home for these.
- [ ] Rename `paper/references/to check/Affeldt.pdf` -- the filename contains `U+FB00` (the "ff" ligature, hence git quoting it as `A\357\254\200eldt.pdf`) and its directory name contains a space. Both are portability hazards on macOS (Unicode normalisation) and Windows.
- [ ] ***Confirm `MeBi Config Solver MutualCofix Auto` should stay the default*** -- needs @dcastrop's call, because it changes what the checked-in `MeBi Sim Solve` bounds mean. Since 2026-09-29 the solver can introduce its coinduction hypotheses two ways: a fresh nested `cofix` per newly-seen pair (what it always did), or one mutual `cofix` over the whole precomputed product relation. The nested one can only close a repeat that is an *ancestor* on the current branch, so it re-derives any pair reachable by a second route, which is why `Proc/Test3` never finished. `Auto` costs both on the model before the proof starts (`Model.Product.estimate`) and picks; it is correct on all 27 checked-in proofs and better than either fixed strategy (2565 iterations over the 18 cheap proofs, against 3794 always-nested and 2654 always-mutual, with no regression anywhere), and it prints a `Notice` whenever it takes the mutual path. The decisions worth a second opinion are: (i) that `Auto` rather than `Nested` is the right default for a released tool, given it can silently change a proof's iteration count; (ii) that `Proc/Test2`'s bounds are left deliberately loose -- they are the nested-path figures, so the file still compiles under `MutualCofix False` -- rather than tightened to the `Auto` counts; and (iii) whether `Proc/Test3/PluginProofs.v` should keep its explicit `MutualCofix True` now that `Auto` would choose the same thing. See `ASSISTED-CHANGES.md`, 2026-09-29, and backlog item B2.
- [ ] Add a `LICENSE` and uncomment `(license ...)` in `dune-project` -- currently commented out, so the generated `rocq-mebi.opam` carries no license field either. Needs @dcastrop's sign-off before picking one, since he owns the upstream repo.
- [ ] Reconcile the overlapping module lists -- `_CoqProject` (39 `.v` entries plus `-I` paths), the `(modules ...)` fields across `src/dune` and `lib/*/dune`, and `src/mebi_plugin.mlpack` for the make path. They can drift silently. The original example here (`src/mebi_plugin.mlpack` listing `Benchmarking` twice) is fixed, but the risk is not theoretical: a second instance turned up and was fixed during the same session -- `_CoqProject` still listed `lib/model/algorithms/similarity.{ml,mli}` after dune's `(modules ...)` had already dropped them (and the files were then deleted per the item below), which broke `make dune` until `_CoqProject` was corrected too.
- [x] ~~`tests.exe` is a no-op...~~ -- resolved: `test/tests.ml` is now a real 9-assertion suite exercising `lib/model` (`dune exec test/tests.exe`), and `test/dune` carries no `(public_name)`, so `opam install .` no longer installs a stray binary.
- [x] Removed the dead code kept in-tree under the leading-underscore convention -- `src/_command.{ml,mli}`, `src/_examples.{ml,mli}`, `src/_mebi_help.{ml,mli}` and `test/saturation.{ml,mli}` (none referenced by any `dune`/`.mlpack`/`_CoqProject` entry) are deleted; git history preserves them if ever needed. `examples/**/_*.v` individually checked, 2026-09-27: `_mutual_exclusion.v` was dead (superseded by `MutualExclusion.v` in the same reorg that underscore-prefixed it) and is deleted; `_no_starvation.v` and `_nat_streams.v` are real unfinished work, not draft filler, and are kept -- see `ASSISTED-CHANGES.md`.
- [ ] `lib/dune` is entirely commented out -- 9 of its 10 lines. Either finish it or delete it.
- [x] Clear stale detritus -- resolved, 2026-09-27: `.gitignore`'s stale `src/commandOLDunify.ml` line removed (the file is long gone). The other two turned out not to need action on re-check: `CoqMakeFile`/`CoqMakeFile.conf`/`.CoqMakeFile.d` are not actually present in the working tree (already gitignored, and apparently cleared by a later `make dune` run); `doc/index.html` redirecting into the gitignored `_build/default/_doc/_html/` is dune's standard `@doc`/odoc local-doc-viewer pattern, not a defect.
