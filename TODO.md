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

- [ ] ***Optimize Saturation algorithm*** (`lib/model/algorithms/saturate`) -- takes a long time on larger/multi-layered examples. We use traces to ensure we don't keep re-exploring the same path, but I think we need to go a step further and keep exploring until we have saturated each trace before continuing. ***To be Revisited***
- [ ] ***Fix duplicate unfolding tactics*** (`src/proof_solver`) -- mechanism for creating unfolding tactic appears to not check for duplicates.
- [ ] ***Need an example that exercises the "multiple actionpairs" case in `Proof_solver_step.transition`*** (`src/proof_solver_step.ml`) -- fixed 2026-09-27 to pick the shortest-annotation candidate (via `Model.Action.Pair.shorter_annotation`) instead of raising when more than one distinct `Action.t` matches the same `(from, label, goto)`, mirroring `try_get_visible_transition`'s existing tie-break. None of the five cheap `PluginProofs.v` baseline examples (`Proc/Test1`, `Proc/Test2`, `CADP/Size1/{MutualExclusion,Glued,Glued/MutualExclusion}`) actually hit this branch, so the fix is confirmed not to regress anything but has never been positively exercised. Needs either a small hand-built LTS where weak-transition saturation produces two distinct witnesses for the same visible transition, or confirmation that one of the more expensive examples (`Proc/Test3`/`Test4`, `CADP/Size2`) already hits it.

## Project Structure & Tooling

*Meta/structural -- none of these concern the plugin's behaviour. Noted while porting to `rocq 9.2` and pinning the toolchain.*

- [x] ~~**Add CI**~~ -- resolved, 2026-09-27: `.github/workflows/ci.yml` runs `nix develop` -> `opam switch create . --locked --deps-only` (cached on `rocq-mebi.opam.locked`'s hash) -> `dune build` -> `dune exec test/tests.exe` -> `make -j$(nproc)`, on push to `main` and on every PR.
- [ ] ***Move `paper/` out of this repository*** -- 73 tracked PDFs, ~36MB, is 96% of the repo (the pack is 38.8MiB; all of `lib/ src/ theories/ examples/ test/` together is ~1.2MB). Untouched for ~18 months. Note that deleting it from `HEAD` will ***not*** shrink anyone's clone -- that needs `git filter-repo` and a force-push, so it has to be coordinated with @dcastrop. There is also a licensing question in redistributing third-party papers from a public repo. A separate repo or a reference manager is the usual home for these.
- [ ] Rename `paper/references/to check/Affeldt.pdf` -- the filename contains `U+FB00` (the "ff" ligature, hence git quoting it as `A\357\254\200eldt.pdf`) and its directory name contains a space. Both are portability hazards on macOS (Unicode normalisation) and Windows.
- [ ] Add a `LICENSE` and uncomment `(license ...)` in `dune-project` -- currently commented out, so the generated `rocq-mebi.opam` carries no license field either. Needs @dcastrop's sign-off before picking one, since he owns the upstream repo.
- [ ] Reconcile the overlapping module lists -- `_CoqProject` (39 `.v` entries plus `-I` paths), the `(modules ...)` fields across `src/dune` and `lib/*/dune`, and `src/mebi_plugin.mlpack` for the make path. They can drift silently. The original example here (`src/mebi_plugin.mlpack` listing `Benchmarking` twice) is fixed, but the risk is not theoretical: a second instance turned up and was fixed during the same session -- `_CoqProject` still listed `lib/model/algorithms/similarity.{ml,mli}` after dune's `(modules ...)` had already dropped them (and the files were then deleted per the item below), which broke `make dune` until `_CoqProject` was corrected too.
- [x] ~~`tests.exe` is a no-op...~~ -- resolved: `test/tests.ml` is now a real 9-assertion suite exercising `lib/model` (`dune exec test/tests.exe`), and `test/dune` carries no `(public_name)`, so `opam install .` no longer installs a stray binary.
- [x] Removed the dead code kept in-tree under the leading-underscore convention -- `src/_command.{ml,mli}`, `src/_examples.{ml,mli}`, `src/_mebi_help.{ml,mli}` and `test/saturation.{ml,mli}` (none referenced by any `dune`/`.mlpack`/`_CoqProject` entry) are deleted; git history preserves them if ever needed. `examples/**/_*.v` individually checked, 2026-09-27: `_mutual_exclusion.v` was dead (superseded by `MutualExclusion.v` in the same reorg that underscore-prefixed it) and is deleted; `_no_starvation.v` and `_nat_streams.v` are real unfinished work, not draft filler, and are kept -- see `ASSISTED-CHANGES.md`.
- [ ] `lib/dune` is entirely commented out -- 9 of its 10 lines. Either finish it or delete it.
- [x] Clear stale detritus -- resolved, 2026-09-27: `.gitignore`'s stale `src/commandOLDunify.ml` line removed (the file is long gone). The other two turned out not to need action on re-check: `CoqMakeFile`/`CoqMakeFile.conf`/`.CoqMakeFile.d` are not actually present in the working tree (already gitignored, and apparently cleared by a later `make dune` run); `doc/index.html` redirecting into the gitignored `_build/default/_doc/_html/` is dune's standard `@doc`/odoc local-doc-viewer pattern, not a defect.
