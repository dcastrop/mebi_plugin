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

## Project Structure & Tooling

*Meta/structural -- none of these concern the plugin's behaviour. Noted while porting to `rocq 9.2` and pinning the toolchain.*

- [ ] ***Add CI*** -- the `rocq 9.2` port was only discovered months late, by trying to build. A minimal GitHub Actions job (`nix develop` -> `opam switch create . --locked --deps-only` -> `dune build`) would have caught it the week it landed. `rocq-mebi.opam.locked` is what makes such a job cheap and deterministic; without CI most of its value goes unused.
- [ ] ***Move `paper/` out of this repository*** -- 73 tracked PDFs, ~36MB, is 96% of the repo (the pack is 38.8MiB; all of `lib/ src/ theories/ examples/ test/` together is ~1.2MB). Untouched for ~18 months. Note that deleting it from `HEAD` will ***not*** shrink anyone's clone -- that needs `git filter-repo` and a force-push, so it has to be coordinated with @dcastrop. There is also a licensing question in redistributing third-party papers from a public repo. A separate repo or a reference manager is the usual home for these.
- [ ] Rename `paper/references/to check/Affeldt.pdf` -- the filename contains `U+FB00` (the "ff" ligature, hence git quoting it as `A\357\254\200eldt.pdf`) and its directory name contains a space. Both are portability hazards on macOS (Unicode normalisation) and Windows.
- [ ] Add a `LICENSE` and uncomment `(license ...)` in `dune-project` -- currently commented out, so the generated `rocq-mebi.opam` carries no license field either.
- [ ] Reconcile the overlapping module lists -- `_CoqProject` (39 `.v` entries plus `-I` paths), the `(modules ...)` fields across `src/dune` and `lib/*/dune`, and `src/mebi_plugin.mlpack` for the make path. They can drift silently, and one already has: `src/mebi_plugin.mlpack` lists `Benchmarking` twice.
- [ ] `tests.exe` is a no-op that `opam install .` would install -- `test/tests.ml` is commented out end to end, yet `test/dune` gives it `(public_name rocq-mebi.tests)`, so installing the package drops a do-nothing binary into the switch's `bin/`.
- [ ] Decide what to do with the dead code kept in-tree under the leading-underscore convention -- `src/_command.{ml,mli}`, `src/_examples.{ml,mli}`, `src/_mebi_help.{ml,mli}` (none referenced by any `dune` or `.mlpack` file), plus `test/saturation.{ml,mli}` and `examples/**/_*.v`. The convention is fine; the point is that git history already preserves them.
- [ ] `lib/dune` is entirely commented out -- 9 of its 10 lines. Either finish it or delete it.
- [ ] Clear stale detritus -- `.gitignore` still lists `src/commandOLDunify.ml`, which no longer exists; `CoqMakeFile`, `CoqMakeFile.conf` and `.CoqMakeFile.d` linger in the working tree from the pre-Rocq `coq_makefile` era; `doc/index.html` is a redirect stub pointing into the gitignored `_build/`, so it is a tracked file whose target is never tracked.
