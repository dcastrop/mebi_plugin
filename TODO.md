# To-Do

## Documenting (`odoc`)
- [ ] `lib/model/...`
  - [ ] `lib/model/`

## Automatically Solve Proofs of Bisimilarity

- [x] Solve each direction bisimilarity in separate proofs for each direction
  - [x] `examples/Proc.v`
  - [ ] `examples/CADP.v`
    - [ ] *Size 1*
      - [x] Original vs Glued (`examples/CADP_Glued.v`)
      - [ ] Properties (E.g., mutual exclusion, no starvation -- ***see example in draft-paper***)
    - [ ] ~~***Size 2***~~ *(this may be infeasible -- state explosion)*
- [ ] Solve both directions in main bisimilarity proof


## Features

- [x] Saturation, Minimization, Bisimimilarity
- [ ] Implement Similarity algorithm (`lib/model/algorithms/similarity`)
- [ ] Plugin help commands
- [ ] **Benchmarking Tools** -- 
- [x] JSON File Dumps (configurable)
- [ ] OCaml examples -- possibly aligned with json-dumped rocq-examples


## Optimizations & Fixes

- [ ] ***Optimize Saturation algorithm*** (`lib/model/algorithms/saturate`) -- takes a long time on larger/multi-layered examples. We use traces to ensure we don't keep re-exploring the same path, but I think we need to go a step further and keep exploring until we have saturated each trace before continuing.
- [ ] ***Fix duplicate unfolding tactics*** (`src/proof_solver`) -- mechanism for creating unfolding tactic appears to not check for duplicates.


## Reorganisation

Keep `src/` clean and plugin-focused for a more maintainable codebase.

- [x] ***Standardise output*** -- Now use `lib/utils/json` for anything we expect to need to printout, pretty-print to `json` (using package `yojson`). 
- [x] ***Configurable output*** -- Output is handled by `lib/utils/logger`, but needs to be user-configurable (via `src/api`)
- [x] ***Encoding*** -- Split components into separate files, move to `lib/model/`
- [x] ***Models*** 
  - [x] Split components into separate files, move to `lib/model/`. *(Now supports usecases beyond this plugin by requiring `lib/terms/base_term` rather than `lib/terms/encoding`.)*
  - [x] Clean-up code with signatures
