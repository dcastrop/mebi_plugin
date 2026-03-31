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
