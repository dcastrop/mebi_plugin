# bench/symbolic/

Phase S0 of [`SYMBOLIC_PLAN.md`](../../SYMBOLIC_PLAN.md): a standalone
prototype of the symbolic route. It is not part of the plugin: nothing here
reads Rocq, and nothing is installed. Like `test/tests.exe`, it links
`rocq-mebi.model` only, which it uses as the explicit oracle.

## What it does

1. **Builds each system as a network.** A global state is one local state per
   component. A global step is a *group*: some components move together,
   under one label ([`network.ml`](network.ml)). This is the plan's rule IR in
   miniature. Two front ends copy the examples' semantics into OCaml,
   already normalised (no structural congruence, a fixed parallel vector):
   - `Proc`'s layered `compLTS`;
   - CCS with a top-level restriction.
2. **Encodes it in BDDs** ([`symbolic.ml`](symbolic.ml)):
   - each component's local state takes `ceil(log2 size)` bits;
   - each bit has a current and a next variable, adjacent;
   - two variable orders: `zip` (component `i` of each side together) and
     `blocks` (all of one side, then the other).
3. **Decides weak bisimilarity as a greatest fixpoint over pairs.** The
   condition is the one in `theories/Bisimilarity.v`: a strong move
   answered by a weak one. Only preimages are taken: the weak answer
   into `Z` is `pre_τ*(pre_a(pre_τ*(Z)))`, so τ\* is never built as a
   relation.
4. **Checks against the plugin's model.** With `--oracle N`, every system
   with at most `N` states a side is also expanded explicitly and checked
   by `Model.Bisimilarity.fsm`, as `MeBi Run Bisim` calls it. The binary
   exits 1 if verdicts or state counts disagree.

BDDs are CUDD, through opam's `mlcuddidl` (`opam install mlcuddidl`),
with dynamic reordering off ([`bdd_cudd.ml`](bdd_cudd.ml), behind the
signature in [`bdd.ml`](bdd.ml)). The executable is built only where
`mlcuddidl` is installed (dune's `optional`).

## Systems

| family | what | expected |
|---|---|---|
| `proc` | Test4 generalised: `n` cyclic components; `p` against `p` rotated by one. At `n = 4`, exactly Test4's `p` and `q`, normalised. 3^n states a side | bisimilar |
| `proc-bad` | the same, with `q`'s last component on label `C` | not |
| `buffers` | `n` one-place buffers chained, inner channels restricted (2^n states), against an `n`-place counter | bisimilar |
| `buffers-bad` | the same against an `(n-1)`-place counter | not |
| `abp` | `examples/CCS.v`'s `abp` against `def 20`, definition by definition | bisimilar |

## Running

**Always under a memory cap** (`systemd-run`; `timeout` is not one). Size
the cap well below the machine's available memory:

```sh
dune build ./bench/symbolic
systemd-run --user --scope -q -p MemoryMax=2G -p MemorySwapMax=0 \
  ./_build/default/bench/symbolic/main.exe --family buffers --n 8,16 --order zip,blocks --oracle 3000
```

Each row gives:
- the verdict, and the expected one;
- reachable states on each side, and the pairs in the bisimulation `R`;
- `R`'s BDD nodes, and the manager's peak nodes;
- the fixpoint's iterations, and the seconds taken;
- the explicit oracle's result, when it ran.

Measurements: [`doc/measurements/symbolic-s0.md`](../../doc/measurements/symbolic-s0.md).
