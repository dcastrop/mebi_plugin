# bench/ocaml/

Pure-OCaml benchmarks of the model's algorithms (saturation, minimization
and bisimilarity), timed with the [`benchmark`] library. Like
`test/tests.exe`, the binary links `rocq-mebi.model` only, without Rocq.
That makes it the place to time the model apart from extraction, the proof
solver and the kernel, which `bench/proofs.sh` measures.

[`benchmark`]: https://github.com/Chris00/ocaml-benchmark

```sh
dune exec bench/ocaml/mebi_bench.exe                  # every input, every algorithm
dune exec bench/ocaml/mebi_bench.exe -- --check       # verdicts and sizes only (CI runs this)
dune exec bench/ocaml/mebi_bench.exe -- --only width --width 3,4,5 --algos bisim
dune exec bench/ocaml/mebi_bench.exe -- --help
```

It prints one tab-separated row per input, side and algorithm: the side's
states and edges, then the number of runs and the CPU milliseconds per run
(mean and least over `--repeat` timings of `--seconds` each). `saturate`
and `minimize` time each side of a pair; `bisim` times the pair
(`Bisimilarity.fsm`, as `MeBi Run Bisim` calls it). Before timing, every
pair's verdict is checked against the expected one; the binary exits 1 if
any differs.

## Inputs

**Fixtures** (`fixtures/`, ~200KB): FSMs the plugin extracted from the Rocq
examples, one file per pair of systems. They come from the
`PluginProofs.v` files of `Proc/Test1-3`, the three CADP `Size1` pairs,
`CCS/PluginProofs.v` (with the Alternating Bit Protocol) and
`CCS/LawProofs.v`, and the `examples/Evaluation` levers. Each records its
source and the command that made it, and the plugin's verdict on it, which
is the one checked. Three CCS pairs are similar but not bisimilar.
`Proc/Test4` is not included: its 9720 states would make a large fixture.

**Generated families** (`families.ml`): the `examples/Evaluation` levers
rebuilt in OCaml with the same semantics, so they scale past what the plugin
extracts in reasonable time:
- `width-N` is `Width.v`'s `wl N` against `wr N`, with 2·3^(N+1) states a
  side and no silent steps, so `saturate` is the identity (as in the
  plugin);
- `depth-K` is `Depth.v`'s `tfix X` against `tfix (tfix X)`, with 2K+1
  states and a silent collapse.

Where a family and a fixture share a size (width 0-2, depth 1-8), states
and edges agree exactly (`--check` prints both). That is the check that
the two are the same systems.

## Regenerating the fixtures

```sh
dune build && bench/ocaml/make-fixtures.sh
```

The script compiles a copy of each source against the dune build. In the
copy, every proof that runs `MeBi Sim Begin` is admitted, and the first
time each pair is met, a `MeBi Run Bisim` on it takes the proof's place,
with dumps on. The command thus runs with the settings and scope the proof
had. `fixtures.py` then converts the dumps into fixtures. The whole run
takes under a minute. Regenerate after a change to extraction or to the
examples, and check the diff: a changed fixture is a changed LTS.

## Not covered

- The proof solver needs Rocq, so `bench/proofs.sh` times it (per
  `Begin`, `Solve` and `Qed` in `solves.tsv`).
- LTS extraction needs Rocq too: `MeBi Benchmark LTS`.
