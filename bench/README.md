# bench/

Scripts for running the plugin's proof suites, and comparing the results
between two builds. They are tooling only: nothing here is part of the
plugin, and none of it changes the source tree. Run them with bash, from
anywhere.

All of them compile against the repository's **dune build**, so run
`dune build` first. Examples are compiled from a fresh copy for each run,
because the source tree may carry stale `.vo` files from an old `make`
build. Each proof file is its own process, capped with `systemd-run --user
--scope -p MemoryMax=...`. (`ulimit -v` does not work: OCaml 5 then cannot
reserve its heaps.)

| script | what it does |
|---|---|
| `proofs.sh` | runs a proof suite under one or more solver strategies, and writes one row per `MeBi Sim Solve` and one per file |
| `compare.sh` | diffs two `proofs.sh` runs: changed or missing Solves, plus time and memory side by side |
| `testv-counts.sh` | lists `theories/Test.v`'s solver counts, for diffing two trees |
| `lib.sh` | shared helpers, sourced by the others |

## Running the proof suites

```sh
bench/proofs.sh                         # default suite, Auto strategy
bench/proofs.sh -m auto,True,False      # ... and with MutualCofix forced
bench/proofs.sh -s abp                  # the two ABP proofs, one at a time
bench/proofs.sh -s test4                # Test4 via the normalised semantics
```

| suite | files | cost |
|---|---|---|
| `default` | the six `PluginProofs.v` marked `### Success` in `_CoqProject`, plus `CCS/PluginProofs.v` and `CCS/LawProofs.v` | ~3 min a mode, run 8 at a time (`-j`) |
| `abp` | `CCS/ABPProofs.v`, `CCS/ABPBisimProofs.v` | ~4 min and ~4.3GB each |
| `test4` | `Proc/Test4/NormProofs.v`, `NormBisimProofs.v` | ~7 + ~4 min, ~3.8GB peak |

`-m` takes a comma-separated list of the following:
- `auto` runs each file as written.
- `True` or `False` forces `MeBi Config Solver MutualCofix` for the whole
  file. The harness drops the copy's own setting and sets the mode right
  after it loads the plugin, so the plugin source is never patched.

Under `False` most files stop at their first `weak_bisimilar` proof; that
is expected (see `CLAUDE.md`).

Each run goes in `$MEBI_BENCH_DIR/<tag>`, or in a new temporary directory
if that variable is unset. The script prints the directory's path. It
holds:

- `solves.tsv`: one row per Solve, in file order (suite, mode, file, index,
  `Solved`/`Unsolved`, iterations);
- `runs.tsv`: one row per file (suite, mode, file, exit status, seconds,
  peak MB, error count);
- `logs/`: each file's full output.

The known-good counts are in `CLAUDE.md`.

## Comparing a branch against `main`

```sh
git worktree add ../mebi-main main && (cd ../mebi-main && dune build)
A=$(cd ../mebi-main && bench/proofs.sh -m auto,True,False -t main | tail -1)
B=$(bench/proofs.sh -m auto,True,False -t branch | tail -1)
bench/compare.sh "$A" "$B"
```

`compare.sh` exits 1 if any Solve differs.
- A change of status (Solved or Unsolved) is a regression.
- A change of count needs an explanation.

For `Test.v`:

```sh
diff <(bench/testv-counts.sh ../mebi-main) <(bench/testv-counts.sh)
```

## Not covered here

- Timing LTS extraction is a plugin command:
  `MeBi Benchmark LTS <min> <max> <term> Using <lts>`.
- Per-phase profiling: `perf` is not available everywhere. The
  measurements so far timed phases in a pure-OCaml replica of the model,
  which is not part of this folder.
