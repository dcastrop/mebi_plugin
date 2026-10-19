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
| `testv-counts.sh` | lists `test/rocq/Test.v`'s solver counts, for diffing two trees |
| `lib.sh` | shared helpers, sourced by the others |
| `ocaml/` | pure-OCaml benchmarks of the model's algorithms, on FSMs extracted from the examples and on generated families: see [`ocaml/README.md`](ocaml/README.md) |

## Requirements

`proofs.sh` needs `systemd-run` and **GNU `time` at `/usr/bin/time`**
(`lib.sh`, `bench_capped`, calls it with `-f` to get each run's peak memory
and seconds). Where `/usr/bin/time` is missing, such as NixOS or a minimal
Debian or Ubuntu image, every run fails at once with `Failed to find
executable /usr/bin/time` in its log, `exit 1` and 0 errors in `runs.tsv`.
That looks like a result and is not one.

To fix it on a host where it matters:
- Debian or Ubuntu: `apt install time`.
- Nix: run under `nix-shell -p time --run 'bench/proofs.sh ...'` and, in a
  local copy of `lib.sh`, change `/usr/bin/time` to `"$(type -P time)"`
  (`type -P` finds the program, not bash's `time` keyword). This is not
  committed: hosts that have `/usr/bin/time` need no change, and a lookup
  that falls back to `PATH` has not been tried on them.
- Any other host: install GNU `time` (not BSD or busybox: they lack `-f`)
  and edit the same line.

## Running the proof suites

```sh
bench/proofs.sh                         # default suite, Auto strategy
bench/proofs.sh -m auto,True,False      # ... and with MutualCofix forced
bench/proofs.sh -s abp                  # the two ABP proofs, one at a time
bench/proofs.sh -s test4                # Test4 via the normalised semantics
bench/proofs.sh -s eval                 # the evaluation levers, as built by default
bench/proofs.sh -s eval-large -f WidthLarge.v   # one file of a suite
bench/proofs.sh -s cadp-large -M 24G    # MutualExclusionLarge.v, with a higher cap
bench/proofs.sh -s cadp-large -a Default -t old   # force an answer policy, to compare
```

| suite | files | cost |
|---|---|---|
| `default` | the six `PluginProofs.v` marked `### Success` in `_CoqProject`, plus `CCS/PluginProofs.v` and `CCS/LawProofs.v` | ~3 min a mode, run 8 at a time (`-j`) |
| `abp` | `CCS/ABPProofs.v`, `CCS/ABPBisimProofs.v` | ~4 min and ~4.3GB each |
| `test4` | `Proc/Test4/NormProofs.v`, `NormBisimProofs.v` | ~0.5 + ~4 min, ~2.8GB peak |
| `eval` | `examples/Evaluation/{Width,Layers,Depth}.v` | ~40s |
| `eval-large` | `examples/Evaluation/{Width,Layers,Depth}Large.v`, the same levers scaled up | ~7 + ~1.5 + ~1 min, ~4GB peak |
| `cadp-large` | `examples/CADP/MutualExclusionLarge.v` | ~10 min, ~4.5GiB (two processes direct), under `Answers Auto` and `Default` alike |
| `cadp-huge` | `examples/CADP/{ThreeProcessesMutex,ThreeProcessesFifo,TwoProcessesGluedLayered}.v`, the three proofs that ran out of memory at 7-10GB on a 14GB host, one per file | on a 31GB host: mutex ~23 min, 13.4GiB; fifo ~37 min, 17.8GiB; glued ≈ layered out of memory at 24GiB after 2-16h (`doc/measurements/cadp-large.md`). Give `-M` and run one at a time |

`-m` takes a comma-separated list of the following:
- `auto` runs each file as written.
- `True` or `False` forces `MeBi Config Solver MutualCofix` for the whole
  file. The harness drops the copy's own setting and sets the mode right
  after it loads the plugin, so the plugin source is never patched.

Under `False` most files stop at their first `weak_bisimilar` proof; that
is expected (see `CLAUDE.md`).

Two more options, for a machine unlike the one the suites were sized for:
- `-M <mem>` replaces each file's memory cap (`systemd-run`'s form, such as
  `28G`). The suites' caps are 5-8GB.
- `-a <policy>` starts every file under `MeBi Config Solver Answers
  <policy>` (`Default`, `Greedy`, `Minimal` or `Auto`), to measure one policy
  against another on the same machine. A file's checked-in bounds are for the
  plugin's default, so a policy that needs more steps stops at its first
  `Solve`; that is the answer, not a fault.

Each run goes in `$MEBI_BENCH_DIR/<tag>`, or in a new temporary directory
if that variable is unset. The script prints the directory's path. It
holds:

- `solves.tsv`: one row per Solve, in file order (suite, mode, file, index,
  `Solved`/`Unsolved`, iterations), then the proof's name and the seconds
  its `MeBi Sim Begin`, `MeBi Sim Solve` and `Qed` took, from `rocq compile
  -time` (`?` where one did not finish). An evaluation file holds one size
  per proof, so these columns give a series' times;
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
- Per-phase profiling inside the plugin: `perf` is not available
  everywhere. `ocaml/` times the model's algorithms without Rocq; the
  proof solver's own overhead is not split from its tactics' time.
