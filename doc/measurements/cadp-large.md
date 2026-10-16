# The large CADP proofs, re-run on a bigger host: measurements

**First pass, written 2026-10-16 straight from the run output. Not yet
cross-checked against the raw files or the proof sources** (a second
commit does that and corrects this file).

Raw files: [`cadp-large-2026-10-15/`](cadp-large-2026-10-15/) (`bench/proofs.sh`
output per run: `solves.tsv`, `runs.tsv`, `logs/`; `journal-scopes.txt` is
the systemd journal's line for each run's memory-capped scope;
`huge-glued-boundsgame/rss-samples.txt` is that run's resident memory every
30s). The plan is `doc/future-work.md`, "Re-running the large CADP proofs".

## Setup

AMD Ryzen 7 5700X (8 cores, 16 threads), 31GB RAM, Linux 7.2.9-zen1, NixOS.
Rocq 9.2, OCaml 5.4.0. Plugin at `main` (`e593bf5`). One proof file per
process, run one at a time, each in a `systemd-run --user --scope` with
`MemoryMax=24G` and no swap. The `bench/lib.sh` there needed a local,
uncommitted edit to find GNU `time` (NixOS has no `/usr/bin/time`); nothing
else differed from `main`.

## Results

| run | policy | result | wall | peak |
|---|---|---|---|---|
| `MutualExclusionLarge.v` (5 proofs) | Auto | all Solved | 10.6 min | 4.6GB |
| `MutualExclusionLarge.v` (5 proofs) | Default | all Solved | 10.5 min | 4.4GB |
| `ThreeProcessesMutex.v` | Auto | Solved, `wsim_norm_3`, 20,432 steps | 22.8 min | 13.7GB |
| `ThreeProcessesMutex.v` | Default | Solved, 20,432 steps | 23.7 min | 13.7GB |
| `ThreeProcessesFifo.v` | Auto | Solved, `wbis_norm_3`, 45,773 steps | 37.6 min | 18.3GB |
| `ThreeProcessesFifo.v` | Default | Solved, 45,773 steps | 36.3 min | 18.3GB |
| `TwoProcessesGluedLayered.v`, nested cofix | Auto | **out of memory** at the 24G cap, no Solve | 2h05m | 24G |
| `TwoProcessesGluedLayered.v`, mutual cofix, `Bounds Game 100000000` | Auto | **out of memory** at the 24G cap, no Solve | 16h00m | 24G |

`MutualExclusionLarge.v`, steps per proof:

| proof | Auto | Default |
|---|---|---|
| `wsim_norm_2` | 1,038 | 1,038 |
| `wsim_norm_r_2` | 75 | 153 |
| `wsim_norm_r_3` | 124 | 326 |
| `wsim_bigstep_direct_2` | 11,128 | 11,128 |
| `wsim_spec_lts_direct_2` | 305 | 405 |

## Observations

- Auto saves steps on three of `MutualExclusionLarge.v`'s five proofs, with
  no change in status, time or memory (the 11,128-step proof dominates).
- Both three-process proofs fit on this host. Under `-a Default` they take
  the same steps, time and memory as under Auto, so `Answers Auto` made no
  difference to them: they fit because the host is bigger, not because of
  the policy. (The 14GB host ran out of memory at 8GB under Auto on mutex.)
- Glued ~ layered, nested: out of memory at 24G after 2h05m, where the
  14GB host ran out at 7-10GB. More memory did not change the outcome.
- Glued ~ layered with `Bounds Game`: `Sim Begin` took 4,489s (the game
  estimate), and Auto chose the mutual cofix over 78,202 pairs (312,808
  moves; the nested path would visit over 1.56 million goals). `Sim Solve`
  then ran about 14.7 hours. Memory went from 4GB to 17GB, stayed flat for
  about 3 hours, then climbed to 24.5GB and the run was killed. The ~2.2GB
  of the earlier, shorter run did not hold.

## Caveats

- The three-process proofs are of *normalised* statements (`wsim_norm_3`,
  `wbis_norm_3`). How these relate to the "not proved" lines in
  `OUTSTANDING.md` has not been checked.
- The glued runs have no peak in `runs.tsv` (`time` died with the process);
  their 24G is the journal's figure.
- The `Bounds Game` run's memory samples are in `rss-samples.txt`.
