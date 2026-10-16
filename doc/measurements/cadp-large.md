# The large CADP proofs, re-run on a bigger host: measurements

Recorded 2026-10-16. Every figure below was checked against the raw files
in [`cadp-large-2026-10-15/`](cadp-large-2026-10-15/) (the first pass,
`ea452ed`, was written from the run output; this version corrects it, see
"Corrections").

Raw files, one directory per run: `solves.tsv`, `runs.tsv` and `logs/` from
`bench/proofs.sh`. `journal-scopes.txt` is the systemd journal's line for
each run's memory-capped scope (the cgroup's peak, and the OOM kills).
`huge-glued-boundsgame/rss-samples.txt` is that run's resident memory
(MiB) every 30s. The plan is `doc/future-work.md`, "Re-running the large
CADP proofs".

## Setup

AMD Ryzen 7 5700X (8 cores, 16 threads), 31GB RAM, Linux 7.2.9-zen1,
NixOS. Rocq 9.2, OCaml 5.4.0. Plugin at `main` (`e593bf5`). One proof file
per process, run strictly one at a time (the host was otherwise idle
apart from desktop use). Each run is in a
`systemd-run --user --scope` with `MemoryMax=24G` (24 GiB) and no swap. The
only difference from `main` was a local, uncommitted edit of
`bench/lib.sh` to find GNU `time` (NixOS has no `/usr/bin/time`).

**Units.** Memory is in GiB (1024³ bytes), as systemd's `24G` is.
`runs.tsv`'s `peak_mb` is MiB: the largest resident size of the process
(`time`'s `%M`). The journal's "memory peak" is the whole scope's, so it is
a little higher or lower. Both are given where they differ.

## Results

| run | policy | result | wall | peak (`runs.tsv`; journal) |
|---|---|---|---|---|
| `MutualExclusionLarge.v` (5 proofs) | Auto | all Solved | 10.6 min | 4.5 GiB; 4.4 |
| `MutualExclusionLarge.v` (5 proofs) | Default | all Solved | 10.5 min | 4.3 GiB; 4.2 |
| `ThreeProcessesMutex.v` | Auto | Solved, 20,432 steps | 22.8 min | 13.4 GiB; 13.3 |
| `ThreeProcessesMutex.v` | Default | Solved, 20,432 steps | 23.7 min | 13.4 GiB; 13.3 |
| `ThreeProcessesFifo.v` | Auto | Solved, 45,773 steps | 37.6 min | 17.8 GiB; 17.8 |
| `ThreeProcessesFifo.v` | Default | Solved, 45,773 steps | 36.3 min | 17.9 GiB; 17.9 |
| `TwoProcessesGluedLayered.v`, nested cofix | Auto | **out of memory** in `Solve` | 2h05m | 24 GiB (cap); 24 |
| `TwoProcessesGluedLayered.v`, mutual cofix, `Bounds Game 100000000` | Auto | **out of memory** in `Solve` | 16h00m | 24 GiB (cap); 24 |

`Default` is `-a Default` to `bench/proofs.sh` (`MeBi Config Solver Answers
Default`), the policy before 2026-10-15; `Auto` is the plugin's default.
All six finished runs exited 0 with 0 errors, and each proof's `Qed` is in
its log. The two glued runs have no peak in `runs.tsv`: `time` was killed
with the process, so the journal is the only source.

### `MutualExclusionLarge.v`, steps per proof

| proof | Auto | Default |
|---|---|---|
| `wsim_norm_2` | 1,038 | 1,038 |
| `wsim_norm_r_2` | 75 | 153 |
| `wsim_norm_r_3` | 124 | 326 |
| `wsim_bigstep_direct_2` | 11,128 | 11,128 |
| `wsim_spec_lts_direct_2` | 305 | 405 |

Counts are `Solved after N iterations`. Total time and memory are the same
for both policies: the 11,128-step proof is ~560s of the ~630s.

### The three-process proofs

| | Auto | Default |
|---|---|---|
| mutex `wsim_norm_3`: `Solve` / `Qed` | 1,205s / 157s | 1,260s / 157s |
| fifo `wbis_norm_3`: `Solve` / `Qed` | 1,906s / 346s | 1,839s / 334s |

Both chose the mutual cofix (`Auto:` line in each log): mutex over 644
pairs and 1,932 moves, fifo over 644 pairs and 3,714 moves. `Qed` is a
large share of the fifo proof's time.

### Glued ~ layered

Both runs saturated FSM B on demand (1,091,776 weak actions from 12,027
states, over the 1,000,000 bound; 10,083 silent SCCs).

- **Nested (no `Bounds Game`):** Auto takes the nested cofix without
  estimating. `Sim Begin` took 30.6s. Out of memory in `Solve` after 2h05m.
- **`Bounds Game 100000000`:** Auto estimates, which took the whole of
  `Sim Begin`: 4,489s. It chose the mutual cofix over 78,202 pairs (312,808
  moves; the nested path would visit over 1,564,040 goals). `Solve` then
  ran about 14.7 hours until it was killed at 11:20:06. Resident memory
  (`rss-samples.txt`):

| elapsed | resident |
|---|---|
| 0:09 (first sample) | 3.9 GiB (4,010 MiB) |
| 1:00 | 5.6 GiB (5,737 MiB) |
| 2:00 | 11.7 GiB (11,948 MiB) |
| 3:00 | 12.5 GiB (12,844 MiB) |
| 4:00 | 13.8 GiB (14,144 MiB) |
| 5:00 | 15.3 GiB (15,698 MiB) |
| 6:00 | 16.7 GiB (17,133 MiB) |
| 6:00 to 10:42 | 16.7 GiB, then 16.7 (17,059 MiB), changing by under 75 MiB |
| 16:00 (last sample) | 24.0 GiB (24,544 MiB), the maximum |

So `Begin`'s estimate does not hold it down: most of the growth came during
the first hours, there was a flat stretch of about 4.6 hours at 16.7 GiB,
and then it grew again to the cap.

## What this says

- **Auto's step savings** on `MutualExclusionLarge.v` are real (three of
  five proofs), and cost nothing in status, time or memory.
- **The three-process proofs fit on this host, and `Answers Auto` is not why.**
  The `Default` runs of both take the same steps, and about the same time
  and memory, as the `Auto` runs. The 14GB host ran out of memory at 8GB on
  mutex under Auto (`ThreeProcessesMutex.v` header), so what changed is the
  host. These two files are the proofs their headers call "NOT PROVED: this
  is the proof that ran out of memory", over the same normalised semantics
  (644 states), so they are now proved, with the figures above.
- **Glued ~ layered is still not proved**, on either path, at 24 GiB.
  More memory than the 14GB host did not change that for the nested path
  (7-10GB there, 24 GiB here).

### A difference from the 14GB host's `Bounds Game` run, not explained

`TwoProcessesGluedLayered.v`'s header records that run (2026-10-12, before
`Answers Auto`): the mutual cofix over **514,020 pairs, 2,056,080 moves**,
unfinished after 2h28m with memory flat at ~2.2GB. This run, under Auto,
had a game of **78,202 pairs, 312,808 moves** (6.6 times fewer pairs), yet
memory was 12.1 GiB at the same point (2h29m: 12,412 MiB).
The two differ in answer policy and host (and possibly toolchain), so they are
not comparable, and I have not run `Bounds Game` under `-a Default` here.
That run would say whether the policy caused it.

## Not run

- `Bounds Game` under `-a Default` on this host (the previous point).
- Glued ~ layered at a cap above 24 GiB. The host has 31GB, so there is
  little room; the steady 16.7 GiB stretch and the final climb suggest the
  need is well above 24 GiB.

## Docs these results would update (not yet done)

`OUTSTANDING.md` ("CADP, the running example"; the "Waiting on Jonah"
entry for this re-run); `examples/README.md` (the three-process table);
`doc/decide-vs-prove.md` (rows marked not proved); `doc/decisions.md`
item 4; the headers of `ThreeProcessesMutex.v`, `ThreeProcessesFifo.v`,
`TwoProcessesGluedLayered.v` and `MutualExclusionLarge.v`, and the
comments in `_CoqProject`; `ASSISTED-CHANGES.md`.

## Corrections to the first pass (`ea452ed`)

- **Units.** The first pass wrote `peak_mb` values as decimal GB (13.7GB
  for 13,679 MiB). They are MiB; the figures above are GiB.
- **Caveat withdrawn.** It said the relation of `wsim_norm_3`/`wbis_norm_3`
  to the "not proved" lines was unchecked. The files' headers say they are
  those proofs.
- **Flat stretch.** It said memory stayed flat for "about 3 hours"; the
  samples show about 4.6 hours (16.7 GiB, 17,133 then 17,059 MiB).
- **An earlier claim in the session, not in this file.** The first glued
  run was first described as killed by the 4h timeout. It was an OOM kill
  at 24 GiB (kernel log), not a timeout.
