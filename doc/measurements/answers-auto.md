# `Answers Auto` as the default: measurements

Recorded 2026-10-15, for the writeup. Everything below is generated from
the raw files in [`answers-auto-2026-10-15/`](answers-auto-2026-10-15/)
(`bench/proofs.sh` output, one row per `MeBi Sim Solve`).

## What changed

`MeBi Config Solver Answers` defaults to `Auto` instead of `Default`. At
`Sim Begin`, `Auto` plans every answer under `Default`, `Greedy` and
`Minimal` and keeps the cheapest predicted. `Default` is the policy the
solver used until 2026-10-15; it is still one setting away
(`MeBi Config Solver Answers Default`). `Auto` is also no longer refused on
an FSM saturated on demand: without `Bounds Game` it answers move by move.

- **Before:** `main` at `a87217c`.
- **After:** branch `perf/answers-auto-default`, code at `d61e1a7`. The
  counts below are from that commit with the old bounds in the examples; a
  second run after the bounds were lowered (`69dfe1d`) gave identical
  counts (`default.new-final-bounds.*`).

## Headline

| | before | after |
|---|---|---|
| default suite, 61 Solves, `Auto` strategy | 58,899 steps | 30,049 steps (-49%) |
| same, `MutualCofix True` forced | 58,886 | 30,036 (-49%) |
| same, `MutualCofix False` forced (39 Solves) | 11,538 | 11,281 (-2%) |
| Solves that took more steps, over all 161 | | **0** |
| Solves that changed status | | **0** |
| Solves that took fewer steps | | 61 |
| `Test4` `weak_sim`, normalised semantics | 48,821 steps, 403s, 3.75GB | 6,115 steps, 32s, 0.72GB |

Counts are `Solved after N iterations` (the solver's step count; `Solve N`
permits `N + 1`). Times are the file's wall time with four files
compiling at once, so absolute figures depend on load; the ratios between
the two runs, made back to back, are the useful part.

## The default suite, per proof (`Auto` strategy)

Seven files: `Proc/Test1`, `Test2`, `Test3` (`PluginProofs.v`),
`CADP/MutualExclusion`, `CADP/GluedLayered`, `CCS/PluginProofs`,
`CCS/LawProofs`. Proofs that did not change are shown too, so the table
is the whole suite. `Solve` time is `Sim Solve` plus `Qed`.

| file | # | proof | before | after | steps | before, s | after, s |
|---|---|---|---:|---:|---:|---:|---:|
| Proc/Test1 | 1 | `wsim_pq` | 114 | 88 | -23% | 0.7s | 0.6s |
| Proc/Test1 | 2 | `wsim_qp` | 105 | 82 | -22% | 0.4s | 0.3s |
| Proc/Test1 | 3 | `wsim_qr` | 106 | 88 | -17% | 0.4s | 0.3s |
| Proc/Test1 | 4 | `wsim_rq` | 109 | 88 | -19% | 0.4s | 0.3s |
| Proc/Test1 | 5 | `wsim_pr` | 22 | 22 | = | 0.0s | 0.0s |
| Proc/Test1 | 6 | `wsim_rp` | 21 | 10 | -52% | 0.0s | 0.0s |
| Proc/Test1 | 7 | `wbis_pq` | 709 | 305 | -57% | 2.0s | 0.4s |
| Proc/Test1 | 8 | `wbis_qr` | 709 | 318 | -55% | 2.7s | 0.5s |
| Proc/Test1 | 9 | `wbis_pr` | 51 | 51 | = | 0.2s | 0.1s |
| Proc/Test2 | 1 | `wsim_pq` | 112 | 112 | = | 0.8s | 0.6s |
| Proc/Test2 | 2 | `wsim_qp` | 112 | 112 | = | 0.5s | 0.5s |
| Proc/Test2 | 3 | `wsim_qr` | 112 | 112 | = | 0.3s | 0.5s |
| Proc/Test2 | 4 | `wsim_rq` | 84 | 84 | = | 0.3s | 0.3s |
| Proc/Test2 | 5 | `wsim_pr` | 112 | 112 | = | 0.3s | 0.4s |
| Proc/Test2 | 6 | `wsim_rp` | 84 | 84 | = | 0.3s | 0.4s |
| Proc/Test2 | 7 | `wbis_pq` | 702 | 425 | -39% | 2.4s | 1.4s |
| Proc/Test2 | 8 | `wbis_qr` | 852 | 375 | -56% | 3.8s | 1.4s |
| Proc/Test2 | 9 | `wbis_pr` | 852 | 409 | -52% | 4.9s | 1.0s |
| Proc/Test3 | 1 | `wsim_p3` | 995 | 408 | -59% | 3.5s | 1.2s |
| Proc/Test3 | 2 | `wsim_pq` | 355 | 355 | = | 0.8s | 0.8s |
| Proc/Test3 | 3 | `wsim_qp` | 483 | 483 | = | 1.0s | 0.9s |
| Proc/Test3 | 4 | `wsim_qr` | 483 | 483 | = | 1.2s | 1.1s |
| Proc/Test3 | 5 | `wsim_rq` | 555 | 555 | = | 1.7s | 1.7s |
| Proc/Test3 | 6 | `wsim_pr` | 195 | 195 | = | 0.5s | 0.5s |
| Proc/Test3 | 7 | `wsim_rp` | 307 | 307 | = | 0.8s | 0.8s |
| Proc/Test3 | 8 | `wsim_rs` | 555 | 291 | -48% | 1.7s | 0.8s |
| Proc/Test3 | 9 | `wsim_sr` | 307 | 307 | = | 0.8s | 0.9s |
| Proc/Test3 | 10 | `wbis_p3` | 13,203 | 3,439 | -74% | 105s | 8.7s |
| Proc/Test3 | 11 | `wbis_pq` | 6,211 | 2,801 | -55% | 23s | 9.7s |
| Proc/Test3 | 12 | `wbis_qr` | 9,379 | 4,132 | -56% | 46s | 13s |
| Proc/Test3 | 13 | `wbis_pr` | 4,083 | 1,742 | -57% | 13s | 2.9s |
| Proc/Test3 | 14 | `wbis_rs` | 6,179 | 2,649 | -57% | 24s | 5.7s |
| CADP/MutualExclusion | 1 | `wsim_lts_1` | 259 | 259 | = | 19s | 12s |
| CADP/MutualExclusion | 2 | `wsim_spec_lts_1` | 212 | 202 | -5% | 1.0s | 0.5s |
| CADP/MutualExclusion | 3 | `wsim_bigstep_1` | 81 | 81 | = | 2.0s | 1.1s |
| CADP/MutualExclusion | 4 | `wsim_spec_bigstep_1` | 63 | 55 | -13% | 1.3s | 0.7s |
| CADP/MutualExclusion | 5 | `wbis_lts_1` | 1,442 | 1,442 | = | 15s | 10s |
| CADP/MutualExclusion | 6 | `wbis_bigstep_1` | 185 | 185 | = | 3.8s | 2.7s |
| CADP/GluedLayered | 1 | `wsim_bigstep_1` | 268 | 246 | -8% | 3.0s | 2.4s |
| CADP/GluedLayered | 2 | `wsim_lts_1` | 355 | 277 | -22% | 27s | 17s |
| CADP/GluedLayered | 3 | `wbis_bigstep_1` | 2,760 | 1,658 | -40% | 64s | 16s |
| CCS/PluginProofs | 1 | `wsim_milner_lr` | 24 | 24 | = | 0.1s | 0.2s |
| CCS/PluginProofs | 2 | `wsim_milner_rl` | 22 | 22 | = | 0.1s | 0.1s |
| CCS/PluginProofs | 3 | `wsim_early_late` | 34 | 34 | = | 0.1s | 0.1s |
| CCS/PluginProofs | 4 | `wsim_vm2_vm1` | 37 | 37 | = | 0.1s | 0.1s |
| CCS/PluginProofs | 5 | `wbis_buffers` | 175 | 175 | = | 1.1s | 0.9s |
| CCS/PluginProofs | 6 | `wsim_spec_abp` | 148 | 134 | -9% | 0.4s | 0.4s |
| CCS/LawProofs | 1 | `plsC` | 36 | 36 | = | 0.2s | 0.3s |
| CCS/LawProofs | 2 | `plsA` | 58 | 58 | = | 0.2s | 0.2s |
| CCS/LawProofs | 3 | `pls0p` | 19 | 19 | = | 0.0s | 0.1s |
| CCS/LawProofs | 4 | `plsp0` | 19 | 19 | = | 0.0s | 0.1s |
| CCS/LawProofs | 5 | `plsidem` | 56 | 56 | = | 0.1s | 0.2s |
| CCS/LawProofs | 6 | `tau1` | 42 | 42 | = | 0.1s | 0.1s |
| CCS/LawProofs | 7 | `tau2` | 53 | 27 | -49% | 0.1s | 0.1s |
| CCS/LawProofs | 8 | `tau3` | 28 | 28 | = | 0.1s | 0.1s |
| CCS/LawProofs | 9 | `tau0` | 15 | 15 | = | 0.0s | 0.0s |
| CCS/LawProofs | 10 | `expansion` | 98 | 98 | = | 0.3s | 0.5s |
| CCS/LawProofs | 11 | `paraC` | 278 | 278 | = | 1.2s | 1.5s |
| CCS/LawProofs | 12 | `para0p` | 357 | 357 | = | 2.4s | 3.1s |
| CCS/LawProofs | 13 | `parap0` | 325 | 325 | = | 1.7s | 1.6s |
| CCS/LawProofs | 14 | `paraA` | 3,192 | 2,836 | -11% | 25s | 17s |

## Per file: time and peak memory (`Auto` strategy)

| file | before | after |
|---|---:|---:|
| Proc/Test1 | 7s, 0.37GB | 3s, 0.34GB |
| Proc/Test2 | 14s, 0.45GB | 7s, 0.38GB |
| Proc/Test3 | 226s, 2.12GB | 51s, 0.66GB |
| CADP/MutualExclusion | 43s, 0.74GB | 28s, 0.73GB |
| CADP/GluedLayered | 95s, 0.77GB | 35s, 0.72GB |
| CCS/PluginProofs | 14s, 0.40GB | 13s, 0.39GB |
| CCS/LawProofs | 33s, 0.80GB | 26s, 0.72GB |

Forced `MutualCofix True` behaves the same way (`default.*.solves.tsv`,
mode `True`). Forced `False` stops at the first `weak_bisimilar` proof of
each file, as it did before; the proofs it reaches are `weak_sim`, and
their counts fall on `Proc/Test1`, `CADP/*` and `CCS/*`, and are equal on
`Proc/Test2` and `Proc/Test3`.

### Which policy `Auto` chose

From the `(Answers: ...)` lines in the logs of the 61 `Auto`-strategy
Solves of the default suite: `default` 36, `minimal` 18, `greedy` 7. It
chose `default` for every ABP proof and every `Fifo.v` proof (so their
counts are unchanged), and `minimal` for `Test4`'s `weak_sim`, whose plan
it predicted at 3,326 steps (it took 6,115).

## The evaluation series (`examples/Evaluation`)

| file | # | before | after | steps |
|---|---|---:|---:|---:|
| Width | 1 | 77 | 77 | = |
| Width | 2 | 1,163 | 974 | -16% |
| Width | 3 | 8,256 | 7,664 | -7% |
| Layers | 1 | 92 | 92 | = |
| Layers | 2 | 120 | 120 | = |
| Layers | 3 | 148 | 148 | = |
| Layers | 4 | 204 | 204 | = |
| Layers | 5 | 91 | 91 | = |
| Layers | 6 | 105 | 105 | = |
| Layers | 7 | 119 | 119 | = |
| Depth | 1 | 21 | 21 | = |
| Depth | 2 | 108 | 93 | -14% |
| Depth | 3 | 672 | 407 | -39% |
| Depth | 4 | 4,704 | 2,171 | -54% |

The `*Large` files were not re-run on `main`. Their checked-in bounds are
the old count minus one, so the old counts are the bounds plus one:

| file | # | before (bound + 1) | after | steps |
|---|---|---:|---:|---:|
| WidthLarge | 1 | 55,963 | 54,894 | -2% |
| LayersLarge | 1 | 316 | 316 | = |
| LayersLarge | 2 | 540 | 540 | = |
| LayersLarge | 3 | 988 | 988 | = |
| DepthLarge | 1 | 15,168 | 6,303 | -58% |
| DepthLarge | 2 | 35,136 | 13,827 | -61% |

After: WidthLarge 414s, 4.0GB; LayersLarge 87s, 2.6GB; DepthLarge 61s, 0.7GB. Before, from `_CoqProject`: WidthLarge ~7 min, ~3.9GB; LayersLarge ~2 min; DepthLarge ~10 min, ~3.1GB.

## Unchanged: ABP and `Fifo.v`

| file | before | after | time before | time after | peak before | peak after |
|---|---:|---:|---:|---:|---:|---:|
| CCS/ABPProofs | 6,494 | 6,494 | 267s | 261s | 4.25GB | 4.25GB |
| CCS/ABPBisimProofs | 9,914 | 9,914 | 273s | 277s | 4.42GB | 4.37GB |

`CADP/Fifo.v` (not in a bench suite; run on the new tree only): 119, 23, 2228, 98 steps, the figures its header already gave.
`Auto` kept `Default` for all of these: where `Default` is the cheapest plan,
`Auto` takes it, so nothing is lost.

## `theories/Test.v`

`bench/testv-counts.sh` on both trees: 65 counted Solves before, 69 after
(four tests added for the on-demand behaviour and the default). The
common ones that moved fall by one or two steps (`testv-base.txt`,
`testv-new.txt`).

## Method

```sh
git worktree add --detach ../mebi-base a87217c   # main, before
# the branch is perf/answers-auto-default
bench/proofs.sh -s default -m auto,True,False -j 4 -t <tag>   # both trees
bench/proofs.sh -s eval -m auto                               # both trees
bench/proofs.sh -s abp  -m auto                               # both trees
bench/proofs.sh -s test4 -m auto -f Proc/Test4/NormProofs.v   # both trees
bench/proofs.sh -s eval-large -m auto                         # new tree only
bench/testv-counts.sh                                         # both trees
bench/compare.sh <before> <after>
```

- **Machine:** Intel Core Ultra 5 125U, 14 cores, 14GB RAM, Linux 7.0.
- **Toolchain:** Rocq 9.2.0, OCaml 5.4.0, dune 3.23.1 (the local opam switch).
- **Isolation:** each file is its own `rocq compile` process, capped with
  `systemd-run --scope -p MemoryMax=...`; examples are compiled from a fresh
  copy. Counts are deterministic; times are not.
- **Forced strategies:** the harness drops each file's own `MutualCofix`
  setting and sets the mode after the plugin loads.

## Not measured

Figures elsewhere that were taken under `Answers Default` and have not been
re-run: `Test4/NormBisimProofs.v` (it sets `Answers Minimal` itself, so the
default does not reach it), `CADP/MutualExclusionLarge.v` and the two- and
three-process CADP figures (heavy, and out of memory before), the 3,859 for
`wbis_bigstep_1` under forced `False`, and `Test4`'s direct proof (not
attempted). Nothing suggests they got worse.

## Files

`answers-auto-2026-10-15/`: for each run, `<suite>.<before|new>.solves.tsv`
(suite, mode, file, index, status, iterations, proof, `Sim Begin`, `Sim Solve`
and `Qed` seconds) and `.runs.tsv` (suite, mode, file, exit, seconds, peak MB,
errors); `testv-base.txt` and `testv-new.txt`. `.new-final-bounds.` is the same
run after the checked-in bounds were lowered.
