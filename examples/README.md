# examples/

Worked examples of the MeBi plugin, mapped to the logical path
`MEBI.Examples`. They show the plugin on real systems, and the paper may
cite them. Plain tests (limits, refusals, regression pins) live in
`theories/Test.v` instead. Evaluation series, one lever each, live in
`Evaluation/`.

Most proof files are commented out of `_CoqProject`, because full proof
search is slow. Each line there says how long its file takes and whether it
needs a memory cap. To build one, uncomment it and run `make`; or use
`bench/proofs.sh`, which compiles copies under a memory cap (see
`bench/README.md`). Run anything heavy under `systemd-run --user --scope
-p MemoryMax=6G -p MemorySwapMax=0`.

## Layout

| path | what it holds |
|---|---|
| `Proc.v` | A small process calculus with sends and receives on labels `A`/`B`/`C`, in two styles. `Flat` has one relation; `Layered` has terms and a composition of processes (`comp`), each with its own relation. |
| `ProcCongruence.v` | A normalised semantics for `Proc.Layered`, with structural congruence made explicit, proved weakly bisimilar to it (by `theories/Transfer.v`'s `stutter_bisimilar`). It carries proofs about `Test4` over to the original semantics (`wsim_transfer`, `wbis_transfer`). |
| `CCS.v` | Value-free CCS (Milner 1989) as one inductive LTS: vending machines, buffers, and the Alternating Bit Protocol. |
| `CADP/` | The MCS queue lock from CADP's demo: the model, its semantics, the specs and the proofs, one file per claim. See [CADP](#cadp) below. |
| `Bisimilarity/` | The proofs for `Proc` and `CCS`, one directory per calculus: see below. |
| `Evaluation/` | Benchmark series for width, semantic layers and nesting depth. `*Large.v` are scaled up for `bench/proofs.sh -s eval-large`. |

### File roles in `Bisimilarity/`

The same names recur in every directory:
- `Terms.v`: the systems compared.
- `TermTests.v`: `MeBi Run` commands on them (extract, saturate, minimise,
  decide bisimilarity), with no proofs.
- `PluginProofs.v`: `weak_sim`, `mutual_sim` and `weak_bisimilar` goals
  proved by `MeBi Sim Begin`/`Solve`. These are the end-to-end test of the
  proof solver. `CLAUDE.md` lists their expected step counts.
- `ManualProofs.v`, `LtacProofs.v` (`Proc/Test1`, `Test2`): the same goals
  proved by hand and by custom Ltac, for comparison.
- `Norm*.v` (`Proc/Test4`): `Test4` handled through `ProcCongruence.v`.

### `Bisimilarity/`

| directory | systems | status |
|---|---|---|
| `CCS/` | `PluginProofs.v`: textbook pairs. `LawProofs.v`: instances of CTrees' CCS laws. `ABPProofs.v`/`ABPBisimProofs.v`: the Alternating Bit Protocol against a one-place buffer. | The first two are built by default. The ABP proofs take ~4 min and ~4.2GB each. |
| `Proc/Test1`, `Test2` | `Proc.Flat`: three recursive terms each, pairwise. | Fast. |
| `Proc/Test3` | `Proc.Layered`: senders and receivers in parallel. | ~18s. |
| `Proc/Test4` | `Proc.Layered`, four processes (9720 states). | Decided directly. Proved only through the normalised semantics (`Norm*.v`, ~4-7 min). |
| `_nat_streams.v` | Streams of naturals and their parity. | Unfinished draft, not built. |

## CADP

CADP is the running example: it shows what the plugin can do and where
it stops. The contribution is the plugin, not the model. The model is
kept faithful to CADP's demo and is not reformulated (for example with
program counters) to reach more processes, so a size the plugin cannot
handle is reported as a limit. The glued and normalised semantics exist
to make proofs tractable; the point is that results proved for them carry
over to the layered model (below).

### The model

`CADP/Model.v` models the MCS queue lock (Mellor-Crummey and Scott) as CADP's
demo does. One difference: shared memory is a resource that the processes
read and write directly, not a separate process. Each process loops through
four phases:

1. **Acquire.** Swap itself into the lock's tail (`FETCH_AND_STORE`).
   - With no predecessor, it holds the lock.
   - Otherwise it links itself behind the predecessor and spins on its own
     `locked` flag.
2. `ENTER` the critical section.
3. `LEAVE` it.
4. **Release.** Hand the lock to its successor by clearing that successor's
   flag. If it has no successor, it compare-and-swaps the tail back to nil;
   if that fails, a successor is halfway through enqueueing, so it waits
   for the link to appear.

Only `ENTER i` and `LEAVE i` are visible (`get_action`); all memory traffic
is silent. `composition_create n` builds `n + 1` processes, with pids
`n .. 0`.

### Layered, glued and normalised

There are three semantics for the same processes:
- **Layered**, `lts` over `step` (`Model.v`): every action, branch and
  recursion unfolding is a step. This is the faithful model.
- **Glued**, `bigstep` (`Glued.v`): each acquire or release block runs
  as one silent step, so the state space is far smaller.
- **Normalised**, `bigstep_norm` (`Norm.v`): `bigstep` with dead data
  erased after every step. Each glued block writes the local variables it
  reads, so between steps they are dead, and the resource keeps two copies
  of the last value read that nothing reads. `Norm.v` proves
  `bigstep_norm` weakly bisimilar to `bigstep` (with `theories/Transfer.v`), for every number of
  processes, following the recipe in `doc/structural-congruence.md`. The
  proof needs every state to be well formed (all indices in range), which
  every `composition_create` is.

The intended argument:
- prove glued ≈ layered;
- prove a property LTS ≈ glued, possibly over `bigstep_norm` and carried
  back by `Norm.v`'s transfer theorems;
- conclude, by `weak_bisimilar_trans` (`theories/Bisimilarity.v`), that the
  layered system has the property.

The glued version exists because the layered one used to be too large to
extract beyond one process.

| size | processes | layered | glued | normalised | glued ≈ layered |
|---|---|---|---|---|---|
| `Size1` | 1 | small | small | 5 states | proved (`GluedLayered.v`) |
| `Size2` | 2 | 12027 states (~32s) | 772 states | 48 states | decided, bisimilar (`ExploreLarge.v`, ~35s); not proved: out of memory at 10GB (nested cofix), unfinished after 2.5h (mutual) (`GluedLayered.v`) |
| `Size3` | 3 | not tried | 128044 states (~19 min, ~4.7GB) | 644 states (~3s) | not tried |

Glued ≈ normalised needs no plugin run: it is `Norm.link`, for every
size.

Measured 2026-10-11.

The argument is carried through for one process in `LayeredTransfer.v`,
with no plugin call: glued ≈ layered (`GluedLayered.v`) and glued ≈ the
FIFO server with joining hidden (`Fifo.v`, below) give
`weak_bisimilar lts spec_fifo_hidden c1 (make_fifo 0)`, and from it mutual
exclusion for the layered system, independently of its direct proof in
`MutualExclusion.v`. For two or more processes it stops at glued ≈
layered.

### Files

All in `examples/CADP/`. The files a claim's proofs need are built by
default; the proofs are commented out of `_CoqProject`. A `*Large.v` file
holds what is too heavy for `bench/proofs.sh`'s default suite and needs a
memory cap.

| file | what it holds |
|---|---|
| `Model.v`, `Glued.v`, `Norm.v`, `Obs.v` | the model and its semantics (above, and "No starvation" below) |
| `SpecMutex.v`, `SpecFifo.v`, `Hidden.v` | the specs (below) |
| `Terms.v` | the systems: `c1`, `c2`, `c3` for one, two and three processes |
| `Explore.v`, `ExploreLarge.v` | state spaces and decisions (`MeBi Run`), no proofs |
| `GluedLayered.v` | glued ≈ layered |
| `MutualExclusion.v`, `MutualExclusionLarge.v` | mutual exclusion: one process, then two and three |
| `Fifo.v` | FIFO order, one and two processes |
| `LayeredTransfer.v` | the transfer to layered, one process |
| `Simple.v` | an early, simpler counter-based lock; not built and not used elsewhere |

A proof's name ends in the number of processes (`wsim_bigstep_2`). Until
2026-10-13 the proofs were in `examples/Bisimilarity/CADP/`, one
directory per size, semantics and property; each file's header names
where its proofs came from.

### The specs

- **`SpecMutex.v`**: `spec_lts`, a lock server that is `Free` or
  `Held i`, for pids `n .. 0` (`make_spec n`).
- **`SpecFifo.v`**: `spec_fifo`, a FIFO lock server (see "No starvation"
  below).
- **`Hidden.v`**: joining the queue hidden again, with
  `theories/Hiding.v`. `bigstep` *is* `bigstep_obs` with that label hidden
  (`bigstep_hides`), and `spec_fifo_hidden` is `spec_fifo` with it hidden.
  So FIFO order for `bigstep_obs` gives `weak_bisimilar bigstep
  spec_fifo_hidden` (`wbis_hidden`).

**One process.** `spec_lts` is weakly *bisimilar* to the system, in both
semantics, layered and glued (`MutualExclusion.v`).

**Two processes.** The system and `spec_lts` are mutually similar but *not*
bisimilar. `MutualExclusionLarge.v` proves both simulations directly
(11,128 and 405 steps, ~10 min, ~4.4GB), and over `bigstep_norm`,
transferred (1,038 and 153 steps, ~15s). A third route goes
through FIFO order: `Fifo.v` derives system ≤
spec from `wbis_hidden` and a 98-step proof that `spec_fifo_hidden` ≤
`spec_lts` (23 steps for one process).
- **System ≤ spec** is mutual exclusion proper: every `ENTER`/`LEAVE` trace
  of the system is allowed by the server.
- **Spec ≤ system** holds because the simulating side picks its own silent
  steps. The system can always answer along a path where the other process
  has not yet queued.
- **Bisimilarity fails** because MCS fixes the order of entry *silently*.
  Once pid 1 has swapped itself into the tail behind pid 0, only `ENTER 1`
  can follow `LEAVE 0`. `spec_lts` has no such state: when `Free`, it lets
  either pid enter next.

So mutual exclusion holds, but `spec_lts` cannot be *the* property LTS
bisimilar to the system once there is more than one process. The order is
part of the system's behaviour, and the spec does not see it.

**Three processes.** Only through `bigstep_norm` (644 states; `bigstep`
has 128,044). As before, the system and `spec_lts` are not bisimilar
(`Explore.v`). Spec ≤ system is proved
(`MutualExclusionLarge.v`, 326 steps, ~7s). Mutual
exclusion proper, system ≤ spec, is not proved: its proof was OOM-killed
at 9GB after ~13 min. It is decided only indirectly: the system is
bisimilar to the FIFO server (below), which lets only the head of its queue
enter. Going from there to `bigstep`, by hiding the queue label, is a
theorem (`Hidden.v`); what is missing is a *proof* of the FIFO
bisimilarity at three processes, which is only decided.

### No starvation: FIFO order (`Fifo.v`)

"Every waiting process eventually enters" is a liveness property.
Simulation and bisimulation cannot state it. Weak bisimilarity also ignores
divergence, and an MCS process waits by spinning, which is a silent loop.
What can be stated, and is MCS's actual guarantee, is **FIFO order**: once
a process has joined the queue, no process that joins later enters before
it.

How it is stated:
- **Joining the queue is made visible.** `bigstep_obs` (`Obs.v`) is
  `bigstep` with its one atomic tail swap, `DO_ACQUIRE`, labelled
  `(FETCH_AND_STORE, pid)` instead of silent. `bigstep` and its proofs are
  unchanged; hiding that label gives `bigstep` back, as a theorem
  (`Hidden.v`).
- **`spec_fifo`** (`SpecFifo.v`) is a lock server with idle pids and
  a queue. `REQ i` (the same label) moves an idle `i` to the back of the
  queue; `ENTER` is allowed only for the head; `LEAVE` makes the head idle
  again. It is mutually exclusive and FIFO by construction.
- **The system is weakly bisimilar to `spec_fifo`.** The plugin proves it
  over the normalised `bigstep_obs_norm`, and `wbis_transfer_obs` carries
  it to `bigstep_obs`:

  | processes | states | result |
  |---|---|---|
  | 1 | 5 | proved, 119 steps (`Fifo.v`) |
  | 2 | 48 | proved, 2,228 steps, ~24s (`Fifo.v`) |
  | 3 | 644 | decided, ~4s (`Explore.v`); proof OOM-killed at 9GB after ~15 min |

  As a check that the verdict discriminates, a LIFO variant of the spec is
  found not bisimilar for three processes.

One theorem then gives:
- mutual exclusion;
- FIFO order;
- no deadlock short of divergence: the system can always do what the queue
  allows next.

No starvation follows *under a fair scheduler*. Fairness is an assumption
stated alongside, not proved, as it is in CADP's own check, which is also
a safety property over traces.

For one process, the layered system is weakly bisimilar to
`spec_fifo_hidden` (`LayeredTransfer.v`): FIFO order with joining hidden,
which still gives mutual exclusion. FIFO order proper, with joining
visible, would need the same label in `step`, and more than one process
would need a proof of glued ≈ layered, which so far is only decided at two
processes. `OUTSTANDING.md` §2 tracks both.
