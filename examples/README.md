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
| `ProcCongruence.v` | A normalised semantics for `Proc.Layered`, with structural congruence made explicit, proved weakly bisimilar to it by hand. It carries proofs about `Test4` over to the original semantics (`wsim_transfer`, `wbis_transfer`). |
| `CCS.v` | Value-free CCS (Milner 1989) as one inductive LTS: vending machines, buffers, and the Alternating Bit Protocol. |
| `CADP.v` | The MCS queue lock from CADP's demo, in its layered semantics: see [CADP](#cadp) below. |
| `CADP_Glued.v` | The same lock in a glued big-step semantics, with a much smaller state space. |
| `CADP_simple.v` | An early, simpler counter-based lock. Not built and not used elsewhere. |
| `Bisimilarity/` | The proofs, one directory per calculus: see below. |
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
| `CADP/` | The MCS lock: see below. | |
| `_nat_streams.v` | Streams of naturals and their parity. | Unfinished draft, not built. |

## CADP

### The model

`CADP.v` models the MCS queue lock (Mellor-Crummey and Scott) as CADP's
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

### Layered and glued

There are two semantics for the same processes:
- **Layered**, `lts` over `step` (`CADP.v`): every action, branch and
  recursion unfolding is a step. This is the faithful model.
- **Glued**, `bigstep` (`CADP_Glued.v`): each acquire or release block runs
  as one silent step, so the state space is far smaller.

The intended argument:
- prove glued ≈ layered;
- prove a property LTS ≈ glued;
- conclude, by `weak_bisimilar_trans` (`theories/Bisimilarity.v`), that the
  layered system has the property.

The glued version exists because the layered one used to be too large to
extract beyond one process.

| size | processes | layered | glued | glued ≈ layered |
|---|---|---|---|---|
| `Size1` | 1 | small | small | proved (`Size1/Glued/PluginProofs.v`) |
| `Size2` | 2 | 12027 states (~32s) | 772 states | decided, bisimilar (`Size2/TermTests.v`, ~35s); not yet proved |
| `Size3` | 3 | not tried | 128044 states (~19 min, ~4.7GB) | not tried; saturating the glued side alone ran out of memory at 9GB |

Measured 2026-10-11. No transfer corollary (layered ≈ property) is stated
yet.

### `Properties/`

- **`MutualExclusion.v`**: `spec_lts`, a lock server that is `Free` or
  `Held i`, for pids `n .. 0` (`make_spec n`).
- **`_no_starvation.v`**: a draft monitor, not built. It is superseded by
  the `spec_fifo` design below.

**One process.** `spec_lts` is weakly *bisimilar* to the system, in both
semantics: layered (`Size1/MutualExclusion/PluginProofs.v`) and glued
(`Size1/Glued/MutualExclusion/PluginProofs.v`).

**Two processes.** The system and `spec_lts` are mutually similar but *not*
bisimilar (`Size2/Glued/MutualExclusion/PluginProofs.v`):
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

### No starvation: the `spec_fifo` design (agreed 2026-10-11, not yet written)

"Every waiting process eventually enters" is a liveness property.
Simulation and bisimulation cannot state it. Weak bisimilarity also ignores
divergence, and an MCS process waits by spinning, which is a silent loop.
What can be stated, and is MCS's actual guarantee, is **FIFO order**: once
a process has joined the queue, no process that joins later enters before
it.

The plan:
- **Make joining the queue visible.** Add a label `REQ i` on the tail swap:
  `FETCH_AND_STORE` in `step`, inside `DO_ACQUIRE` in `bigstep`. Do it in
  an observing variant of both semantics, so the existing proofs and their
  step counts stay as they are. It must be the same in both, so that
  glued ≈ layered still means something.
- **`spec_fifo`.** A lock server with a queue. `REQ i` appends `i`,
  `ENTER i` is allowed only for the head of the queue, and `LEAVE i` pops
  it. It is mutually exclusive and FIFO by construction.
- **Prove `weak_bisimilar` between the system and `spec_fifo`,** then carry
  it to the layered system. One theorem then gives:
  - mutual exclusion;
  - FIFO order;
  - no deadlock short of divergence: the system can always do what the
    queue allows next.

  No starvation follows *under a fair scheduler*. Fairness is an assumption
  stated alongside, not proved, as it is in CADP's own check, which is also
  a safety property over traces.

`OUTSTANDING.md` §2 tracks what is still open.
