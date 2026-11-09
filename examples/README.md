# examples/

Worked examples of the MeBi plugin, mapped to the logical path
`MEBI.Examples`: each states a claim about a system in its own, faithful
semantics; the plugin decides it, Sylvan logs the certificate, and the
tactic closes the goal with the proof built from that certificate
([`doc/certificate.md`](../doc/certificate.md)), which the kernel checks
at `Qed`. A goal that does not hold is a `Fail` (a decided "no"). Plain
tests live in `test/rocq/`.

`dune build` builds the examples `examples/dune` lists.
`python3 bench/prooflog.py` runs `CADP/TwoProcesses.v` and
`CADP/ThreeProcesses.v` (to `Qed` in about 5 minutes, 6.6GB), each
step under a memory cap (`bench/README.md`). `Bisimilarity/Proc/Test4/TermTests.v`
is not built.

## Layout

| path | what it holds |
|---|---|
| `CCS.v` | Value-free CCS (Milner 1989) as one inductive LTS: vending machines, buffers, and the Alternating Bit Protocol. |
| `Bisimilarity/CCS/PluginProofs.v` | Textbook pairs, each stating exactly what holds (simulations each way, bisimilarity, and decided refusals), and the Alternating Bit Protocol against a one-place buffer (the buffer simulates the protocol, and the two are weakly bisimilar). About 2s. |
| `Proc.v` | A small process calculus with sends and receives on labels `A`/`B`/`C`, in two styles: `Flat` (one relation) and `Stacked` (terms, and a composition of processes, each with its own relation). |
| `Bisimilarity/Proc/Test1`-`Test3` | `Terms.v` (the systems compared) and `PluginProofs.v` (`weak_sim` and `weak_bisimilar` goals). Under a second each. |
| `Bisimilarity/Proc/Test4` | `Proc.Stacked`, four processes: `Terms.v`, and `TermTests.v`, its goals (9,720 states, 120 shapes; decided in under 1s; to be certified up to reordering, `doc/optimisations.md`). |
| `CADP/` | The MCS queue lock from CADP's demo against a lock server: see below. |

## CADP

CADP is the running example. The model is kept faithful to CADP's demo,
one step per instruction, so a size the plugin cannot handle is reported
as a limit rather than avoided by another semantics.

### The model

`CADP/Model.v` models the MCS queue lock (Mellor-Crummey and Scott) as
CADP's demo does. One difference: shared memory is a resource that the
processes read and write directly, not a separate process. Each process
loops through four phases:

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
is silent. `composition_create n` (`CADP/Terms.v`: `c1`, `c2`, `c3`)
builds `n + 1` processes, with pids `n .. 0`. The semantics is `lts` over
`step`, one step per instruction.

### The specification and the claim

`CADP/SpecMutex.v`: `spec_lts`, a lock server that is `Free` or `Held i`,
for pids `n .. 0` (`make_spec n`). `spec_lts_mutex`: every trace of the
server alternates `ENTER i` and `LEAVE i` for the same `i`.

The claim is that the server simulates the processes, `weak_sim lts
spec_lts c (make_spec n)`: every trace of the processes is one of the
server's, so by `weak_sim_safe` (`theories/Traces.v`) the processes are
mutually exclusive.

| file | processes | states | what is proved |
|---|---|---|---|
| `MutualExclusion.v` | 1 | small | the server simulates the system, and the two are weakly bisimilar |
| `TwoProcesses.v` | 2 | 12,027 | the server simulates the system (`Qed`; the file in about 6s, 0.83GB) |
| `ThreeProcesses.v` | 3 | 7,935,608 | the server simulates the system (`Qed`; the file in about 5 minutes, 6.6GB) |

Only the direction **system ≤ spec** is decided: it is mutual exclusion
proper (every `ENTER`/`LEAVE` trace of the system is allowed by the
server), and the other direction is not needed for verification.

**One process.** The server and the system are also weakly bisimilar.

**Two or more processes.** They are not bisimilar:
- **Bisimilarity fails** because MCS fixes the order of entry *silently*.
  Once pid 1 has swapped itself into the tail behind pid 0, only `ENTER 1`
  can follow `LEAVE 0`; `spec_lts`, when `Free`, lets either pid enter next.

So mutual exclusion holds, but `spec_lts` cannot be *the* property LTS
bisimilar to the system once there is more than one process: the order is
part of the system's behaviour, and the spec does not see it. Weak
simulation and bisimilarity ignore divergence, so these results say
nothing about starvation (`theories/Traces.v`).
