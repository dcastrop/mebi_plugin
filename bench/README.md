# Benchmarks

`bench/prooflog.py` measures the proofs of the CADP examples
(doc/certificate.md): each file copied into `_certify/log/` and compiled,
each goal decided, proved from Sylvan's certificate and checked at `Qed`
(timed with `Time Qed` in the copy), or by `MeBi Prove` (its pieces
checked as they are declared; the times from its message). Each file runs in its own
memory-capped process (`systemd-run --user --scope -p MemoryMax=<cap>
-p MemorySwapMax=0`), the cap sized from the memory available when it
starts, less 4GB, at most `--cap`.

    dune build
    python3 bench/prooflog.py                                  # two- and three-process CADP
    python3 bench/prooflog.py examples/CADP/TwoProcesses.v     # one file

Reference (11 November 2026): two processes (12,027 pairs), the file in
about 6s (the proof built in 3.3s, `Qed` 1.7s), 0.83GB at most; three
processes (7.9M pairs), by `MeBi Prove` (12 November 2026): the lemma
declared in about 280s, 2.6GB at most (by the tactic: the proof built in
124s, `Qed` 144s, the file in 293s, 6.6GB). Each job's cap is the
memory available when it starts less 4GB, at most `--cap` (7000M):
three processes need about 7GB available. The examples prove only that
the server simulates the system (P ≤ I). Each file's output, with the
phases' times and memory (`MEBI_CERT_STATS=1`), is in
`_certify/log/*.out`. To measure the certificate's facts alone, keep the
log (`Set MeBi Log Mode "keep"`) and check it with `MeBi Check Log` in a
file of its own.

Run heavy files one at a time, with a cap well below the free memory.
`test/cert_tool.exe --log <log>` runs the translation alone, with the facts
per lemma.
