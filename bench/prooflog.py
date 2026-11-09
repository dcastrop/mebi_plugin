#!/usr/bin/env python3
"""Proofs of the CADP examples (doc/certificate.md): each file copied into
`_certify/log/` and compiled, each goal decided, proved from Sylvan's
certificate and checked at `Qed` (`Time Qed`, in the copy). One job at a
time, each memory-capped, with the time to build each proof, the time of
its `Qed`, and the job's time and peak memory; the phases
(`MEBI_CERT_STATS`) are in `_certify/log/*.out`.

Usage: bench/prooflog.py [--cap 7000M] [--timeout 3000] [FILE.v ...]
(default: examples/CADP/TwoProcesses.v and ThreeProcesses.v). The cap of
each job, at most --cap, is what is available when it starts less 4GB
(CLAUDE.md: never push the machine into swap; the margin leaves room for
a program started meanwhile); a job that would get less than 600MB is
skipped. Run `dune build` first. Two processes: the file in about 6s,
0.83GB at most; three processes: the proof built in 124s, `Qed` 144s,
6.6GB at most, so it needs about 11GB available (11 November 2026).
"""
import argparse
import glob
import os
import re
import subprocess
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DEFAULT = ["examples/CADP/TwoProcesses.v", "examples/CADP/ThreeProcesses.v"]
B = os.path.join(ROOT, "_build")
FLAGS = [
    "-native-compiler", "no",
    "-R", f"{B}/default/theories", "MEBI",
    "-R", f"{B}/default/examples", "MEBI.Examples",
    "-I", f"{B}/install/default/lib",
]


def cap_mb(limit: int) -> int:
    """available memory now, less 4GB, at most [limit] (MB)"""
    with open("/proc/meminfo") as f:
        avail = next(int(l.split()[1]) for l in f if l.startswith("MemAvailable:"))
    return min(avail // 1024 - 4096, limit)


def run(cwd: str, vfile: str, env: dict, limit: int, timeout: int):
    cap = cap_mb(limit)
    if cap < 600:
        return None, f"skipped: {cap + 4096}MB available"
    cmd = [
        "systemd-run", "--user", "--scope", "-q",
        "-p", f"MemoryMax={cap}M", "-p", "MemorySwapMax=0",
        "env", *[f"{k}={v}" for k, v in env.items()],
        "timeout", str(timeout),
        "/usr/bin/time", "-f", "%e s, %M kB",
        "rocq", "compile", *FLAGS, vfile,
    ]
    p = subprocess.run(cmd, cwd=cwd, capture_output=True, text=True)
    out = p.stdout + p.stderr
    with open(os.path.join(cwd, vfile[:-2] + ".out"), "w") as f:
        f.write(out)
    last = out.strip().split("\n")[-1] if out.strip() else ""
    return p.returncode, f"cap {cap}M, {last}", out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--cap", default="7000M")
    ap.add_argument("--timeout", type=int, default=3000)
    ap.add_argument("files", nargs="*")
    a = ap.parse_args()
    limit = int(a.cap.rstrip("Mm")) if a.cap[-1] in "Mm" else int(float(a.cap.rstrip("Gg")) * 1024)
    work = os.path.join(ROOT, "_certify", "log")
    os.makedirs(work, exist_ok=True)
    bad = 0
    for f in a.files or DEFAULT:
        name = os.path.basename(f)[:-2]
        text = open(os.path.join(ROOT, f)).read()
        # each tactic's proof timed at its Qed
        text = re.sub(r"(Proof\. (?:weak_sim|weak_bisim|sim|bisim)\.) Qed\.", r"\1 Time Qed.", text)
        with open(os.path.join(work, name + ".v"), "w") as g:
            g.write(text)
        for old in glob.glob(os.path.join(work, f".{name}.v.mebi", "*")):
            os.remove(old)
        r = run(work, name + ".v", {"MEBI_CERT_STATS": "1"}, limit, a.timeout)
        if r[0] is None:
            print(f"{f}: {r[1]}")
            continue
        code, what, out = r
        built = re.findall(r"decided \(([\d.]+) pairs\); the proof from Sylvan's certificate built in ([\d.]+)s", out)
        qeds = re.findall(r"Finished transaction in ([\d.]+) secs", out)
        if code != 0:
            i = out.find("Error")
            print(f"{f}: NOT PROVED, exit {code} ({what})\n  " + out[i:i + 300].replace("\n", "\n  "))
            bad += 1
            continue
        print(f"{f}: proved ({what})")
        for (pairs, b), q in zip(built, qeds):
            print(f"  {pairs} pairs: proof built in {b}s, Qed in {q}s")
        for ext in (".vo", ".vos", ".vok", ".glob"):
            p = os.path.join(work, name + ext)
            if os.path.exists(p):
                os.remove(p)
    sys.exit(1 if bad else 0)


if __name__ == "__main__":
    main()
