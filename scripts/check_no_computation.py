#!/usr/bin/env python3
"""No proof by computation in Rocq (CLAUDE.md, hard rule 1).

Fails if the Rocq or OCaml sources use computation as proof:
`vm_compute`, `native_compute`, VM and native casts, `Eval vm_compute`,
or the VM or native compiler called from OCaml. A proof is Sylvan's
certificate, loaded as a proof term for the kernel to typecheck; it is
never recomputed in Rocq. There is no allowlist, and the patterns are not
to be loosened.

Usage: python3 scripts/check_no_computation.py  (exit status 1 on failure)
"""
import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DIRS = ["theories", "src", "lib", "test", "examples", "bench"]
EXTS = (".v", ".ml", ".mli", ".mlg")
PATTERN = re.compile(
    r"vm_compute|native_compute|VMcast|NATIVEcast|vm_cast_no_check"
    r"|native_cast_no_check|Eval\s+(?:vm|native)"
    # the VM or native compiler called from OCaml (an oracle, or values
    # computed for a proof)
    r"|cbv_vm|Vnorm\.|Nativenorm|Vconv|Nativeconv|Vmvalues|Vmsymtable"
    r"|Nativelib")

def counts() -> dict:
    found = {}
    for d in DIRS:
        for base, dirs, files in os.walk(os.path.join(ROOT, d)):
            dirs[:] = [x for x in dirs if x not in ("_build", ".git")]
            for f in files:
                if not f.endswith(EXTS):
                    continue
                path = os.path.join(base, f)
                with open(path, encoding="utf-8", errors="replace") as h:
                    n = len(PATTERN.findall(h.read()))
                if n:
                    found[os.path.relpath(path, ROOT)] = n
    return found


def main() -> int:
    found = counts()
    for path, n in sorted(found.items()):
        print(f"{path}: {n} uses of computation as proof. A proof must be "
              "Sylvan's certificate, typechecked (CLAUDE.md, hard rule 1).")
    if not found:
        print("no computation as proof")
    return 1 if found else 0


if __name__ == "__main__":
    sys.exit(main())
