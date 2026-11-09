#!/usr/bin/env python3
"""Claude Code PreToolUse hook (Edit, Write, MultiEdit): refuse writing
computation as proof into Rocq or OCaml sources (CLAUDE.md, hard rule 1).

Reads the hook's JSON on stdin; exits 2 (the tool call is blocked, the
message on stderr goes back to the model) if the text being written to a
.v/.ml/.mli/.mlg file uses one of the patterns of
scripts/check_no_computation.py. Removing a use passes (the new text no
longer has it). Exit 0 otherwise.
"""
import json
import os
import sys

sys.dont_write_bytecode = True  # no __pycache__ in scripts/
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from check_no_computation import EXTS, PATTERN  # noqa: E402


def main() -> int:
    try:
        data = json.load(sys.stdin)
    except ValueError:
        return 0
    ti = data.get("tool_input") or {}
    path = ti.get("file_path") or ""
    if not path.endswith(EXTS):
        return 0
    texts = [ti.get("new_string") or "", ti.get("content") or ""]
    texts += [e.get("new_string") or "" for e in ti.get("edits") or []]
    hits = sorted({m.group(0) for t in texts for m in PATTERN.finditer(t)})
    if not hits:
        return 0
    print(f"Refused: proof by computation ({', '.join(hits)} in {path}) "
          "(CLAUDE.md, hard rule 1). Certificates come from the engine as "
          "proof terms; nothing is recomputed in Rocq.", file=sys.stderr)
    return 2


if __name__ == "__main__":
    sys.exit(main())
