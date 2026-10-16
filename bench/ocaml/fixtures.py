#!/usr/bin/env python3
"""Make the OCaml benchmark's fixtures from the Rocq examples.

Used by make-fixtures.sh, in two steps:

  fixtures.py prepare SRC.v OUT.v MANIFEST.json
      Copy SRC.v to OUT.v with every proof that runs [MeBi Sim Begin]
      replaced by [Admitted.] and, in its place, the first time each pair of
      systems is met, a [MeBi Run Bisim] on that pair with result dumps on.
      The command then runs with the settings and in the scope the proof
      had. MANIFEST.json records, per command, its line in OUT.v and the
      pair.

  fixtures.py convert DUMPS MANIFEST.json TAG OUTDIR
      From the dumps the commands wrote into DUMPS (the compile
      directory's _dumps), write one fixture per command to OUTDIR, in the
      format bench/ocaml/fixture.ml reads.
"""
import json
import re
import sys
from pathlib import Path

PROOF = re.compile(r"Proof\.(.*?)(Qed|Defined|Abort|Admitted)\.", re.S)
BEGIN = re.compile(
    r"MeBi\s+Sim\s+Begin\s+(\S+)\s+(.+?)\s+And\s+(\S+)\s+(.+?)\s+Using\s+(.+?)\.(?=\s|$)",
    re.S,
)


def squash(s):
    return " ".join(s.split())


def in_comment(text):
    """in_comment(text)[i] is whether text[i] is inside a (nested) comment."""
    flags, depth, i = [False] * len(text), 0, 0
    while i < len(text):
        if text.startswith("(*", i):
            depth += 1
            flags[i : i + 2] = [True, True]
            i += 2
        elif depth and text.startswith("*)", i):
            depth -= 1
            flags[i : i + 2] = [True, True]
            i += 2
        else:
            flags[i] = depth > 0
            i += 1
    return flags


def prepare(src, out, manifest):
    text = Path(src).read_text()
    commented = in_comment(text)
    seen, commands, pieces, last = set(), [], [], 0
    for m in PROOF.finditer(text):
        if commented[m.start()]:
            continue
        b = BEGIN.search(m.group(1))
        # a proof expected to fail ([Fail MeBi Sim Begin ... Abort.]) is
        # left as it is
        if not b or m.group(2) == "Abort" or re.search(r"Fail\s+$", m.group(1)[: b.start()]):
            continue
        r1, x, r2, y, using = (squash(g) for g in b.groups())
        pieces.append(text[last : m.start()])
        last = m.end()
        key = frozenset([(r1, x), (r2, y)])
        if key in seen:
            pieces.append("Admitted.")
            continue
        seen.add(key)
        command = f"MeBi Run Bisim {x} With {r1} And {y} With {r2} Using {using}."
        before = "".join(pieces) + "Admitted.\n"
        commands.append(
            {"line": before.count("\n") + 2, "x": x, "y": y, "r1": r1, "r2": r2,
             "command": command}
        )
        pieces.append(
            'Admitted.\nMeBi Config Output "DumpResults" True. '
            + 'MeBi Config Output "DecodeResults" True. '
            + "MeBi Config FailIf NotBisimilar False.\n"
            + command
            + '\nMeBi Config Output "DumpResults" False. '
            + 'MeBi Config Output "DecodeResults" False. '
            + "MeBi Config FailIf NotBisimilar True."
        )
    pieces.append(text[last:])
    # Name a pair by its terms, with the relations where the two sides'
    # differ or where the terms alone would name two pairs alike.
    terms = [(c["x"], c["y"]) for c in commands]
    for c in commands:
        if c["r1"] != c["r2"] or terms.count((c["x"], c["y"])) > 1:
            c["x"], c["y"] = f"{c['r1']} {c['x']}", f"{c['r2']} {c['y']}"
    Path(out).write_text("".join(pieces))
    Path(manifest).write_text(json.dumps({"source": src, "commands": commands}))
    print(f"{src}: {len(commands)} pairs", file=sys.stderr)


def enc(x):
    """An encoded state or label: a bare encoding, or, when the dump was
    decoded, an object holding it."""
    return int(x["enc"]) if isinstance(x, dict) else int(x)


def side(fsm):
    edges = []
    for e in fsm["edges"]:
        f = int(e["From"])
        for a in e["Actions"]:
            l = int(a["Action"]["label"]["base"])
            edges += [[f, l, int(d)] for d in a["Destinations"]]
    return {
        "init": enc(fsm["init"]),
        "states": sorted(enc(s) for s in fsm["states"]),
        "edges": sorted(edges),
    }


def slug(s):
    return re.sub(r"[^A-Za-z0-9]+", "_", s).strip("_")[:40]


def convert(dumps, manifest, tag, outdir):
    m = json.loads(Path(manifest).read_text())
    files = list(Path(dumps).glob("*.json"))
    names = set()
    for c in m["commands"]:
        mine = [f for f in files if f"| line {c['line']} |" in f.name]
        a = b = result = None
        for f in mine:
            j = json.loads(f.read_text())
            if f.name.endswith("FSM a (original).json"):
                a = j["FSM"]
            elif f.name.endswith("FSM b (original).json"):
                b = j["FSM"]
            elif "Result" in j and "initial states related" in j["Result"]:
                result = j["Result"]["initial states related"]
        if a is None or b is None or result is None:
            sys.exit(f"{tag}: no dumps for line {c['line']}: {c['command']}")
        labels = {}
        for fsm in (a, b):
            for x in fsm["alphabet"]:
                e = x.get("EConstr", x.get("base"))
                term = e.get("term", "") if isinstance(e, dict) else ""
                labels[str(enc(e))] = (term, x["is_silent"])
        out = {
            "name": f"{tag}: {c['x']} ~ {c['y']}",
            "source": m["source"],
            "command": c["command"],
            "bisimilar": result,
            "silent": sorted(int(k) for k, (_, s) in labels.items() if s),
            "labels": {k: t for k, (t, _) in sorted(labels.items(), key=lambda kv: int(kv[0]))},
            "a": side(a),
            "b": side(b),
        }
        dest = Path(outdir) / f"{tag}__{slug(c['x'])}__{slug(c['y'])}.json"
        if dest.name in names:
            sys.exit(f"{tag}: two pairs would both be {dest.name}")
        names.add(dest.name)
        dest.write_text(json.dumps(out, separators=(",", ":")) + "\n")
        print(f"  {dest.name}: {len(out['a']['states'])}+{len(out['b']['states'])} states, bisimilar {result}", file=sys.stderr)


if __name__ == "__main__":
    cmd, *args = sys.argv[1:]
    {"prepare": prepare, "convert": convert}[cmd](*args)
