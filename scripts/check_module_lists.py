#!/usr/bin/env python3
"""Check that the plugin's three module lists agree with the source tree.

The OCaml sources under lib/ and src/ are listed three times, by hand:

  - _CoqProject           every .ml/.mli/.mlg, for the `make` build
  - src/mebi_plugin.mlpack every module, for linking the plugin under `make`
  - each dune (modules ...) every module, for the `dune` build

Nothing kept them in sync, and they have drifted more than once: a module
registered in dune but not in the other two passes `dune build` and fails
`make` (backlog item C3). This compares all three against the files actually
on disk and exits non-zero, naming every mismatch, if they disagree.

Only set membership is checked. Link order in the .mlpack still matters, but
`make` reports a misordering itself, loudly, at link time.

Run from the repository root: python3 scripts/check_module_lists.py
"""

import re
import sys
from pathlib import Path

ROOTS = ("lib", "src")
SOURCE_SUFFIXES = (".ml", ".mli", ".mlg")
MLPACK = Path("src/mebi_plugin.mlpack")
COQPROJECT = Path("_CoqProject")


def module_name(path: Path) -> str:
    """OCaml's module name for a file: its stem, first letter capitalised."""
    return path.stem[:1].upper() + path.stem[1:]


def sources_on_disk() -> set[Path]:
    return {
        p
        for root in ROOTS
        for p in Path(root).rglob("*")
        if p.suffix in SOURCE_SUFFIXES and "_build" not in p.parts
    }


def coqproject_entries() -> set[Path]:
    entries = set()
    for line in COQPROJECT.read_text().splitlines():
        line = line.split("#", 1)[0].strip()
        if line.startswith(ROOTS) and line.endswith(SOURCE_SUFFIXES):
            entries.add(Path(line))
    return entries


def mlpack_modules() -> set[str]:
    return {
        line.strip()
        for line in MLPACK.read_text().splitlines()
        if line.strip() and not line.strip().startswith("#")
    }


def dune_modules() -> dict[str, list[Path]]:
    """Module name -> every dune file whose (modules ...) claims it."""
    claims: dict[str, list[Path]] = {}
    for dune in (p for root in ROOTS for p in Path(root).rglob("dune")):
        if "_build" in dune.parts:
            continue
        # Drop ;-comments first: they appear inside src/dune's modules list.
        text = "\n".join(l.split(";", 1)[0] for l in dune.read_text().splitlines())
        # A (rocq.pp (modules g_mebi)) stanza only preprocesses the .mlg; the
        # module still belongs to the library that lists it.
        text = re.sub(r"\((?:rocq|coq)\.pp\b[^()]*\(modules[^()]*\)\s*\)", "", text)
        for body in re.findall(r"\(modules\b([^()]*)\)", text):
            for name in body.split():
                name = name[:1].upper() + name[1:]
                claims.setdefault(name, []).append(dune)
    return claims


def main() -> int:
    disk = sources_on_disk()
    # Every stem is a module: .ml/.mlg implementations, and interface-only
    # modules such as lib/terms/base_.mli, which have no .ml at all.
    disk_modules = {module_name(p) for p in disk}
    problems: list[str] = []

    listed = coqproject_entries()
    for p in sorted(disk - listed):
        problems.append(f"_CoqProject: missing {p}")
    for p in sorted(listed - disk):
        problems.append(f"_CoqProject: lists {p}, which does not exist")

    packed = mlpack_modules()
    for m in sorted(disk_modules - packed):
        problems.append(f"{MLPACK}: missing module {m}")
    for m in sorted(packed - disk_modules):
        problems.append(f"{MLPACK}: lists module {m}, which has no source file")

    claims = dune_modules()
    for m in sorted(disk_modules - claims.keys()):
        problems.append(f"dune: no (modules ...) stanza claims {m}")
    for m in sorted(claims.keys() - disk_modules):
        problems.append(f"dune: claims module {m}, which has no source file")
    for m, files in sorted(claims.items()):
        if len(files) > 1:
            where = ", ".join(str(f) for f in files)
            problems.append(f"dune: module {m} claimed more than once ({where})")

    if problems:
        print("Module lists disagree with the source tree:", file=sys.stderr)
        for p in problems:
            print(f"  {p}", file=sys.stderr)
        return 1
    print(f"Module lists agree: {len(disk_modules)} modules, {len(disk)} files.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
