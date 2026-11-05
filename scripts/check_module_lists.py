#!/usr/bin/env python3
"""Check that dune's module lists agree with the source tree.

The OCaml sources under lib/ and src/ are listed by hand in each dune
file's (modules ...). dune builds only what is listed, so a source file
missing from every list is silently left out of the build. This compares
the lists against the files on disk and exits non-zero, naming every
mismatch: a file no list claims, a listed module with no file, or a module
claimed twice.

Until 2026-11-05 two more lists existed for the `make` build (_CoqProject
and src/mebi_plugin.mlpack), and drift between the three had broken one
build or the other (backlog item C3). The project now builds with dune
alone.

Run from the repository root: python3 scripts/check_module_lists.py
"""

import re
import sys
from pathlib import Path

ROOTS = ("lib", "src")
SOURCE_SUFFIXES = (".ml", ".mli", ".mlg")


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
