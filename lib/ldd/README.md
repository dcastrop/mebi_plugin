# lib/ldd: Sylvan's List Decision Diagrams, from OCaml

`ldd.mli` is the interface; `ldd_stubs.c` the C side; `test/ldd_tests.ml`
the tests (no Rocq).

## Sylvan and Lace

Both are git submodules, forks, so that changes upstream
can be pulled and our own changes, if any, kept on a branch of the fork:

| submodule | fork | upstream | commit |
|---|---|---|---|
| `vendor/sylvan` | github.com/dcastrop/sylvan | github.com/trolando/sylvan | the local branch `proof-log`, on the fork's `master` (`5e8b8d4`, 2026-11-10) |
| `vendor/lace` | github.com/dcastrop/lace | github.com/trolando/lace | `v1.10.0` (`2c25581`, the version this Sylvan's CMake build pins) |

Lace is unmodified. Sylvan's own changes (the proof log, to a file or
kept in memory, `doc/certificate.md`, A) go on the local branch
`proof-log`, kept
rebased onto the fork's `master` (`git -C vendor/sylvan rebase
origin/master`; the branch has no upstream, so a plain `git push`
cannot reach the fork's `master`). It is not pushed unless asked; until
then, CI and fresh clones cannot fetch the pointer's commit. Until
2026-11-10 the pins were Sylvan `2d0a348` and Lace `11d85db` (1.7.0). A fresh clone needs them:
`git clone --recursive ...`, or `git submodule update --init` after a
plain clone.

**Built by dune, not CMake** (`lib/ldd/dune`): the C files of Sylvan's
`src/` except `sylvan_gmp.c` (needs GMP) and `sylvan_obj.cpp` (C++), and
Lace's `src/lace.c`, compiled into this library (`-O2 -std=gnu11
-D_GNU_SOURCE`, warnings off). A separate C library does not work: Rocq
loads the plugin's libraries as `.cmxs` files, and a C archive's objects
reach a `.cmxs` only when its own link refers to them. `vendor/dune` keeps
dune from reading the submodules as part of the build.

**Written here**, in place of CMake's `configure_file` (`config/`):
- `sylvan_config.h`: version 1.0.0, statistics off, `mmap` on, 64-byte
  cache lines;
- `lace_config.h`: timing and counters off, backoff on, no thread
  pinning (`LACE_PIN_OFF`, as with 1.7 and no `hwloc`), worker stacks of
  16 to 64MB (`LACE_DONT_OVERCOMMIT`, close to 1.7's 16MB, rather than a
  1GB reservation each), `sched_getaffinity` available.

**Updating:**
1. In the fork, pull upstream (`git pull https://github.com/trolando/sylvan
   master`), push.
2. In `vendor/sylvan`, check out the new commit; in `vendor/lace`, the
   commit the new Sylvan pins (its `CMakeLists.txt`, `FetchContent`).
3. Compare `config/` with the new `sylvan_config.h.in` and
   `lace_config.h.in`, and the C file list in `lib/ldd/dune` with `src/`.
4. Rebuild, `dune exec test/ldd_tests.exe`, `bench/prooflog.py`; commit the
   two submodule pointers.

Sylvan and Lace are under the Apache License 2.0 (their repositories'
`LICENSE` files).
