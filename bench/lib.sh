# Shared helpers for the bench/ scripts. Sourced, not run.
#
# Everything compiles against the repository's dune build
# (_build/install/default/lib and _build/default/theories), so run
# `dune build` first. Examples are compiled from a copy, never in place,
# so a run leaves the source tree untouched.

set -u

# The repository root: the parent of this file's directory.
BENCH_ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

# bench_die MSG: print MSG to stderr and exit 1.
bench_die() {
  echo "bench: $*" >&2
  exit 1
}

# bench_check_build: exit with a hint unless the dune build the scripts
# compile against exists.
bench_check_build() {
  [ -d "$BENCH_ROOT/_build/install/default/lib/rocq-mebi" ] \
    && [ -d "$BENCH_ROOT/_build/default/theories" ] \
    || bench_die "no dune build found under $BENCH_ROOT/_build: run 'dune build' first"
}

# bench_run_dir TAG: create and print a fresh directory for one run, under
# $MEBI_BENCH_DIR if set, else under a new temporary directory. An existing
# directory for the same TAG is replaced.
bench_run_dir() {
  local base=${MEBI_BENCH_DIR:-}
  if [ -z "$base" ]; then base=$(mktemp -d -t mebi-bench.XXXXXX); fi
  local dir="$base/$1"
  rm -rf "$dir" && mkdir -p "$dir" || bench_die "cannot create $dir"
  echo "$dir"
}

# bench_copy_examples DIR: copy examples/ into DIR/examples, dropping any
# compiled files the source tree carries from an old in-tree build (they
# would fail with "inconsistent assumptions" against the dune build).
bench_copy_examples() {
  cp -r "$BENCH_ROOT/examples" "$1/examples"
  find "$1/examples" \( -name '*.vo' -o -name '*.vos' -o -name '*.vok' \
    -o -name '*.glob' -o -name '.*.aux' \) -delete
}

# bench_rocq_argv DIR: set the array BENCH_RC to the command that compiles
# against the dune build, with the examples copied into DIR as
# MEBI.Examples. An argument list, not a function, so that it can be handed
# to systemd-run (which cannot run shell functions).
bench_rocq_argv() {
  BENCH_RC=(env "OCAMLPATH=$BENCH_ROOT/_build/install/default/lib" rocq compile
    -R "$BENCH_ROOT/_build/default/theories" MEBI
    -R "$1/examples" MEBI.Examples)
}

# bench_rocq DIR ARGS...: run that command on ARGS.
bench_rocq() {
  local dir=$1
  shift
  bench_rocq_argv "$dir"
  "${BENCH_RC[@]}" "$@"
}

# bench_compile_deps DIR FILE...: compile each example FILE (relative to
# DIR/examples) in order, quietly; exit naming the first that fails.
bench_compile_deps() {
  local dir=$1 f
  shift
  for f in "$@"; do
    bench_rocq "$dir" "$dir/examples/$f" > /dev/null 2>&1 \
      || bench_die "dependency failed to compile: examples/$f"
  done
}

# bench_force_strategy FILE MODE: for MODE True or False, make the copied
# proof FILE run under [MeBi Config Solver MutualCofix MODE] throughout:
# drop the file's own MutualCofix settings and set MODE right after it
# loads the plugin. MODE auto leaves FILE as it is. (Only a bare [MeBi
# Config Reset] would undo this, and no proof file uses one.)
bench_force_strategy() {
  local file=$1 mode=$2
  case $mode in
    auto) return 0 ;;
    True | False) ;;
    *) bench_die "unknown solver mode '$mode' (expected auto, True or False)" ;;
  esac
  grep -q '^Require Import MEBI.loader.' "$file" \
    || bench_die "$file does not load MEBI.loader, cannot force the strategy"
  sed -i -e '/MeBi Config Solver MutualCofix/d' \
    -e "s/^Require Import MEBI.loader.$/&\nMeBi Config Solver MutualCofix $mode./" "$file"
}

# bench_capped MEM LOG CMD...: run CMD in a memory-capped scope (MemoryMax
# MEM, no swap), its output to LOG, then append the peak memory, wall time
# and exit status as BENCH_ lines. `ulimit -v` cannot be used instead: OCaml
# 5 then fails to reserve its heaps.
bench_capped() {
  local mem=$1 log=$2
  shift 2
  systemd-run --user --scope -q -p MemoryMax="$mem" -p MemorySwapMax=0 \
    /usr/bin/time -f 'BENCH_MAXRSS_KB %M BENCH_SECONDS %e' "$@" > "$log" 2>&1
  echo "BENCH_EXIT $?" >> "$log"
}

# bench_rows SUITE MODE NAME LOG SOLVES RUNS: from one proof file's LOG,
# append a row per [MeBi Sim Solve] to SOLVES (suite, mode, file, index,
# Solved/Unsolved, iterations) and one row for the file to RUNS (suite,
# mode, file, exit, seconds, peak MB, errors).
bench_rows() {
  local suite=$1 mode=$2 name=$3 log=$4 solves=$5 runs=$6
  grep -oE '(Uns|S)olved after [0-9]+' "$log" \
    | awk -v s="$suite" -v m="$mode" -v f="$name" \
      '{ print s "\t" m "\t" f "\t" NR "\t" $1 "\t" $3 }' >> "$solves"
  local exit secs kb errs
  exit=$(sed -n 's/^BENCH_EXIT //p' "$log" | tail -1)
  secs=$(sed -n 's/.*BENCH_SECONDS \([0-9.]*\).*/\1/p' "$log" | tail -1)
  kb=$(sed -n 's/.*BENCH_MAXRSS_KB \([0-9]*\).*/\1/p' "$log" | tail -1)
  errs=$(grep -c '^Error' "$log")
  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$suite" "$mode" "$name" \
    "${exit:-?}" "${secs:-?}" "$(( ${kb:-0} / 1024 ))" "$errs" >> "$runs"
}
