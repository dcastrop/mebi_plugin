#!/usr/bin/env bash
# bench/proofs.sh -- run the plugin's proof suites and tabulate the results.
#
# usage: bench/proofs.sh [-s SUITE] [-m MODES] [-j JOBS] [-t TAG]
#   -s SUITE  default (the six PluginProofs.v marked ### Success, CCS's
#             PluginProofs.v and LawProofs.v; ~3 min a mode), abp (the
#             Alternating Bit Protocol proofs, ~4 min and ~4.3GB each) or
#             test4 (Test4 through the normalised semantics, ~11 min,
#             ~3.8GB peak). Default: default.
#   -m MODES  comma-separated solver strategies: auto (each file as
#             written), True, False (MeBi Config Solver MutualCofix forced
#             for the whole file). Default: auto.
#   -j JOBS   files compiled at once (default suite only; abp and test4 run
#             one file at a time, as two heavy proofs together can exhaust
#             memory). Default: 8.
#   -t TAG    name of the run's directory. Default: SUITE-YYYYmmdd-HHMMSS.
#
# Each file is its own memory-capped process (5G, or 6G for abp/test4)
# with its own log, so every "Solved after N" line is attributable. The run
# directory ($MEBI_BENCH_DIR/TAG, or a fresh temporary directory) gets:
#   solves.tsv  suite, mode, file, index, Solved/Unsolved, iterations --
#               one row per MeBi Sim Solve, in file order;
#   runs.tsv    suite, mode, file, exit, seconds, peak MB, errors -- one row
#               per file;
#   logs/       the full output of each file.
# Compare two runs with bench/compare.sh. The known-good iteration counts
# are in CLAUDE.md.

. "$(dirname "$0")/lib.sh"

suite=default modes=auto jobs=8 tag=
while getopts "s:m:j:t:h" opt; do
  case $opt in
    s) suite=$OPTARG ;;
    m) modes=$OPTARG ;;
    j) jobs=$OPTARG ;;
    t) tag=$OPTARG ;;
    *) sed -n '2,/^$/s/^# \{0,1\}//p' "$0"; exit 2 ;;
  esac
done

# Per suite: the examples to compile first (in order), the proof files
# (relative to examples/Bisimilarity), the memory cap, the parallelism.
case $suite in
  default)
    deps="CCS.v Proc.v Bisimilarity/Proc/Test1/Terms.v Bisimilarity/Proc/Test2/Terms.v
          Bisimilarity/Proc/Test3/Terms.v CADP.v CADP_Glued.v
          Bisimilarity/CADP/Properties/MutualExclusion.v Bisimilarity/CADP/Size1/Terms.v"
    files="Proc/Test1/PluginProofs.v Proc/Test2/PluginProofs.v Proc/Test3/PluginProofs.v
           CADP/Size1/MutualExclusion/PluginProofs.v CADP/Size1/Glued/PluginProofs.v
           CADP/Size1/Glued/MutualExclusion/PluginProofs.v
           CCS/PluginProofs.v CCS/LawProofs.v"
    mem=5G ;;
  abp)
    deps="CCS.v"
    files="CCS/ABPProofs.v CCS/ABPBisimProofs.v"
    mem=6G jobs=1 ;;
  test4)
    deps="Proc.v ProcCongruence.v Bisimilarity/Proc/Test4/Terms.v"
    files="Proc/Test4/NormProofs.v Proc/Test4/NormBisimProofs.v"
    mem=6G jobs=1 ;;
  *) bench_die "unknown suite '$suite' (expected default, abp or test4)" ;;
esac

bench_check_build
run=$(bench_run_dir "${tag:-$suite-$(date +%Y%m%d-%H%M%S)}")
mkdir -p "$run/logs"
bench_copy_examples "$run"
# shellcheck disable=SC2086 # word-splitting the lists is intended
bench_compile_deps "$run" $deps
printf 'suite\tmode\tfile\tindex\tstatus\titerations\n' > "$run/solves.tsv"
printf 'suite\tmode\tfile\texit\tseconds\tpeak_mb\terrors\n' > "$run/runs.tsv"

# run_one MODE FILE: compile one proof FILE under MODE from its own copy
# (forcing the strategy edits the file), memory-capped, logging to logs/.
run_one() {
  local mode=$1 file=$2
  local name=${file//\//_}
  local src="$run/examples/Bisimilarity/$file"
  local copy="${src%.v}_bench_$mode.v"
  # the copy must keep a valid module name: no dots or dashes
  cp "$src" "$copy"
  bench_force_strategy "$copy" "$mode"
  bench_rocq_argv "$run"
  bench_capped "$mem" "$run/logs/$mode.${name%.v}.log" \
    "${BENCH_RC[@]}" -o "${copy%.v}.vo" "$copy"
}
export -f run_one bench_rocq_argv bench_capped bench_force_strategy bench_die
export run mem BENCH_ROOT

for mode in ${modes//,/ }; do
  echo "bench: $suite, mode $mode ..." >&2
  # shellcheck disable=SC2086
  printf '%s\n' $files | xargs -P "$jobs" -I{} bash -c 'run_one "$0" "$1"' "$mode" {}
  for file in $files; do
    name=${file//\//_}
    bench_rows "$suite" "$mode" "${file%.v}" "$run/logs/$mode.${name%.v}.log" \
      "$run/solves.tsv" "$run/runs.tsv"
  done
done

column -t -s $'\t' "$run/runs.tsv" >&2
echo "$run"
