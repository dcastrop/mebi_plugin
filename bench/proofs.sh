#!/usr/bin/env bash
# bench/proofs.sh -- run the plugin's proof suites and tabulate the results.
#
# usage: bench/proofs.sh [-s SUITE] [-f FILES] [-m MODES] [-j JOBS] [-t TAG]
#   -s SUITE  default (the six PluginProofs.v marked ### Success, CCS's
#             PluginProofs.v and LawProofs.v; ~3 min a mode), abp (the
#             Alternating Bit Protocol proofs, ~4 min and ~4.3GB each),
#             test4 (Test4 through the normalised semantics, ~11 min,
#             ~3.8GB peak), eval (the evaluation levers in
#             examples/Evaluation as built by default, ~40s) or eval-large
#             (the same levers scaled up). Default: default.
#   -f FILES  comma-separated subset of the suite's proof files to run,
#             as listed below (e.g. -s eval-large -f WidthLarge.v).
#             Default: all of them.
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
#   solves.tsv  suite, mode, file, index, Solved/Unsolved, iterations,
#               then the proof's name and the seconds its MeBi Sim Begin,
#               MeBi Sim Solve and Qed took (from rocq compile -time; "?"
#               where a command did not finish) -- one row per MeBi Sim
#               Solve, in file order;
#   runs.tsv    suite, mode, file, exit, seconds, peak MB, errors -- one row
#               per file;
#   logs/       the full output of each file.
# Compare two runs with bench/compare.sh. The known-good iteration counts
# are in CLAUDE.md.

. "$(dirname "$0")/lib.sh"

suite=default only= modes=auto jobs=8 tag=
while getopts "s:f:m:j:t:h" opt; do
  case $opt in
    s) suite=$OPTARG ;;
    f) only=$OPTARG ;;
    m) modes=$OPTARG ;;
    j) jobs=$OPTARG ;;
    t) tag=$OPTARG ;;
    *) sed -n '2,/^$/s/^# \{0,1\}//p' "$0"; exit 2 ;;
  esac
done

# Per suite: the examples to compile first (in order), the directory under
# examples/ holding its proof files, the proof files (relative to it), the
# memory cap, the parallelism.
dir=Bisimilarity
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
  eval)
    deps="Evaluation/Base.v" dir=Evaluation
    files="Width.v Layers.v Depth.v"
    mem=5G ;;
  eval-large)
    deps="Evaluation/Base.v Evaluation/Layers.v Evaluation/Depth.v" dir=Evaluation
    files="WidthLarge.v LayersLarge.v DepthLarge.v"
    mem=6G jobs=1 ;;
  *) bench_die "unknown suite '$suite' (expected default, abp, test4, eval or eval-large)" ;;
esac

if [ -n "$only" ]; then
  for f in ${only//,/ }; do
    case " $(echo $files) " in
      *" $f "*) ;;
      *) bench_die "$f is not in suite '$suite' (its files: $(echo $files))" ;;
    esac
  done
  files=${only//,/ }
fi

bench_check_build
run=$(bench_run_dir "${tag:-$suite-$(date +%Y%m%d-%H%M%S)}")
mkdir -p "$run/logs"
bench_copy_examples "$run"
# shellcheck disable=SC2086 # word-splitting the lists is intended
bench_compile_deps "$run" $deps
printf 'suite\tmode\tfile\tindex\tstatus\titerations\tproof\tbegin_s\tsolve_s\tqed_s\n' > "$run/solves.tsv"
printf 'suite\tmode\tfile\texit\tseconds\tpeak_mb\terrors\n' > "$run/runs.tsv"

# run_one MODE FILE: compile one proof FILE under MODE from its own copy
# (forcing the strategy edits the file), memory-capped, logging to logs/.
run_one() {
  local mode=$1 file=$2
  local name=${file//\//_}
  local src="$run/examples/$dir/$file"
  local copy="${src%.v}_bench_$mode.v"
  # the copy must keep a valid module name: no dots or dashes
  cp "$src" "$copy"
  bench_force_strategy "$copy" "$mode"
  bench_rocq_argv "$run"
  bench_capped "$mem" "$run/logs/$mode.${name%.v}.log" \
    "${BENCH_RC[@]}" -time -o "${copy%.v}.vo" "$copy"
}
export -f run_one bench_rocq_argv bench_capped bench_force_strategy bench_die
export run mem dir BENCH_ROOT

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
