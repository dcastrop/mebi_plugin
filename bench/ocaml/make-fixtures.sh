#!/usr/bin/env bash
# bench/ocaml/make-fixtures.sh -- regenerate the OCaml benchmark's fixtures
# from the Rocq examples.
#
# usage: bench/ocaml/make-fixtures.sh [OUTDIR]
#   OUTDIR  where to write the fixtures. Default: bench/ocaml/fixtures,
#           whose old fixtures are replaced.
#
# For each source below, compiles a copy (fixtures.py prepare) in which every
# proof that runs MeBi Sim Begin is admitted and replaced by a MeBi Run Bisim
# on the same pair, with result dumps on; then turns each command's dumps
# into one fixture (fixtures.py convert). Needs a current `dune build`;
# compiles against it from a copy of examples/, as bench/proofs.sh does.
# Each file is memory-capped at 6G. The work directory is kept, and printed
# at the end.

. "$(dirname "$0")/../lib.sh"

out=${1:-$BENCH_ROOT/bench/ocaml/fixtures}
here=$BENCH_ROOT/bench/ocaml

# source (relative to examples/), tag. CCS/PluginProofs.v already has the
# Alternating Bit Protocol's pair (abp, var 20), so CCS/ABP*Proofs.v are not
# listed.
sources="Bisimilarity/Proc/Test1/PluginProofs.v proc-test1
Bisimilarity/Proc/Test2/PluginProofs.v proc-test2
Bisimilarity/Proc/Test3/PluginProofs.v proc-test3
Bisimilarity/CADP/Size1/MutualExclusion/PluginProofs.v cadp-mutex
Bisimilarity/CADP/Size1/Glued/PluginProofs.v cadp-glued
Bisimilarity/CADP/Size1/Glued/MutualExclusion/PluginProofs.v cadp-glued-mutex
Bisimilarity/CCS/PluginProofs.v ccs
Bisimilarity/CCS/LawProofs.v ccs-laws
Evaluation/Width.v eval-width
Evaluation/Layers.v eval-layers
Evaluation/Depth.v eval-depth"
deps="CCS.v Proc.v Bisimilarity/Proc/Test1/Terms.v Bisimilarity/Proc/Test2/Terms.v
      Bisimilarity/Proc/Test3/Terms.v CADP.v CADP_Glued.v
      Bisimilarity/CADP/Properties/MutualExclusion.v Bisimilarity/CADP/Size1/Terms.v
      Evaluation/Base.v"

bench_check_build
# A short directory of its own, not $MEBI_BENCH_DIR: a dump's file name
# holds the full path of the file that wrote it, and a long one is refused.
run=$(mktemp -d -t mebi-fx.XXXXXX)
bench_copy_examples "$run"
# shellcheck disable=SC2086 # word-splitting the list is intended
bench_compile_deps "$run" $deps
mkdir -p "$out"
rm -f "$out"/*.json

while read -r src tag; do
  dir="$run/work/$tag"
  mkdir -p "$dir"
  # the copy sits beside its source, so its module path resolves the same
  copy="$run/examples/${src%.v}_fixtures.v"
  python3 "$here/fixtures.py" prepare "$run/examples/$src" "$copy" "$dir/manifest.json" \
    || bench_die "cannot prepare $src"
  bench_rocq_argv "$run"
  (cd "$dir" && bench_capped 6G "$dir/log" "${BENCH_RC[@]}" -o "${copy%.v}.vo" "$copy")
  grep -q '^BENCH_EXIT 0$' "$dir/log" || bench_die "$src failed: see $dir/log"
  python3 "$here/fixtures.py" convert "$dir/_dumps" "$dir/manifest.json" "$tag" "$out" \
    || bench_die "cannot convert the dumps of $src"
done <<< "$sources"

echo "bench: work in $run" >&2
echo "bench: $(ls "$out" | wc -l) fixtures in $out ($(du -sh "$out" | cut -f1))" >&2
