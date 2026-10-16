#!/usr/bin/env bash
# bench/compare.sh -- compare two bench/proofs.sh runs.
#
# usage: bench/compare.sh RUN_A RUN_B
#   RUN_A, RUN_B: run directories printed by bench/proofs.sh (each holding
#   solves.tsv and runs.tsv), e.g. one built from main and one from a
#   branch.
#
# Prints every MeBi Sim Solve whose status or iteration count differs (a
# change of status is a regression; a change of count needs explaining, see
# CLAUDE.md), every Solve present in only one run, and each file's time and
# peak memory side by side. Exits 1 if any Solve differs, else 0.

. "$(dirname "$0")/lib.sh"

[ $# -eq 2 ] || { sed -n '2,/^$/s/^# \{0,1\}//p' "$0"; exit 2; }
a=$1 b=$2
for d in "$a" "$b"; do
  [ -f "$d/solves.tsv" ] && [ -f "$d/runs.tsv" ] \
    || bench_die "$d is not a bench/proofs.sh run directory"
done

# Solves, keyed by suite, mode, file and index.
awk -F'\t' '
  FNR == 1 { next }
  { key = $1 FS $2 FS $3 FS $4; val = $5 " " $6 }
  NR == FNR { A[key] = val; next }
  { B[key] = val }
  END {
    diff = 0
    for (k in A) {
      if (!(k in B)) { print "only in A: " k "  " A[k]; diff = 1 }
      else if (A[k] != B[k]) { print "DIFFERS:   " k "  " A[k] " -> " B[k]; diff = 1 }
    }
    for (k in B) if (!(k in A)) { print "only in B: " k "  " B[k]; diff = 1 }
    if (!diff) print "Every Solve identical."
    exit diff
  }' "$a/solves.tsv" "$b/solves.tsv"
status=$?

# Per file: seconds and peak MB, A then B.
echo
awk -F'\t' '
  FNR == 1 { next }
  { key = $1 "\t" $2 "\t" $3 }
  NR == FNR { A[key] = $5 "\t" $6; next }
  { print key "\t" ((key in A) ? A[key] : "-\t-") "\t" $5 "\t" $6 }
' "$a/runs.tsv" "$b/runs.tsv" \
  | (printf 'suite\tmode\tfile\tA_s\tA_MB\tB_s\tB_MB\n'; cat) | column -t -s $'\t'

exit $status
