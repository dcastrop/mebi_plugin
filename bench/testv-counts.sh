#!/usr/bin/env bash
# bench/testv-counts.sh -- list theories/Test.v's proof-solver counts.
#
# usage: bench/testv-counts.sh [TREE]
#   TREE: a checkout of the repository with its own dune build (default:
#   this one). Use a `git worktree` of main to compare a branch against it.
#
# Compiles a copy of TREE's theories/Test.v against TREE's build, with
# Notice output switched on so every MeBi Sim Solve reports its count, and
# prints one line per Solve: the Test.v section it is in and the count.
# Errors print as "ERROR <section>". Diff two outputs to compare trees.

set -u
tree=$(cd "${1:-$(dirname "$0")/..}" && pwd)
[ -d "$tree/_build/default/theories" ] || { echo "no dune build in $tree" >&2; exit 1; }
dir=$(mktemp -d -t mebi-testv.XXXXXX)
sed 's/Output "Notice" False/Output "Notice" True/' "$tree/theories/Test.v" > "$dir/Test.v"
cd "$dir" || exit 1
OCAMLPATH="$tree/_build/install/default/lib" rocq compile \
  -R "$tree/_build/default/theories" MEBI -R . TV Test.v 2>&1 \
  | awk '/^Theories\.Test\./ { sec = $1 }
         /Solved after|Unsolved after/ { print sec, $0 }
         /^Error/ { print "ERROR", sec }'
rm -rf "$dir"
