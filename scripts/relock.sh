#!/usr/bin/env bash
# Regenerate rocq-mebi.opam (from dune-project) and rocq-mebi.opam.locked
# (from the packages installed in the local switch). Run after changing
# dune-project's (depends ...) and installing what it asks for.
#
# opam lock gives each locked package a single filter. A package needed both
# by the doc tooling (with-doc) and by the editor tooling (with-dev-setup)
# -- cmdliner, astring, ... -- is filed under with-doc only, so a
# `--with-dev-setup` install would leave it unpinned and skip odoc. Every
# with-doc package is wanted for dev setup too (odoc is declared for both in
# dune-project), so the filter is widened afterwards.
#
# opam lock also drops a filter's packages silently unless *all* of them are
# installed: install the dev setup first
# (`opam install . --deps-only --with-dev-setup --with-doc`).
set -euo pipefail
cd "$(dirname "$0")/.."
dune build rocq-mebi.opam
opam lock .
sed -i -E 's/ & with-doc\}/ \& (with-doc | with-dev-setup)}/' rocq-mebi.opam.locked
# the lock must describe this switch exactly: nothing to install or change
if opam install . --locked --deps-only --with-dev-setup --with-doc \
     --show-actions 2>&1 | grep -q 'would be performed'; then
  echo "relock: the lock does not match the switch:" >&2
  opam install . --locked --deps-only --with-dev-setup --with-doc --show-actions >&2
  exit 1
fi
echo "relock: rocq-mebi.opam and rocq-mebi.opam.locked regenerated"
