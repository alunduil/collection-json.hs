#!/usr/bin/env bash
# Reads a cabal file on stdin and prints the GHC major series its
# tested-with declares, one per line in version order. Fails when it
# declares none.
set -euo pipefail

if ! grep -oE 'GHC == [0-9]+\.[0-9]+' | grep -oE '[0-9]+\.[0-9]+' | sort -V; then
  echo "no tested-with GHC versions found" >&2
  exit 1
fi
