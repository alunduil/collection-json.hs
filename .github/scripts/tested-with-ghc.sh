#!/usr/bin/env bash
# Prints the GHC major series a cabal file's tested-with declares, one per
# line in version order. Reads the cabal file on stdin.
set -euo pipefail

if ! grep -oE 'GHC == [0-9]+\.[0-9]+' | grep -oE '[0-9]+\.[0-9]+' | sort -V; then
  echo "no tested-with GHC versions found" >&2
  exit 1
fi
