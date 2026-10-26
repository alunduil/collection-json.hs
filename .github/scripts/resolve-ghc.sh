#!/usr/bin/env bash
# Prints the newest GHC release in the major series given as $1 (such as
# 9.12), read from the metadata ghcup installs from. An exact version
# passes through unchanged. Fails when the series has no release.
set -euo pipefail

if [[ ! $1 =~ ^[0-9]+\.[0-9]+$ ]]; then
  echo "$1"
  exit 0
fi

if ! curl -fsSL https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-0.1.0.yaml \
  | yq '.ghcupDownloads.GHC.toolVersions | keys | .[]' \
  | grep -E "^${1//./\\.}\.[0-9]+$" | sort -V | tail -n1 | grep .; then
  echo "no GHC release found in series $1" >&2
  exit 1
fi
