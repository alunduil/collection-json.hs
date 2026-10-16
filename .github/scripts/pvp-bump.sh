#!/usr/bin/env bash
# Prints the PVP bump (A.B, C, or D) from the latest version Hackage
# published to the working tree, then the interface diff it came from:
# removed lines prefixed "- ", added lines "+ ".
#
# The published interface is Hackage's Hoogle file rather than a rebuild:
# old releases stop building as their dependencies move on.
set -euo pipefail

readonly HACKAGE="https://hackage.haskell.org"

cabal_file=$(ls -- *.cabal)
package=$(sed -n 's/^name:[[:space:]]*//p' "$cabal_file" | tr -d '[:space:]')

# Module qualification comes off instance heads because it names the
# defining package's internal module layout, which moves between
# compiler and library versions without any consumer seeing it.
strip_qualification() {
  sed -E "/^instance /s/([A-Z][A-Za-z0-9_']*\.)+//g"
}

normalise() {
  grep -vE '^(--|@version )' "$1" |
    sed '/^[[:space:]]*$/d' |
    strip_qualification |
    LC_ALL=C sort
}

# An instance is local when its head, context aside, names a class or
# type from one of this package's modules. Matching on qualified names
# keeps an external type that shares a local type's name foreign.
foreign_instances() {
  local modules local_name line
  modules=$(sed -n 's/^module //p' "$1" | sed 's/\./\\./g' | paste -sd '|')
  local_name="(^|[^A-Za-z0-9_'.])($modules)\.[A-Z][A-Za-z0-9_']*([^A-Za-z0-9_'.]|$)"

  grep '^instance ' "$1" |
    while IFS= read -r line; do
      if ! sed -E 's/^instance .*=> //' <<<"$line" | grep -qE "$local_name"; then
        strip_qualification <<<"$line"
      fi
    done |
    LC_ALL=C sort -u
}

published=$(curl -fsSL -H 'Accept: application/json' \
  "$HACKAGE/package/$package/preferred" |
  jq -er '."normal-version"[0]')

workdir=$(mktemp -d)
trap 'rm -rf "$workdir"' EXIT

if ! curl -fsSL -o "$workdir/published.txt" \
  "$HACKAGE/package/$package-$published/docs/$package.txt"; then
  echo "Hackage has no Hoogle docs for $package-$published" >&2
  exit 1
fi

cabal haddock --haddock-hoogle >&2
head_hoogle=$(find dist-newstyle -path "*/doc/html/$package/$package.txt" -print -quit)
if [ -z "$head_hoogle" ]; then
  echo "cabal haddock produced no Hoogle file for $package" >&2
  exit 1
fi

normalise "$workdir/published.txt" >"$workdir/published"
normalise "$head_hoogle" >"$workdir/head"
LC_ALL=C comm -23 "$workdir/published" "$workdir/head" >"$workdir/removed"
LC_ALL=C comm -13 "$workdir/published" "$workdir/head" >"$workdir/added"

# PVP: a removal breaks importers, and so does a new orphan instance,
# which can collide with one a consumer already defines.
if [ -s "$workdir/removed" ] ||
  foreign_instances "$head_hoogle" | LC_ALL=C comm -12 - "$workdir/added" | grep -q .; then
  echo A.B
elif [ -s "$workdir/added" ]; then
  echo C
else
  echo D
fi

sed 's/^/- /' "$workdir/removed"
sed 's/^/+ /' "$workdir/added"
