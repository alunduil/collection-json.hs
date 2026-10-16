#!/usr/bin/env bash
# Prints the PVP bump (A.B, C, or D) from Hackage's latest release to the
# working tree, then the interface diff behind it: removed lines prefixed
# "- ", added lines "+ ".
#
# Old releases stop building as their dependencies move on, so the
# published interface is Hackage's Hoogle file.
set -euo pipefail

readonly HACKAGE="https://hackage.haskell.org"

cabal_file=$(ls -- *.cabal)
package=$(sed -n 's/^name:[[:space:]]*//p' "$cabal_file" | tr -d '[:space:]')

published_version() {
  curl -fsSL -H 'Accept: application/json' \
    "$HACKAGE/package/$package/preferred" |
    jq -er '."normal-version"[0]'
}

download_published_hoogle() {
  local version=$1 out=$2
  if ! curl -fsSL -o "$out" "$HACKAGE/package/$package-$version/docs/$package.txt"; then
    echo "Hackage has no Hoogle docs for $package-$version" >&2
    return 1
  fi
}

build_head_hoogle() {
  local hoogle
  cabal haddock --haddock-hoogle >&2
  hoogle=$(find dist-newstyle -path "*/doc/html/$package/$package.txt" -print -quit)
  if [ -z "$hoogle" ]; then
    echo "cabal haddock produced no Hoogle file for $package" >&2
    return 1
  fi
  printf '%s\n' "$hoogle"
}

# Qualification names the defining package's internal module layout,
# which moves between compiler and library versions unseen by consumers.
strip_qualification() {
  sed -E "/^instance /s/([A-Z][A-Za-z0-9_']*\.)+//g"
}

normalise() {
  grep -vE '^(--|@version )' "$1" |
    sed '/^[[:space:]]*$/d' |
    strip_qualification |
    LC_ALL=C sort
}

# Matching qualified names keeps an external type that shares a local
# type's name foreign.
foreign_instances() {
  local modules local_name
  modules=$(sed -n 's/^module //p' "$1" | sed 's/\./\\./g' | paste -sd '|')
  local_name="(^|[^A-Za-z0-9_'.])($modules)\.[A-Z][A-Za-z0-9_']*([^A-Za-z0-9_'.]|$)"

  awk -v local_name="$local_name" '
    /^instance / { head = $0; sub(/^instance .*=> /, "", head); if (head !~ local_name) print }
  ' "$1" |
    strip_qualification |
    LC_ALL=C sort -u
}

adds_orphan() {
  local hoogle=$1 added=$2
  foreign_instances "$hoogle" | LC_ALL=C comm -12 - "$added" | grep -q .
}

# A new orphan instance can collide with one a consumer already defines.
classify_bump() {
  local hoogle=$1 removed=$2 added=$3
  if [ -s "$removed" ] || adds_orphan "$hoogle" "$added"; then
    echo A.B
  elif [ -s "$added" ]; then
    echo C
  else
    echo D
  fi
}

workdir=$(mktemp -d)
trap 'rm -rf "$workdir"' EXIT

published=$(published_version)
download_published_hoogle "$published" "$workdir/published.txt"
head_hoogle=$(build_head_hoogle)

normalise "$workdir/published.txt" >"$workdir/published"
normalise "$head_hoogle" >"$workdir/head"
LC_ALL=C comm -23 "$workdir/published" "$workdir/head" >"$workdir/removed"
LC_ALL=C comm -13 "$workdir/published" "$workdir/head" >"$workdir/added"

classify_bump "$head_hoogle" "$workdir/removed" "$workdir/added"
sed 's/^/- /' "$workdir/removed"
sed 's/^/+ /' "$workdir/added"
