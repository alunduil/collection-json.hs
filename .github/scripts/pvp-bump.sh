#!/usr/bin/env bash
# Prints the PVP bump (A.B, C, or D) from Hackage's latest release to the
# working tree, then the interface diff behind it: removed lines prefixed
# "- ", added lines "+ ".
#
# The verdict covers the interface alone. Behaviour changes behind an
# unchanged signature, and orphan instances a dependency adds, never
# reach it.
#
# Old releases stop building as their dependencies move on, so the
# published interface is Hackage's Hoogle file.
set -euo pipefail

readonly HACKAGE="https://hackage.haskell.org"
readonly CAPITALISED_NAME="[A-Z][A-Za-z0-9_']*"

package=$(sed -n 's/^name:[[:space:]]*//p' -- *.cabal | tr -d '[:space:]')

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

build_working_hoogle() {
  local out=$1 hoogle
  cabal haddock --haddock-hoogle >&2
  hoogle=$(find dist-newstyle -path "*/doc/html/$package/$package.txt" -print -quit)
  if [ -z "$hoogle" ]; then
    echo "cabal haddock produced no Hoogle file for $package" >&2
    return 1
  fi
  cp -- "$hoogle" "$out"
}

# Qualification names the defining package's internal module layout,
# which moves between compiler and library versions unseen by consumers.
strip_qualification() {
  sed -E "/^instance /s/($CAPITALISED_NAME\.)+//g"
}

readonly DEPRECATION='-- [|] <i>(Deprecated|Warning): '

# Haddock renders DEPRECATED and WARNING pragmas as a doc comment, possibly
# wrapped, directly above the declaration they mark. Normalising drops
# comments, and a warning apart from its declaration names no entity.
fold_deprecations() {
  awk -v deprecation="^$DEPRECATION" '
    $0 ~ deprecation { pending = $0; wrapped = $0 !~ /<\/i>$/; next }
    wrapped { sub(/^--[[:space:]]*/, ""); pending = pending " " $0; wrapped = $0 !~ /<\/i>$/; next }
    pending != "" && /^[^-[:space:]]/ { print $0 " " pending; pending = ""; next }
    { print }
  ' "$1"
}

normalise() {
  fold_deprecations "$1" |
    grep -vE '^(--|@version )' |
    sed '/^[[:space:]]*$/d' |
    strip_qualification |
    LC_ALL=C sort
}

# Only qualified names tell a local type from an external one of the same
# name.
foreign_instances() {
  local modules local_name
  modules=$(sed -n 's/^module //p' "$1" | sed 's/\./\\./g' | paste -sd '|')
  local_name="(^|[^A-Za-z0-9_'.])($modules)\.$CAPITALISED_NAME([^A-Za-z0-9_'.]|$)"

  awk -v local_name="$local_name" '
    /^instance / { head = $0; sub(/^instance .*=> /, "", head); if (head !~ local_name) print }
  ' "$1" |
    strip_qualification |
    LC_ALL=C sort -u
}

# A new orphan instance can collide with one a consumer already defines.
adds_orphan() {
  local hoogle=$1 added=$2
  foreign_instances "$hoogle" | LC_ALL=C comm -12 - "$added" | grep -q .
}

# A new deprecation breaks consumers that build with -Werror.
adds_deprecation() {
  grep -qE " $DEPRECATION" "$1"
}

# Dropping only the pragma leaves the entity exported.
removes_entity() {
  local removed=$1 added=$2
  sed -E "s/ $DEPRECATION.*//" "$removed" | LC_ALL=C sort -u |
    LC_ALL=C comm -23 - "$added" | grep -q .
}

classify_bump() {
  local hoogle=$1 removed=$2 added=$3
  if removes_entity "$removed" "$added" ||
    adds_orphan "$hoogle" "$added" ||
    adds_deprecation "$added"; then
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
build_working_hoogle "$workdir/working.txt"

normalise "$workdir/published.txt" >"$workdir/published"
normalise "$workdir/working.txt" >"$workdir/working"
LC_ALL=C comm -23 "$workdir/published" "$workdir/working" >"$workdir/removed"
LC_ALL=C comm -13 "$workdir/published" "$workdir/working" >"$workdir/added"

classify_bump "$workdir/working.txt" "$workdir/removed" "$workdir/added"
sed 's/^/- /' "$workdir/removed"
sed 's/^/+ /' "$workdir/added"
