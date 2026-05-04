# Contributing to collection-json

Issues, pull requests, and docs improvements are all welcome. This
file is the cold-clone briefing: build, test, branch policy, release
model.

By participating you agree to the [Code of Conduct](CODE_OF_CONDUCT.md).

## Getting started

Prerequisites: GHC and `cabal-install`. The supported GHC range is
declared in [`tested-with`](collection-json.cabal); CI runs the
matrix on every PR.

```
cabal build
cabal test
```

Formatting and linting are enforced in CI:

```
fourmolu --mode check src test
hlint src test
```

Run `fourmolu --mode inplace src test` before pushing to keep the
`format` job green.

## Branch policy

Trunk-based. `main` is the only long-lived branch and the merge
target for every PR. Topic branches are short-lived and deleted on
merge.

## Pull requests

- Open every PR as a draft; promote to ready once CI is green and
  you want review.
- Keep PRs focused. Unrelated cleanup goes in a separate PR.
- The full CI matrix (`cabal build`, `cabal test`, `cabal check`,
  `cabal haddock`, `fourmolu --mode check`, `hlint`) must pass before
  merge. There are no manual overrides.

## Releases

Tag-driven. Pushing a `v*` tag (e.g. `v1.3.1.4`) triggers the release
workflow, which uploads the sdist and Haddock to Hackage as a
candidate; a manual `publish` job promotes the candidate to the
final release. The tag and the `version:` field in
`collection-json.cabal` must match — CI fails the release otherwise.

## PVP contract

This package follows the
[Haskell Package Versioning Policy](https://pvp.haskell.org/). For
version `A.B.C.D`:

- **D** (patch) — bug fixes, cabal metadata, dependency-bound
  widening. No change to the exported API.
- **C** (minor) — additions only. Every identifier exported in the
  previous `A.B.C` line must remain exported, with the same type, in
  `A.B.(C+1)`.
- **A.B** (major) — anything that removes, renames, or changes the
  signature of an exported identifier requires a major bump (either
  `A` or `B`).

When in doubt, bump higher. `cabal-diff` or `packdiff` against the
previously released sdist is the reference check before tagging a
non-major release.

## Reporting bugs and proposing changes

File an issue on [GitHub](https://github.com/alunduil/collection-json.hs/issues).
For bugs, include the GHC version, cabal version, and a minimal
repro. For proposals, describe the user-visible outcome and what
problem it solves.

## Attribution

Contributors are listed in [`COPYRIGHT`](COPYRIGHT). Add yourself in
the same PR as your first contribution if you want attribution.
