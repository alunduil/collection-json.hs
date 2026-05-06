# Contributing to collection-json

Issues, pull requests, and docs improvements are welcome. This file
covers what you need to build, test, and open a PR: commands, branch
policy, release model.

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

- Keep PRs focused. Unrelated cleanup goes in a separate PR.
- The full CI matrix (`cabal build`, `cabal test`, `cabal check`,
  `cabal haddock`, `fourmolu --mode check`, `hlint`) must pass before
  merge. There are no manual overrides.

Don't bump the `version:` field in `collection-json.cabal` in your
PR; releases are cut by the maintainer.

## Reporting bugs and proposing changes

File an issue on [GitHub](https://github.com/alunduil/collection-json.hs/issues).
For bugs, include the GHC version, cabal version, and a minimal
repro. For proposals, describe the user-visible outcome and what
problem it solves.

## Attribution

Contributors are listed in [`COPYRIGHT`](COPYRIGHT). Add yourself in
the same PR as your first contribution if you want attribution.
