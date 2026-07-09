# Contributing to collection-json

Issues, pull requests, and docs improvements are welcome. This file
covers what you need to build, test, and open a PR: commands, branch
policy, release model.

By participating you agree to the [Code of Conduct](CODE_OF_CONDUCT.md).

## Getting started

Prerequisites: GHC and `cabal-install`. The supported GHC range is
declared in [`tested-with`](collection-json.cabal); CI runs the
matrix on every PR.

```sh
cabal build
cabal test
```

## Formatting and linting

Formatting (Fourmolu), Haskell linting (HLint), Markdown linting
(markdownlint-cli2), and prose linting (Vale) run through
[pre-commit](https://pre-commit.com). The `Pre-commit` workflow runs
the same hooks on every push and PR; merges are blocked until they
pass.

```sh
pre-commit install            # install the git hook (one-off)
pre-commit run --all-files    # run all hooks against the repo
pre-commit autoupdate         # bump third-party hook revs
```

The Haskell hooks shell out to `fourmolu` and `hlint` from `PATH`,
so install them locally (`cabal install fourmolu hlint`, or via
`ghcup`). The pinned versions used by CI live at the top of
[`.github/workflows/pre-commit.yml`](.github/workflows/pre-commit.yml).
Vale, by contrast, is built and its styles fetched by pre-commit, so
it needs no local install.

## Branch policy

Trunk-based. `main` is the only long-lived branch and the merge
target for every PR. Topic branches are short-lived and deleted on
merge.

## Pull requests

- Keep PRs focused. Unrelated cleanup goes in a separate PR.
- The full CI matrix (`cabal build`, `cabal test`, `cabal check`,
  `cabal haddock`, plus the `Pre-commit` workflow) must pass before
  merge. There are no manual overrides.

Don't bump the `version:` field in `collection-json.cabal` in your
PR; releases are cut by the maintainer.

## Release cadence

Cut a Hackage release whenever a patch or minor's worth of merged
changes has accumulated—dependency bumps included. Don't hold
changes back to batch them; tag and ship. Releases are
[tag-driven](.github/workflows/release.yml), so the `version:` bump
and the dated [`CHANGELOG.md`](CHANGELOG.md) entry belong to the
release cut, not to feature PRs.

## Reporting bugs and proposing changes

File an issue on [GitHub](https://github.com/alunduil/collection-json.hs/issues).
For bugs, include the GHC version, cabal version, and a minimal
repro. For proposals, describe the user-visible outcome and what
problem it solves.

## Attribution

Contributors are listed in [`COPYRIGHT`](COPYRIGHT). Add yourself in
the same PR as your first contribution if you want attribution.
