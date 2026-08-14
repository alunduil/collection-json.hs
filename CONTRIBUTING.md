# Contributing to collection-json

Issues, questions, pull requests, and docs improvements are welcome.
This file covers what you need to build, test, and open a PR:
commands, branch policy, and the conventions a review expects.

By participating you agree to the [Code of Conduct](CODE_OF_CONDUCT.md).

## Your first contribution

Issues tagged [`good first issue`][gfi] are scoped small and don't
assume prior context on the codebase; [`help wanted`][hw] marks
anything else open to a contributor. Neither is a prerequisite. An
unlabelled issue you want to take is fine, as is a PR for something
not yet filed.

## Getting started

Prerequisites: GHC and `cabal-install`. The supported GHC range is
declared in [`tested-with`](collection-json.cabal); CI runs the
matrix on every PR.

```sh
cabal update    # first checkout, or after a gap between contributions
cabal build
cabal test
```

`cabal update` refreshes the Hackage index. Skipping it on a stale
index is the usual cause of a dependency resolution failure that looks
like a broken build.

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

Fork the repository, branch from `main`, and open the PR against
`main`.

- Open it as a draft and mark it ready once CI is green and you want
  review. A draft says "still moving" and won't be reviewed until you
  promote it.
- Keep PRs focused. Unrelated cleanup goes in a separate PR.
- The full CI matrix (`cabal build`, `cabal test`, `cabal check`,
  `cabal haddock`, plus the `Pre-commit` workflow) must pass before
  merge. There are no manual overrides.

Don't bump the `version:` field in `collection-json.cabal` in your
PR; releases are cut by the maintainer.

### Commit messages

PRs are squash-merged, so the PR title becomes the subject of the
single commit that lands on `main`, and the PR description becomes its
body. Write the title as an imperative summary under 50 characters:
`Accept URI references for href`, not `fixed the href parsing bug`.
Use the description for why the change is needed and anything a reader
of the diff would find surprising.

Commits within the PR are collapsed on merge, so their messages are
review aids rather than permanent history. Keep them legible enough to
review commit by commit.

## Reporting bugs, asking questions, proposing changes

File an issue on [GitHub](https://github.com/alunduil/collection-json.hs/issues).
For bugs, include the GHC version, cabal version, and a minimal
repro. For proposals, describe the user-visible outcome and what
problem it solves. Questions about using the library are welcome as
issues too; tag them [`question`][question] so they're distinguishable
from defect reports.

## Attribution

Contributors are listed in [`COPYRIGHT`](COPYRIGHT). Add yourself in
the same PR as your first contribution if you want attribution.

[gfi]: https://github.com/alunduil/collection-json.hs/labels/good%20first%20issue
[hw]: https://github.com/alunduil/collection-json.hs/labels/help%20wanted
[question]: https://github.com/alunduil/collection-json.hs/labels/question
