# Contributing

Thanks for your interest in `collection-json`. This file documents the local
checks CI enforces; everything else lives in the issue/PR you are working on.

## Pre-commit

Formatting and linting run through [pre-commit](https://pre-commit.com).
The `Pre-commit` workflow (`.github/workflows/pre-commit.yml`) runs the same
hooks on every push and PR; merges are blocked until they pass.

```sh
pre-commit install                # install the git hook (one-off)
pre-commit run --all-files        # run all hooks against the repo
pre-commit autoupdate             # bump third-party hook revs
```

The Haskell hooks shell out to `fourmolu` and `hlint` from `PATH`, so install
them locally (e.g. `cabal install fourmolu hlint` or via `ghcup`). The pinned
versions used by CI live at the top of the Pre-commit workflow.

## Configuration

- `fourmolu.yaml` — formatter config (indent=2, leading commas).
- `hlint.yaml` — linter config.
- `.pre-commit-config.yaml` — hook list and third-party revs.
