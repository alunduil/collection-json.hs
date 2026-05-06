# Contributing

Thanks for your interest in `collection-json`. This file documents the local
checks CI enforces; everything else lives in the issue/PR you are working on.

## Formatting

Sources are formatted with [Fourmolu](https://github.com/fourmolu/fourmolu).
The pinned config is `fourmolu.yaml` at the repo root.

```sh
fourmolu --mode check src test   # what CI runs
fourmolu --mode inplace src test # apply fixes locally
```

## Linting

Sources are linted with [HLint](https://github.com/ndmitchell/hlint). The
project rules live in `hlint.yaml` at the repo root.

```sh
hlint src test
```

Both checks run in the `format` job of `.github/workflows/ci.yml` and must
pass before a PR can merge.
