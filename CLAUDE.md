# collection-json — agent guide

## Project summary

Haskell library: types, `FromJSON`/`ToJSON` instances, and
`FromCollection`/`ToCollection` classes for the
`application/vnd.collection+json` hypermedia type. Single exposed module
`Data.CollectionJSON`; the test tree under `test/` mirrors it. MIT.
Hackage: `collection-json`.

## Tooling inventory

Single-package project (`cabal.project`: `packages: .`).

- **Build / test**: `cabal update` (first run or after a gap), then
  `cabal build`, `cabal test` (hspec-discover, `*Spec.hs`). No Nix or
  direnv — plain `cabal`. Supported GHC: `ci.yml` tests 9.10/9.12/9.14;
  `tested-with:` in the cabal file lags at `9.10.*`, so trust the CI
  matrix and `build-depends` bounds — verify with `cabal build`, not
  metadata.
- **Lint / format**: `pre-commit` (`.pre-commit-config.yaml`) runs
  fourmolu (`fourmolu.yaml`), hlint (`hlint.yaml`), markdownlint
  (`.markdownlint.jsonc`), Vale (`.vale.ini`; styles vendored under
  `.vale/styles`, no `vale sync`). fourmolu/hlint run from PATH, pinned
  in `.github/workflows/pre-commit.yml` (not the pre-commit config).
- **CI**: `ci.yml` (GHC matrix above × ubuntu/macos, builds from the
  sdist; `coverage` job → Codecov, `codecov.yml`), `pre-commit.yml`,
  `links.yml` (lychee, `lychee.toml`), `ghc-matrix.yml` (monthly:
  files/refreshes a tracking issue when the matrix drifts from the GHC
  series endoflife.date marks supported).
- **Publish**: `.github/workflows/release.yml` — see Release process.
- **Deps / versioning**: `renovate.json`; Haskell PVP.

## Source of truth for behaviour

The Collection+JSON spec at <https://github.com/collection-json/spec>
is authoritative. The hspec suite is an attempt to encode that spec;
when the two disagree, the spec wins and the tests are wrong. Don't
shift semantics to make a test pass — re-derive from the spec, then fix
the test.

## Scope discipline

PVP-classified API surface makes out-of-scope edits expensive — an
incidental export change forces a major bump. Keep issue work tight:

- Before opening a PR, check the diff doesn't bleed into linked or
  sibling issues; revert incidental edits before review.
- Blocked by unshipped prerequisites: propose deferral with a
  `blocked-by` edge, don't write premature code.
- File unrelated problems found mid-task as their own issues.

The `issue-work` skill encodes the full flow; this is the reminder to
apply it.

## PVP obligations

Hackage requires PVP-compliant version bumps. Classify every change:

- **A.B (major)** — anything a downstream import could notice:
  removed/renamed export, changed signature, new method on
  `FromCollection`/`ToCollection`, removed constructor, narrowed
  constraint.
- **C (minor)** — purely additive: new exported function, new
  constructor, new instance for a type we own, new module.
- **D (patch)** — no API change visible to importers: widened
  dependency upper bounds, doc fixes, internal refactors,
  CI/packaging-only changes.

Upper bounds **are** API under PVP — widen them in a patch bump and
ship; don't sit on bumps to batch them.

## Release process

- `main` is the integration branch and PR target.
- Current version `1.3.1.3`; next `1.3.1.4`.
- Tag-driven via `.github/workflows/release.yml`: a `v*` tag on `main`
  uploads a Hackage candidate; a `workflow_dispatch` run publishes
  (gated by the `hackage` environment). Both check the tag matches
  `version:` in the cabal file and use the `HACKAGE_TOKEN` secret. Read
  `release.yml` before touching publish.

## Don't-touch list

- `Setup.hs` — one-line `defaultMain`; cabal boilerplate.
- `dist/`, `dist-newstyle/`, `.stack-work/`, `.cabal-sandbox/`, `*.hi`,
  `*.o` — gitignored build artefacts.
- `.vale/styles/` (except `config/`) — vendored by `vale sync`; our
  vocabulary lives in `.vale/styles/config`.
- `~/.cabal/packages/`, `~/.cabal/store/` — Hackage index and global
  build cache outside the repo. Never `rm -rf` to "fix" a build.
