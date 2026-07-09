# collection-json — agent guide

## Project summary

Haskell library: types, `FromJSON`/`ToJSON` instances, and
`FromCollection`/`ToCollection` classes for the
`application/vnd.collection+json` hypermedia type. Single exposed module
`Data.CollectionJSON`; the test tree under `test/` mirrors it. MIT.
Hackage: `collection-json`.

## Tooling inventory

Reach for what's already wired before curl/manual API calls/first-
principles scripts. Single-package project (`cabal.project`: `packages: .`).

- **Build / test**: `cabal build`, `cabal test` (hspec-discover picks up
  `*Spec.hs`). No Nix, no direnv — plain `cabal`.
- **Lint / format**: `pre-commit` (`.pre-commit-config.yaml`) runs
  fourmolu (`fourmolu.yaml`), hlint (`hlint.yaml`), markdownlint
  (`.markdownlint.jsonc`), and Vale (`.vale.ini`; styles vendored under
  `.vale/styles`, so no `vale sync` needed). fourmolu and hlint run from
  PATH; their pinned versions live in `.github/workflows/pre-commit.yml`,
  not the pre-commit config.
- **CI**: `.github/workflows/ci.yml` (GHC 9.10/9.12/9.14 × ubuntu/macos,
  builds from the sdist tarball; a `coverage` job reports to Codecov,
  `codecov.yml`), `pre-commit.yml` (hooks), `links.yml` (lychee,
  `lychee.toml`).
- **Publish**: `.github/workflows/release.yml` — see Release process.
- **Deps**: `renovate.json`. **Versioning**: Haskell PVP.

## Source of truth for behaviour

The Collection+JSON spec at <https://github.com/collection-json/spec>
is authoritative. The hspec suite is an attempt to encode that spec;
when the two disagree, the spec wins and the tests are wrong. Don't
shift semantics to make a test pass — re-derive from the spec, then fix
the test.

## Build & test

```sh
cabal update                 # first time, or after a long gap
cabal build
cabal test                   # hspec-discover picks up *Spec.hs
```

Supported GHC: `ci.yml` tests 9.10/9.12/9.14. The `tested-with:` field
in `collection-json.cabal` lags (legacy, still `9.10.*`); trust the CI
matrix and `build-depends` bounds over that field, and verify any GHC
claim with `cabal build`, not metadata.

## Scope discipline

PVP-classified API surface makes out-of-scope edits expensive: an
incidental export change forces a major bump. Keep issue work tight.

- Before opening a PR, verify the diff doesn't bleed into linked or
  sibling issues; revert incidental out-of-scope edits before review.
- An issue blocked by unshipped prerequisites: propose deferral with a
  `blocked-by` edge, don't write premature code.
- File unrelated problems found mid-task as their own issues by default.

(The `issue-work` skill encodes the full flow; this note is the reminder
to apply it without invoking the skill.)

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
- Versioning: Haskell **PVP** (`A.B.C.D`). Current `1.3.1.3`; next
  `1.3.1.4` per milestone.
- Tag-driven via `.github/workflows/release.yml`: pushing a `v*` tag on
  `main` uploads a Hackage candidate; a `workflow_dispatch` run
  publishes (guarded by the `hackage` environment). Both steps enforce
  that the tag matches `version:` in `collection-json.cabal` and
  authenticate with the `HACKAGE_TOKEN` secret. Read `release.yml`
  before touching anything publish-related.

## Don't-touch list

- `Setup.hs` — one-line `defaultMain`; cabal boilerplate.
- `dist/`, `dist-newstyle/`, `.stack-work/`, `.cabal-sandbox/`, `*.hi`,
  `*.o` — gitignored build artefacts.
- `.vale/styles/` (except `config/`) — vendored by `vale sync`; our
  vocabulary lives in `.vale/styles/config`.
- `~/.cabal/packages/`, `~/.cabal/store/` — Hackage index and global
  build cache outside the repo. Never `rm -rf` to "fix" a build.
