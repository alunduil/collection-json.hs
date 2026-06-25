# collection-json — agent guide

## Project summary

Haskell library: types, `FromJSON`/`ToJSON` instances, and
`FromCollection`/`ToCollection` classes for the
`application/vnd.collection+json` hypermedia type. Single exposed module
`Data.CollectionJSON`; the test tree under `test/` mirrors it. MIT.
Hackage: `collection-json`.

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

`direnv allow` enters the `shell.nix` / `default.nix` Nix shell; plain
`cabal` outside Nix works too. No known `cabal test` gotchas; if one
turns up, file it rather than papering over it with flags.

Supported GHC: authoritative source is `tested-with:` plus
`build-depends` bounds in `collection-json.cabal`. They currently
disagree (legacy `tested-with` predates Renovate's bumps); #113
reconciles them. Verify any GHC claim with `cabal build`, not metadata.

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

## Release process (in flux)

- `develop` is the integration branch and PR target; #82 renames to
  `main`.
- Versioning: Haskell **PVP** (`A.B.C.D`). Current `1.3.1.3`; next
  `1.3.1.4` per milestone (#84).
- The current Cloud Build + `cabal.config.enc` + `cabal upload` flow
  will be replaced wholesale; #115 tracks the tag-driven replacement
  and #114 the CI migration. Don't memorise today's specifics — read
  `cloudbuild.yaml` if you need them, and check #114/#115 before
  touching anything publish-related.

## Don't-touch list

- `cabal.config.enc` — GCP-KMS-encrypted Hackage credentials. Never
  decrypt locally, never commit plaintext, never regenerate without
  rotating the KMS key.
- `Setup.hs` — one-line `defaultMain`; cabal boilerplate.
- `dist/`, `dist-newstyle/`, `.stack-work/`, `.cabal-sandbox/`, `*.hi`,
  `*.o` — gitignored build artefacts.
- `nix/network-arbitrary.nix`, `nix/network-uri-json.nix` — generated
  by `cabal2nix`; regenerate, don't hand-edit.
- `~/.cabal/packages/`, `~/.cabal/store/` — Hackage index and global
  build cache outside the repo. Never `rm -rf` to "fix" a build.
- `.travis.yml` — outdated; replacement in #114. Don't patch, don't
  rely on.
