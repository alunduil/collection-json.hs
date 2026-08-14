# collection-json

[![Hackage](https://img.shields.io/hackage/v/collection-json.svg)](https://hackage.haskell.org/package/collection-json)
[![CI](https://github.com/alunduil/collection-json.hs/actions/workflows/ci.yml/badge.svg)](https://github.com/alunduil/collection-json.hs/actions/workflows/ci.yml)
[![Codecov](https://codecov.io/gh/alunduil/collection-json.hs/branch/main/graph/badge.svg)](https://codecov.io/gh/alunduil/collection-json.hs)
[![License](https://img.shields.io/github/license/alunduil/collection-json.hs.svg)](LICENSE)
[![GHC](https://img.shields.io/badge/GHC-9.6%20%7C%209.8%20%7C%209.10%20%7C%209.12%20%7C%209.14-blue.svg)](https://www.haskell.org/ghc/)

[Collection+JSON—Hypermedia Type][Collection+JSON] tools for [Haskell].

By Alex Brandt. Source at <https://github.com/alunduil/collection-json.hs>.

`collection-json` lets you encode, decode, and manipulate
`application/vnd.collection+json` documents. The library exposes a single
module, `Data.CollectionJSON`, with `aeson` `ToJSON`/`FromJSON` instances for
each type defined by the spec—`Collection`, `Item`, `Link`, `Query`,
`Template`, `Datum`, and `Error`.

## Install

You need GHC and `cabal-install`.

Add `collection-json` to the `build-depends` of your package, or install it
directly:

```sh
cabal install --lib collection-json
```

[`CHANGELOG.md`](CHANGELOG.md) records what changed in each release.

## Usage

Roundtrip a minimal collection through `aeson`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.CollectionJSON (Collection (..))
import Data.Maybe (fromJust)
import Network.URI (parseURI)

main :: IO ()
main = do
  let c =
        Collection
          { cVersion = "1.0"
          , cHref = fromJust (parseURI "http://example.com/friends/")
          , cLinks = []
          , cItems = []
          , cQueries = []
          , cTemplate = Nothing
          , cError = Nothing
          }
      json = encode c

  BL.putStrLn json
  -- {"collection":{"href":"http://example.com/friends/","version":"1.0"}}

  print (decode json :: Maybe Collection)
  -- Just (Collection {cVersion = "1.0", cHref = ..., ...})
```

Full API reference on [Hackage]. The Collection+JSON specification is at
<https://github.com/collection-json/spec>.

## Scope

`collection-json` stops at the wire format: it models the envelope and
bridges your domain types through `FromCollection`/`ToCollection`, but
leaves content negotiation, profile semantics, and request-shape
validation to adjacent layers. For `href` values, [`network-uri-json`]
provides `Network.URI` JSON instances that compose with these types.

## Contributing

Report bugs and feature requests on the [issue tracker]. See
[`CONTRIBUTING.md`](CONTRIBUTING.md) for the build, test, formatter, branch,
and release conventions. [`CODE_OF_CONDUCT.md`](CODE_OF_CONDUCT.md) covers
how contributors treat each other.

Support is free and comes from one maintainer, as time allows.

## License

Use, modify, and redistribute this library freely, including inside
proprietary software, as long as you keep the copyright and permission
notices. [`LICENSE`](LICENSE) reproduces the MIT license in full.

[Collection+JSON]: https://github.com/collection-json/spec
[Hackage]: https://hackage.haskell.org/package/collection-json
[Haskell]: https://www.haskell.org/
[issue tracker]: https://github.com/alunduil/collection-json.hs/issues
[`network-uri-json`]: https://github.com/alunduil/network-uri-json
