# collection-json

[![Hackage](https://img.shields.io/hackage/v/collection-json.svg)](https://hackage.haskell.org/package/collection-json)
[![CI](https://github.com/alunduil/collection-json.hs/actions/workflows/ci.yml/badge.svg)](https://github.com/alunduil/collection-json.hs/actions/workflows/ci.yml)
[![Codecov](https://codecov.io/gh/alunduil/collection-json.hs/branch/main/graph/badge.svg)](https://codecov.io/gh/alunduil/collection-json.hs)
[![License](https://img.shields.io/github/license/alunduil/collection-json.hs.svg)](LICENSE)
[![GHC](https://img.shields.io/badge/GHC-9.10%20%7C%209.12%20%7C%209.14-blue.svg)](https://www.haskell.org/ghc/)

[Collection+JSON—Hypermedia Type][Collection+JSON] tools for [Haskell].

`collection-json` lets you encode, decode, and manipulate
`application/vnd.collection+json` documents. The library exposes a single
module, `Data.CollectionJSON`, with `aeson` `ToJSON`/`FromJSON` instances for
each type defined by the spec—`Collection`, `Item`, `Link`, `Query`,
`Template`, `Datum`, and `Error`.

## Install

Add `collection-json` to the `build-depends` of your package, or install it
directly:

```sh
cabal install --lib collection-json
```

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
  let href = fromJust (parseURI "http://example.com/friends/")
      c    = Collection "1.0" href [] [] [] Nothing Nothing

  BL.putStrLn (encode c)
  -- {"collection":{"href":"http://example.com/friends/","version":"1.0"}}

  print (decode (encode c) :: Maybe Collection)
  -- Just (Collection {cVersion = "1.0", cHref = ..., ...})
```

Full API reference on [Hackage]. The Collection+JSON specification is at
<http://amundsen.com/media-types/collection/>.

## Contributing

Report bugs and feature requests on the [issue tracker]. See
[`CONTRIBUTING.md`](CONTRIBUTING.md) for the build, test, formatter, branch,
and release conventions.

For changes, fork the repository, branch from `main`, and open a pull request
against `main`. PRs are squash-merged. If you'd like attribution, add yourself
to the [`COPYRIGHT`](COPYRIGHT) file in the same PR.

## License

MIT—see [`LICENSE`](LICENSE).

[Collection+JSON]: http://amundsen.com/media-types/collection/
[Hackage]: https://hackage.haskell.org/package/collection-json
[Haskell]: https://www.haskell.org/
[issue tracker]: https://github.com/alunduil/collection-json.hs/issues
