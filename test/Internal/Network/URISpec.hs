{-|
Module      : Internal.Network.URISpec
Description : Tests for Internal.Network.URI
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "Internal.Network.URISpec".
-}
module Internal.Network.URISpec (main, spec) where

import Data.Aeson (decode, encode)
import Data.Maybe (fromJust)
import Network.URI (URI)
import Test.Hspec (context, describe, hspec, it, Spec)
import Test.Invariant ((<=>))
import Test.QuickCheck (property)

import Internal.Network.URI ()
import Internal.Network.URI.Arbitrary ()

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Internal.Network.URI" $
    describe "properties" $
      context "fromJust . decode . encode == id" $
        it "URI" $ property (fromJust . decode . encode <=> id :: URI -> Bool)
