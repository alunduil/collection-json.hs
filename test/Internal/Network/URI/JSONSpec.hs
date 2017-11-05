{-|
Module      : Internal.Network.URI.JSONSpec
Description : Tests for Internal.Network.URI.JSON
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "Internal.Network.URI.JSON".
-}
module Internal.Network.URI.JSONSpec (main, spec) where

import Data.Aeson (decode, encode)
import Data.Maybe (fromJust)
import Network.URI (URI)
import Test.Hspec (describe, hspec, Spec)
import Test.Hspec.QuickCheck (prop)
import Test.Invariant ((<=>))

import Internal.Network.URI.Arbitrary ()
import Internal.Network.URI.JSON ()

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "properties" $
    prop "fromJust . decode . encode == id" (fromJust . decode . encode <=> id :: URI -> Bool)
