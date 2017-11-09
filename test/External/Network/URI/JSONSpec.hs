{-|
Module      : External.Network.URI.JSONSpec
Description : Tests for External.Network.URI.JSON
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "External.Network.URI.JSON".
-}
module External.Network.URI.JSONSpec (main, spec) where

import Data.Aeson (decode, encode)
import Data.Maybe (fromJust)
import Network.URI (URI)
import Test.Hspec (describe, hspec, Spec)
import Test.Hspec.QuickCheck (prop)
import Test.Invariant ((<=>))

import External.Network.URI.Arbitrary ()
import External.Network.URI.JSON ()

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "properties" $
    prop "fromJust . decode . encode == id" (fromJust . decode . encode <=> id :: URI -> Bool)
