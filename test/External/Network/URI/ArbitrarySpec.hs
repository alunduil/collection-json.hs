{-|
Module      : External.Network.URI.ArbitrarySpec
Description : Tests for External.Network.URI.Arbitrary
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "External.Network.URI.Arbitrary".
-}
module External.Network.URI.ArbitrarySpec (main, spec) where

import Network.URI (isURIReference, parseURIReference, uriToString)
import Test.Hspec (describe, hspec, Spec)
import Test.Hspec.QuickCheck (prop)

import External.Network.URI.Arbitrary ()

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "properties" $
    do prop "isURIReference (uriToString id u \"\")" $ \ u -> isURIReference (uriToString id u "")

       prop "Just u == parseURIReference (uriToString id u \"\")" $ \ u -> Just u == parseURIReference (uriToString id u "")
