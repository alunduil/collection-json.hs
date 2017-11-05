{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Data.CollectionJSONSpec
Description : Tests for Data.CollectionJSON
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "Data.CollectionJSON".
-}
module Data.CollectionJSONSpec (main, spec) where

import Data.Aeson (decode, encode)
import Data.Maybe (fromJust, isJust, isNothing)
import Network.URI (parseURIReference)
import Test.Hspec (context, describe, hspec, it, shouldBe, Spec)
import Test.Hspec.QuickCheck (modifyMaxSize, prop)
import Test.Invariant ((<=>))

import qualified Data.ByteString.Lazy as BL (ByteString)

import Data.CollectionJSON
import Data.CollectionJSON.Arbitrary ()

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "application/vnd.collection+json" $ modifyMaxSize (const 25) $
    do describe "RFC compliance (http://amundsen.com/media-types/collection/format/)" $
         do prop "'Template' decode JSON string: \"{}\"" $ isJust (decode "{}" :: Maybe Template)
            prop "'Collection' decode JSON string: \"{\"collection\":{}}\"" $ isJust (decode "{\"collection\":{}}" :: Maybe Collection) 

       describe "common parse errors" $
         prop "'Collection' errors on \"{}\"" $ isNothing (decode "{}" :: Maybe Collection)

       describe "properties" $
         context "fromJust . decode . encode == id" $
           do prop "Datum"      (fromJust . decode . encode <=> id :: Datum -> Bool)
              prop "Error"      (fromJust . decode . encode <=> id :: Error -> Bool)
              prop "Template"   (fromJust . decode . encode <=> id :: Template -> Bool)
              prop "Query"      (fromJust . decode . encode <=> id :: Query -> Bool)
              prop "Item"       (fromJust . decode . encode <=> id :: Item -> Bool)
              prop "Link"       (fromJust . decode . encode <=> id :: Link -> Bool)
              prop "Collection" (fromJust . decode . encode <=> id :: Collection -> Bool)

       describe "JSON Missing Keys" $
         do context "decode minimal JSON strings" $
              do it "Datum"      $ isJust (decode mDatum :: Maybe Datum)
                 it "Error"      $ isJust (decode mError :: Maybe Error)
                 it "Template"   $ isJust (decode mTemplate :: Maybe Template)
                 it "Query"      $ isJust (decode mQuery :: Maybe Query)
                 it "Item"       $ isJust (decode mItem :: Maybe Item)
                 it "Link"       $ isJust (decode mLink :: Maybe Link)
                 it "Collection" $ isJust (decode mCollection :: Maybe Collection)

            context "encode minimal data to JSON" $
              do it "Datum" $
                   encode (Datum "name" Nothing Nothing) `shouldBe` mDatum

                 it "Error" $
                   encode (Error Nothing Nothing Nothing) `shouldBe` mError

                 it "Template" $
                   encode (Template []) `shouldBe` mTemplate

                 it "Query" $
                   encode (Query eURI "item" Nothing Nothing []) `shouldBe` mQuery

                 it "Item" $
                   encode (Item eURI [] []) `shouldBe` mItem

                 it "Link" $
                   encode (Link eURI "item" Nothing Nothing Nothing) `shouldBe` mLink

                 it "Collection" $
                   encode (Collection "1.0" eURI [] [] [] Nothing Nothing) `shouldBe` mCollection

  where mCollection = "{\"collection\":{\"href\":\"http://example.com\",\"version\":\"1.0\"}}" :: BL.ByteString
        mLink       = "{\"href\":\"http://example.com\",\"rel\":\"item\"}" :: BL.ByteString
        mItem       = "{\"href\":\"http://example.com\"}" :: BL.ByteString
        mQuery      = "{\"href\":\"http://example.com\",\"rel\":\"item\"}" :: BL.ByteString
        mTemplate   = "{\"data\":[]}" :: BL.ByteString
        mError      = "{}" :: BL.ByteString
        mDatum      = "{\"name\":\"name\"}" :: BL.ByteString

        eURI = fromJust $ parseURIReference "http://example.com"
