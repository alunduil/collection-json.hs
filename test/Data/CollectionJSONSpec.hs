{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.CollectionJSONSpec
Description : Tests for Data.CollectionJSON
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "Data.CollectionJSON".
-}
module Data.CollectionJSONSpec (main, spec) where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Maybe (fromJust, isJust, isNothing)
import Network.URI (URI, nullURI, parseURIReference)
import Test.Hspec (Spec, context, describe, hspec, it, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSize, prop)
import Test.Invariant ((<=>))

import qualified Data.ByteString.Lazy as BL (ByteString)

import Data.CollectionJSON
import Data.CollectionJSON.Arbitrary ()

main :: IO ()
main = hspec spec

uri :: String -> URI
uri = fromJust . parseURIReference

spec :: Spec
spec =
  describe "application/vnd.collection+json" $
    modifyMaxSize (const 25) $
      do
        rfcComplianceSpec
        commonParseErrorsSpec
        requiredKeysSpec
        hrefSpec
        valueSpec
        renderSpec
        versionSpec
        propertiesSpec
        missingKeysSpec

rfcComplianceSpec :: Spec
rfcComplianceSpec =
  describe "RFC compliance (http://amundsen.com/media-types/collection/format/)" $
    do
      it "'Template' decode JSON string: \"{}\"" $ isJust (decode "{}" :: Maybe Template)
      it "'Collection' decode JSON string: \"{\"collection\":{}}\"" $ isJust (decode "{\"collection\":{}}" :: Maybe Collection)

commonParseErrorsSpec :: Spec
commonParseErrorsSpec =
  describe "common parse errors" $
    do
      it "'Collection' errors on \"{}\"" $
        isNothing (decode "{}" :: Maybe Collection)

requiredKeysSpec :: Spec
requiredKeysSpec =
  describe "required keys" $
    context "decode fails when a spec-required key is absent" $
      do
        it "'Link' without \"href\"" $ isNothing (decode withoutHref :: Maybe Link)
        it "'Link' without \"rel\"" $ isNothing (decode withoutRel :: Maybe Link)
        it "'Query' without \"href\"" $ isNothing (decode withoutHref :: Maybe Query)
        it "'Query' without \"rel\"" $ isNothing (decode withoutRel :: Maybe Query)
        it "'Datum' without \"name\"" $ isNothing (decode "{}" :: Maybe Datum)
 where
  withoutHref = "{\"rel\":\"item\"}" :: BL.ByteString
  withoutRel = "{\"href\":\"http://example.com\"}" :: BL.ByteString

hrefSpec :: Spec
hrefSpec =
  describe "href" $
    do
      context "decode accepts a relative reference" $
        do
          it "'Link'" $ fmap lHref (decode relativeLink) `shouldBe` Just relativeURI
          it "'Item'" $ fmap iHref (decode relativeItem) `shouldBe` Just (Just relativeURI)
          it "'Query'" $ fmap qHref (decode relativeQuery) `shouldBe` Just relativeURI
          it "'Collection'" $ fmap cHref (decode relativeCollection) `shouldBe` Just relativeURI

      context "decode resolves an absent or empty 'Collection' href to the empty reference" $
        do
          it "absent" $ fmap cHref (decode absentHref) `shouldBe` Just nullURI
          it "empty" $ fmap cHref (decode emptyHref) `shouldBe` Just nullURI

      context "'Item' href is optional" $
        do
          it "decode reports an absent href as absent" $
            fmap iHref (decode itemWithoutHref) `shouldBe` Just Nothing

          it "encode omits an absent href" $
            encode (Item Nothing [] []) `shouldBe` itemWithoutHref
 where
  relativeLink = "{\"href\":\"/api/characters\",\"rel\":\"item\"}" :: BL.ByteString
  relativeItem = "{\"href\":\"/api/characters\"}" :: BL.ByteString
  relativeQuery = "{\"href\":\"/api/characters\",\"rel\":\"search\"}" :: BL.ByteString
  relativeCollection = "{\"collection\":{\"href\":\"/api/characters\"}}" :: BL.ByteString

  absentHref = "{\"collection\":{}}" :: BL.ByteString
  emptyHref = "{\"collection\":{\"href\":\"\"}}" :: BL.ByteString

  itemWithoutHref = "{}" :: BL.ByteString

  relativeURI = uri "/api/characters"

valueSpec :: Spec
valueSpec =
  describe "'Datum' value" $
    do
      context "decode accepts every scalar the format admits" $
        do
          it "STRING" $ dValueOf "\"x\"" `shouldBe` Just (Just (DatumString "x"))
          it "NUMBER" $ dValueOf "5" `shouldBe` Just (Just (DatumNumber 5))
          it "true" $ dValueOf "true" `shouldBe` Just (Just (DatumBool True))
          it "false" $ dValueOf "false" `shouldBe` Just (Just (DatumBool False))
          it "null" $ dValueOf "null" `shouldBe` Just Nothing

      context "decode rejects the structures the format excludes" $
        do
          it "OBJECT" $ isNothing (decode (datum "{\"a\":1}") :: Maybe Datum)
          it "ARRAY" $ isNothing (decode (datum "[1,2]") :: Maybe Datum)
 where
  datum v = "{\"name\":\"n\",\"value\":" <> v <> "}" :: BL.ByteString
  dValueOf v = fmap dValue (decode (datum v) :: Maybe Datum)

renderSpec :: Spec
renderSpec =
  describe "'Link' render" $
    do
      it "decode reads \"image\"" $ lRenderOf "\"image\"" `shouldBe` Just (Just RenderImage)
      it "decode reads \"link\"" $ lRenderOf "\"link\"" `shouldBe` Just (Just RenderLink)

      it "decode fails on any other value" $
        isNothing (decode (link "\"embed\"") :: Maybe Link)

      it "encode writes the format's spelling" $
        encode (Link eURI "item" Nothing (Just RenderImage) Nothing) `shouldBe` link "\"image\""
 where
  link r = "{\"href\":\"http://example.com\",\"rel\":\"item\",\"render\":" <> r <> "}" :: BL.ByteString
  lRenderOf r = fmap lRender (decode (link r) :: Maybe Link)

  eURI = uri "http://example.com"

versionSpec :: Spec
versionSpec =
  describe "'Collection' version" $
    it "decode passes through a version other than 1.0" $
      fmap cVersion (decode "{\"collection\":{\"version\":\"1.1\"}}" :: Maybe Collection)
        `shouldBe` Just "1.1"

propertiesSpec :: Spec
propertiesSpec =
  describe "properties" $
    context "fromJust . decode . encode == id" $
      do
        prop "Datum" (roundtrips :: Datum -> Bool)
        prop "Error" (roundtrips :: Error -> Bool)
        prop "Template" (roundtrips :: Template -> Bool)
        prop "Query" (roundtrips :: Query -> Bool)
        prop "Item" (roundtrips :: Item -> Bool)
        prop "Link" (roundtrips :: Link -> Bool)
        prop "Collection" (roundtrips :: Collection -> Bool)

roundtrips :: (Eq a, FromJSON a, ToJSON a) => a -> Bool
roundtrips = fromJust . decode . encode <=> id

missingKeysSpec :: Spec
missingKeysSpec =
  describe "JSON Missing Keys" $
    do
      context "decode minimal JSON strings" $
        do
          it "Datum" $ isJust (decode mDatum :: Maybe Datum)
          it "Error" $ isJust (decode mError :: Maybe Error)
          it "Template" $ isJust (decode mTemplate :: Maybe Template)
          it "Query" $ isJust (decode mQuery :: Maybe Query)
          it "Item" $ isJust (decode mItem :: Maybe Item)
          it "Link" $ isJust (decode mLink :: Maybe Link)
          it "Collection" $ isJust (decode mCollection :: Maybe Collection)

      context "encode minimal data to JSON" $
        do
          it "Datum" $
            encode (Datum "name" Nothing Nothing) `shouldBe` mDatum

          it "Error" $
            encode (Error Nothing Nothing Nothing) `shouldBe` mError

          it "Template" $
            encode (Template []) `shouldBe` mTemplate

          it "Query" $
            encode (Query eURI "item" Nothing Nothing []) `shouldBe` mQuery

          it "Item" $
            encode (Item (Just eURI) [] []) `shouldBe` mItem

          it "Link" $
            encode (Link eURI "item" Nothing Nothing Nothing) `shouldBe` mLink

          it "Collection" $
            encode (Collection "1.0" eURI [] [] [] Nothing Nothing) `shouldBe` mCollection

      context "decode supplies defaults for absent optional keys" $
        do
          it "Collection \"version\" defaults to 1.0" $
            fmap cVersion (decode "{\"collection\":{}}" :: Maybe Collection) `shouldBe` Just "1.0"
 where
  mDatum = "{\"name\":\"name\"}" :: BL.ByteString
  mError = "{}" :: BL.ByteString
  mTemplate = "{\"data\":[]}" :: BL.ByteString
  mQuery = "{\"href\":\"http://example.com\",\"rel\":\"item\"}" :: BL.ByteString
  mItem = "{\"href\":\"http://example.com\"}" :: BL.ByteString
  mLink = "{\"href\":\"http://example.com\",\"rel\":\"item\"}" :: BL.ByteString
  mCollection = "{\"collection\":{\"href\":\"http://example.com\",\"version\":\"1.0\"}}" :: BL.ByteString

  eURI = uri "http://example.com"
