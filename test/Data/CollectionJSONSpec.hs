{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.CollectionJSONSpec
Description : Tests for Data.CollectionJSON
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "Data.CollectionJSON".
-}
module Data.CollectionJSONSpec (tests) where

import Data.Aeson (FromJSON, ToJSON, decode, eitherDecode, encode)
import Data.Either (fromLeft)
import Data.List (isInfixOf, isSuffixOf)
import Data.Maybe (fromJust, isJust, isNothing)
import Network.URI (URI, nullURI, parseURIReference)
import Test.Invariant ((<=>))
import Test.Tasty (TestName, TestTree, localOption, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck (QuickCheckMaxSize (QuickCheckMaxSize), testProperty)

import qualified Data.ByteString.Lazy as BL (ByteString)

import Data.CollectionJSON
import Data.CollectionJSON.Arbitrary ()

uri :: String -> URI
uri = fromJust . parseURIReference

exampleURI :: URI
exampleURI = uri "http://example.com"

decodeFailure :: Either String a -> String
decodeFailure = fromLeft "decode succeeded"

holds :: TestName -> Bool -> TestTree
holds name = testCase name . assertBool "expected to hold"

contains :: String -> String -> Assertion
contains reported fragment =
  assertBool (show reported <> " does not contain " <> show fragment) $
    fragment `isInfixOf` reported

endsWith :: String -> String -> Assertion
endsWith reported suffix =
  assertBool (show reported <> " does not end with " <> show suffix) $
    suffix `isSuffixOf` reported

tests :: TestTree
tests =
  localOption (QuickCheckMaxSize 25) $
    testGroup
      "application/vnd.collection+json"
      [ rfcComplianceTests
      , commonParseErrorsTests
      , requiredKeysTests
      , hrefTests
      , valueTests
      , renderTests
      , versionTests
      , propertiesTests
      , missingKeysTests
      ]

rfcComplianceTests :: TestTree
rfcComplianceTests =
  testGroup
    "RFC compliance (http://amundsen.com/media-types/collection/format/)"
    [ holds "'Template' decode JSON string: \"{}\"" $ isJust (decode "{}" :: Maybe Template)
    , holds "'Collection' decode JSON string: \"{\"collection\":{}}\"" $ isJust (decode "{\"collection\":{}}" :: Maybe Collection)
    ]

commonParseErrorsTests :: TestTree
commonParseErrorsTests =
  testGroup
    "common parse errors"
    [ holds "'Collection' errors on \"{}\"" $
        isNothing (decode "{}" :: Maybe Collection)
    ]

requiredKeysTests :: TestTree
requiredKeysTests =
  testGroup
    "required keys"
    [ testGroup
        "decode fails when a spec-required key is absent"
        [ holds "'Link' without \"href\"" $ isNothing (decode withoutHref :: Maybe Link)
        , holds "'Link' without \"rel\"" $ isNothing (decode withoutRel :: Maybe Link)
        , holds "'Query' without \"href\"" $ isNothing (decode withoutHref :: Maybe Query)
        , holds "'Query' without \"rel\"" $ isNothing (decode withoutRel :: Maybe Query)
        , holds "'Datum' without \"name\"" $ isNothing (decode "{}" :: Maybe Datum)
        ]
    ]
 where
  withoutHref = "{\"rel\":\"item\"}" :: BL.ByteString
  withoutRel = "{\"href\":\"http://example.com\"}" :: BL.ByteString

hrefTests :: TestTree
hrefTests =
  testGroup
    "href"
    [ testGroup
        "decode accepts a relative reference"
        [ testCase "'Link'" $ fmap lHref (decode relativeLink) @?= Just relativeURI
        , testCase "'Item'" $ fmap iHref (decode relativeItem) @?= Just (Just relativeURI)
        , testCase "'Query'" $ fmap qHref (decode relativeQuery) @?= Just relativeURI
        , testCase "'Collection'" $ fmap cHref (decode relativeCollection) @?= Just relativeURI
        ]
    , testGroup
        "decode resolves an absent or empty 'Collection' href to the empty reference"
        [ testCase "absent" $ fmap cHref (decode absentHref) @?= Just nullURI
        , testCase "empty" $ fmap cHref (decode emptyHref) @?= Just nullURI
        ]
    , testGroup
        "'Item' href is optional"
        [ testCase "decode reports an absent href as absent" $
            fmap iHref (decode itemWithoutHref) @?= Just Nothing
        , testCase "encode omits an absent href" $
            encode (Item Nothing [] []) @?= itemWithoutHref
        ]
    ]
 where
  relativeLink = "{\"href\":\"/api/characters\",\"rel\":\"item\"}" :: BL.ByteString
  relativeItem = "{\"href\":\"/api/characters\"}" :: BL.ByteString
  relativeQuery = "{\"href\":\"/api/characters\",\"rel\":\"search\"}" :: BL.ByteString
  relativeCollection = "{\"collection\":{\"href\":\"/api/characters\"}}" :: BL.ByteString

  absentHref = "{\"collection\":{}}" :: BL.ByteString
  emptyHref = "{\"collection\":{\"href\":\"\"}}" :: BL.ByteString

  itemWithoutHref = "{}" :: BL.ByteString

  relativeURI = uri "/api/characters"

valueTests :: TestTree
valueTests =
  testGroup
    "'Datum' value"
    [ testGroup
        "decode accepts every scalar the format admits"
        [ testCase "STRING" $ dValueOf "\"x\"" @?= Just (Just (DatumString "x"))
        , testCase "NUMBER" $ dValueOf "5" @?= Just (Just (DatumNumber 5))
        , testCase "true" $ dValueOf "true" @?= Just (Just (DatumBool True))
        , testCase "false" $ dValueOf "false" @?= Just (Just (DatumBool False))
        , testCase "null" $ dValueOf "null" @?= Just Nothing
        ]
    , testGroup
        "decode rejects the structures the format excludes"
        [ testCase "OBJECT" $ rejects "{\"a\":1}" "Object"
        , testCase "ARRAY" $ rejects "[1,2]" "Array"
        ]
    ]
 where
  datum v = "{\"name\":\"n\",\"value\":" <> v <> "}" :: BL.ByteString
  dValueOf v = fmap dValue (decode (datum v) :: Maybe Datum)

  rejects v structure = do
    let reported = decodeFailure (eitherDecode (datum v) :: Either String Datum)
    reported `contains` "DatumValue"
    reported `endsWith` structure

renderTests :: TestTree
renderTests =
  testGroup
    "'Link' render"
    [ testCase "decode reads \"image\"" $ lRenderOf "\"image\"" @?= Just (Just RenderImage)
    , testCase "decode reads \"link\"" $ lRenderOf "\"link\"" @?= Just (Just RenderLink)
    , testCase "decode fails on any other value, naming it and the two allowed" $
        decodeFailure (eitherDecode (link "\"embed\"") :: Either String Link)
          `endsWith` "render must be \"image\" or \"link\", not \"embed\""
    , testCase "encode writes the format's spelling" $
        encode (Link exampleURI "item" Nothing (Just RenderImage) Nothing) @?= link "\"image\""
    ]
 where
  link r = "{\"href\":\"http://example.com\",\"rel\":\"item\",\"render\":" <> r <> "}" :: BL.ByteString
  lRenderOf r = fmap lRender (decode (link r) :: Maybe Link)

versionTests :: TestTree
versionTests =
  testGroup
    "'Collection' version"
    [ testCase "decode defaults an absent version to 1.0" $
        cVersionOf "{\"collection\":{}}" @?= Just "1.0"
    , testCase "decode passes through a version other than 1.0" $
        cVersionOf "{\"collection\":{\"version\":\"1.1\"}}" @?= Just "1.1"
    ]
 where
  cVersionOf d = fmap cVersion (decode d :: Maybe Collection)

propertiesTests :: TestTree
propertiesTests =
  testGroup
    "properties"
    [ testGroup
        "fromJust . decode . encode == id"
        [ testProperty "Datum" (roundtrips :: Datum -> Bool)
        , testProperty "Error" (roundtrips :: Error -> Bool)
        , testProperty "Template" (roundtrips :: Template -> Bool)
        , testProperty "Query" (roundtrips :: Query -> Bool)
        , testProperty "Item" (roundtrips :: Item -> Bool)
        , testProperty "Link" (roundtrips :: Link -> Bool)
        , testProperty "Collection" (roundtrips :: Collection -> Bool)
        ]
    ]

roundtrips :: (Eq a, FromJSON a, ToJSON a) => a -> Bool
roundtrips = fromJust . decode . encode <=> id

missingKeysTests :: TestTree
missingKeysTests =
  testGroup
    "JSON Missing Keys"
    [ testGroup
        "decode minimal JSON strings"
        [ holds "Datum" $ isJust (decode mDatum :: Maybe Datum)
        , holds "Error" $ isJust (decode mError :: Maybe Error)
        , holds "Template" $ isJust (decode mTemplate :: Maybe Template)
        , holds "Query" $ isJust (decode mQuery :: Maybe Query)
        , holds "Item" $ isJust (decode mItem :: Maybe Item)
        , holds "Link" $ isJust (decode mLink :: Maybe Link)
        , holds "Collection" $ isJust (decode mCollection :: Maybe Collection)
        ]
    , testGroup
        "encode minimal data to JSON"
        [ testCase "Datum" $
            encode (Datum "name" Nothing Nothing) @?= mDatum
        , testCase "Error" $
            encode (Error Nothing Nothing Nothing) @?= mError
        , testCase "Template" $
            encode (Template []) @?= mTemplate
        , testCase "Query" $
            encode (Query exampleURI "item" Nothing Nothing []) @?= mQuery
        , testCase "Item" $
            encode (Item (Just exampleURI) [] []) @?= mItem
        , testCase "Link" $
            encode (Link exampleURI "item" Nothing Nothing Nothing) @?= mLink
        , testCase "Collection" $
            encode (Collection "1.0" exampleURI [] [] [] Nothing Nothing) @?= mCollection
        ]
    ]
 where
  mDatum = "{\"name\":\"name\"}" :: BL.ByteString
  mError = "{}" :: BL.ByteString
  mTemplate = "{\"data\":[]}" :: BL.ByteString
  mQuery = "{\"href\":\"http://example.com\",\"rel\":\"item\"}" :: BL.ByteString
  mItem = "{\"href\":\"http://example.com\"}" :: BL.ByteString
  mLink = "{\"href\":\"http://example.com\",\"rel\":\"item\"}" :: BL.ByteString
  mCollection = "{\"collection\":{\"href\":\"http://example.com\",\"version\":\"1.0\"}}" :: BL.ByteString
