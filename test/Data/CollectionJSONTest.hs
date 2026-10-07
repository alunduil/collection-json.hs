{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Data.CollectionJSONTest
Description : Tests for Data.CollectionJSON
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "Data.CollectionJSON".
-}
module Data.CollectionJSONTest (tests) where

import Data.Aeson (FromJSON, ToJSON, decode, eitherDecode, encode)
import Data.Either (fromLeft)
import Data.List (isInfixOf, isSuffixOf)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Typeable (Proxy (Proxy), TypeRep, Typeable, typeOf, typeRep)
import Network.URI (URI, nullURI, parseURIReference)
import Test.Invariant ((<=>))
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck (QuickCheckMaxSize (QuickCheckMaxSize), testProperty)

import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BLC (unpack)

import Data.CollectionJSON
import Data.CollectionJSON.Arbitrary ()

uri :: String -> URI
uri = fromJust . parseURIReference

exampleURI :: URI
exampleURI = uri "http://example.com"

decodeField :: FromJSON a => (a -> b) -> BL.ByteString -> Maybe b
decodeField field = fmap field . decode

decodeFailure :: Either String a -> String
decodeFailure = fromLeft "decode succeeded"

quotedType :: TypeRep -> String
quotedType t = "'" <> show t <> "'"

decodeSucceeds :: forall a. (FromJSON a, Typeable a) => BL.ByteString -> TestTree
decodeSucceeds json = testCase name . assertBool "decode returned Nothing" . isJust $ (decode json :: Maybe a)
 where
  name = quotedType (typeRep (Proxy :: Proxy a)) <> " " <> BLC.unpack json

decodeFails :: forall a. (FromJSON a, Typeable a) => String -> BL.ByteString -> TestTree
decodeFails key json = testCase name . assertBool "decode returned a value" . isNothing $ (decode json :: Maybe a)
 where
  name = quotedType (typeRep (Proxy :: Proxy a)) <> " without " <> show key

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
      [ optionalKeysTests
      , requiredKeysTests
      , hrefTests
      , valueTests
      , renderTests
      , versionTests
      , roundTripTests
      ]

optionalKeysTests :: TestTree
optionalKeysTests =
  testGroup
    "absent optional keys"
    [ testGroup "decode succeeds" $
        [ decodeSucceeds @Template "{}"
        , decodeSucceeds @Collection "{\"collection\":{}}"
        ]
          <> decodeTests
    , testGroup "encode omits them" encodeTests
    ]
 where
  (decodeTests, encodeTests) =
    unzip
      [ minimal (Datum "name" Nothing Nothing) "{\"name\":\"name\"}"
      , minimal (Error Nothing Nothing Nothing) "{}"
      , minimal (Template []) "{\"data\":[]}"
      , minimal (Query exampleURI "item" Nothing Nothing []) "{\"href\":\"http://example.com\",\"rel\":\"item\"}"
      , minimal (Item (Just exampleURI) [] []) "{\"href\":\"http://example.com\"}"
      , minimal (Link exampleURI "item" Nothing Nothing Nothing) "{\"href\":\"http://example.com\",\"rel\":\"item\"}"
      , minimal (Collection "1.0" exampleURI [] [] [] Nothing Nothing) "{\"collection\":{\"href\":\"http://example.com\",\"version\":\"1.0\"}}"
      ]

{- Separate decode and encode tables would need a heterogeneous list,
   which cannot carry the type variable a value shares with its JSON. -}
minimal :: forall a. (FromJSON a, ToJSON a, Typeable a) => a -> BL.ByteString -> (TestTree, TestTree)
minimal value json =
  ( decodeSucceeds @a json
  , testCase (quotedType (typeOf value)) $ encode value @?= json
  )

requiredKeysTests :: TestTree
requiredKeysTests =
  testGroup
    "absent required keys"
    [ testGroup
        "decode fails"
        [ decodeFails @Collection "collection" "{}"
        , decodeFails @Link "href" withoutHref
        , decodeFails @Link "rel" withoutRel
        , decodeFails @Query "href" withoutHref
        , decodeFails @Query "rel" withoutRel
        , decodeFails @Datum "name" "{}"
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
        [ testCase "'Link'" $ decodeField lHref relativeLink @?= Just relativeURI
        , testCase "'Item'" $ decodeField iHref relativeItem @?= Just (Just relativeURI)
        , testCase "'Query'" $ decodeField qHref relativeQuery @?= Just relativeURI
        , testCase "'Collection'" $ decodeField cHref relativeCollection @?= Just relativeURI
        ]
    , testGroup
        "decode resolves an absent or empty 'Collection' href to the empty reference"
        [ testCase "absent" $ decodeField cHref absentHref @?= Just nullURI
        , testCase "empty" $ decodeField cHref emptyHref @?= Just nullURI
        ]
    , testGroup
        "'Item' href is optional"
        [ testCase "decode reports an absent href as absent" $
            decodeField iHref itemWithoutHref @?= Just Nothing
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
  dValueOf v = decodeField dValue (datum v)

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
  lRenderOf r = decodeField lRender (link r)

versionTests :: TestTree
versionTests =
  testGroup
    "'Collection' version"
    [ testCase "decode defaults an absent version to 1.0" $
        decodeField cVersion "{\"collection\":{}}" @?= Just "1.0"
    , testCase "decode passes through a version other than 1.0" $
        decodeField cVersion "{\"collection\":{\"version\":\"1.1\"}}" @?= Just "1.1"
    ]

roundTripTests :: TestTree
roundTripTests =
  testGroup
    "round trip"
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
