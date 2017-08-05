{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Data.CollectionJSON.Tests
Description : Tests for Data.CollectionJSON
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "Data.CollectionJSON".
-}
module Data.CollectionJSON.Tests (tests) where

import Data.Aeson (decode, encode)
import Data.Maybe (fromJust)
import Data.Text (pack)
import Distribution.TestSuite (Test)
import Test.Invariant ((<=>))
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.QuickCheck.Instances ()

import Data.CollectionJSON
import Internal.Distribution.TestSuite.Compat.QuickCheck (qcTest)
import Internal.Network.URI.Tests ()

tests :: [Test]
tests = [ qcTest "fromJSON . toJSON == id—Collection" [] prop_id_c
        , qcTest "fromJSON . toJSON == id—Link)"      [] prop_id_l
        , qcTest "fromJSON . toJSON == id—Item)"      [] prop_id_i
        , qcTest "fromJSON . toJSON == id—Query)"     [] prop_id_q
        , qcTest "fromJSON . toJSON == id—Template)"  [] prop_id_t
        , qcTest "fromJSON . toJSON == id—Error)"     [] prop_id_e
        , qcTest "fromJSON . toJSON == id—Datum)"     [] prop_id_d
        ]

prop_id_c :: Collection -> Bool
prop_id_c = fromJust . decode . encode <=> id

prop_id_l :: Link -> Bool
prop_id_l = fromJust . decode . encode <=> id

prop_id_i :: Item -> Bool
prop_id_i = fromJust . decode . encode <=> id

prop_id_q :: Query -> Bool
prop_id_q = fromJust . decode . encode <=> id

prop_id_t :: Template -> Bool
prop_id_t = fromJust . decode . encode <=> id

prop_id_e :: Error -> Bool
prop_id_e = fromJust . decode . encode <=> id

prop_id_d :: Datum -> Bool
prop_id_d = fromJust . decode . encode <=> id

instance Arbitrary Collection where
  arbitrary =
    do let cVersion = pack "1.0"
       cHref     <- arbitrary
       cLinks    <- arbitrary
       cItems    <- arbitrary
       cQueries  <- arbitrary
       cTemplate <- arbitrary
       cError    <- arbitrary

       return Collection{..}

instance Arbitrary Link where
  arbitrary =
    do lHref   <- arbitrary
       lRel    <- arbitrary
       lName   <- arbitrary
       lRender <- arbitrary
       lPrompt <- arbitrary

       return Link{..}

instance Arbitrary Item where
  arbitrary =
    do iHref  <- arbitrary
       iData  <- arbitrary
       iLinks <- arbitrary

       return Item{..}

instance Arbitrary Query where
  arbitrary =
    do qHref   <- arbitrary
       qRel    <- arbitrary
       qName   <- arbitrary
       qPrompt <- arbitrary
       qData   <- arbitrary

       return Query{..}

instance Arbitrary Template where
  arbitrary =
    do tData <- arbitrary
       
       return Template{..}

instance Arbitrary Error where
  arbitrary =
    do eTitle   <- arbitrary
       eCode    <- arbitrary
       eMessage <- arbitrary

       return Error{..}

instance Arbitrary Datum where
  arbitrary =
    do dName   <- arbitrary
       dValue  <- arbitrary
       dPrompt <- arbitrary

       return Datum{..}
