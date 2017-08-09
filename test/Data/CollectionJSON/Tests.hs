{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Data.CollectionJSON.Tests
Description : Tests for Data.CollectionJSON
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "Data.CollectionJSON".
-}
module Data.CollectionJSON.Tests (runTests) where

import Data.Aeson (decode, encode)
import Data.Maybe (fromJust)
import Data.Text (pack)
import Test.Invariant ((<=>))
import Test.QuickCheck (Arbitrary (arbitrary), forAllProperties, maxSize, stdArgs, quickCheckWithResult)
import Test.QuickCheck.Instances ()

import Data.CollectionJSON
import Internal.Network.URI.Tests ()

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

return []
runTests :: IO Bool
runTests = $forAllProperties $ quickCheckWithResult $ stdArgs {maxSize = 50}

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
