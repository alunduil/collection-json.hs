{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Data.CollectionJSON.Arbitrary
Description : Arbitrary Instances for Data.CollectionJSON
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Arbitrary instances for "Data.CollectionJSON".
-}
module Data.CollectionJSON.Arbitrary where

import Control.Applicative ((<$>), (<*>))
import Data.Text (pack)
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.QuickCheck.Instances ()

import Data.CollectionJSON
import External.Network.URI.Arbitrary ()

instance Arbitrary Collection where
  arbitrary = Collection (pack "1.0") <$> arbitrary
                                      <*> arbitrary
                                      <*> arbitrary
                                      <*> arbitrary
                                      <*> arbitrary
                                      <*> arbitrary

instance Arbitrary Link where
  arbitrary = Link <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary

instance Arbitrary Item where
  arbitrary = Item <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary

instance Arbitrary Query where
  arbitrary = Query <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary

instance Arbitrary Template where
  arbitrary = Template <$> arbitrary

instance Arbitrary Error where
  arbitrary = Error <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary

instance Arbitrary Datum where
  arbitrary = Datum <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary
