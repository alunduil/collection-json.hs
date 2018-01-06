{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE RecordWildCards #-}

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
import Network.URI.Arbitrary ()
import Test.QuickCheck (Arbitrary (arbitrary, shrink))
import Test.QuickCheck.Instances ()

import Data.CollectionJSON

instance Arbitrary Collection where
  arbitrary = Collection (pack "1.0") <$> arbitrary
                                      <*> arbitrary
                                      <*> arbitrary
                                      <*> arbitrary
                                      <*> arbitrary
                                      <*> arbitrary

  shrink Collection{..} = [ Collection (pack "1.0") cHref' cLinks' cItems' cQueries' cTemplate' cError' | (cHref', cLinks', cItems', cQueries', cTemplate', cError') <- shrink (cHref, cLinks, cItems, cQueries, cTemplate, cError) ]

instance Arbitrary Link where
  arbitrary = Link <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary

  shrink Link{..} = [ Link lHref' lRel' lName' lRender' lPrompt' | (lHref', lRel', lName', lRender', lPrompt') <- shrink (lHref, lRel, lName, lRender, lPrompt) ]

instance Arbitrary Item where
  arbitrary = Item <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary

  shrink Item{..} = [ Item iHref' iData' iLinks' | (iHref', iData', iLinks') <- shrink (iHref, iData, iLinks) ]

instance Arbitrary Query where
  arbitrary = Query <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary

  shrink Query{..} = [ Query qHref' qRel' qName' qPrompt' qData' | (qHref', qRel', qName', qPrompt', qData') <- shrink (qHref, qRel, qName, qPrompt, qData) ]

instance Arbitrary Template where
  arbitrary = Template <$> arbitrary

  shrink Template{..} = [ Template tData' | tData' <- shrink tData ]

instance Arbitrary Error where
  arbitrary = Error <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary

  shrink Error{..} = [ Error eTitle' eCode' eMessage' | (eTitle', eCode', eMessage') <- shrink (eTitle, eCode, eMessage) ]

instance Arbitrary Datum where
  arbitrary = Datum <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary

  shrink Datum{..} = [ Datum dName' dValue' dPrompt' | (dName', dValue', dPrompt') <- shrink (dName, dValue, dPrompt) ]
