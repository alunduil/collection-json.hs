{-|
Module      : TestSuite
Copyright   : (c) Alex Brandt, 2017
License     : MIT

"Main" Module for collection-json tests.
-}
module TestSuite (tests) where

import Distribution.TestSuite (Test (Group))

import qualified Data.CollectionJSON.Tests

tests :: IO [Test]
tests = return [ Group "Data.CollectionJSON" True Data.CollectionJSON.Tests.tests
               ]
