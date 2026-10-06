module Main (main) where

import Test.Tasty (defaultMain)

import qualified Data.CollectionJSONTest (tests)

main :: IO ()
main = defaultMain Data.CollectionJSONTest.tests
