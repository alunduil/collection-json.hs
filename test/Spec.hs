module Main (main) where

import Test.Tasty (defaultMain)

import qualified Data.CollectionJSONSpec (tests)

main :: IO ()
main = defaultMain Data.CollectionJSONSpec.tests
