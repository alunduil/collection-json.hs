{-|
Module      : Main
Copyright   : (c) Alex Brandt, 2017
License     : MIT

"Main" Module for collection-json property tests.
-}
module Main where

import System.Exit (exitFailure, exitSuccess)

import qualified Data.CollectionJSON.Tests

main :: IO ()
main =
  do success <- and <$> sequence [ Data.CollectionJSON.Tests.runTests ]
     if success then exitSuccess else exitFailure
