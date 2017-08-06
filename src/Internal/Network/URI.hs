{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Internal.Network.URI
Description : URI Helper Functions
Copyright   : (c) Alex Brandt, 2017
License     : MIT

URI utility functions that don't belong anywhere else.
-}
module Internal.Network.URI where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import Data.Text (unpack)
import Network.URI (parseURIReference, URI)

instance FromJSON URI where
  parseJSON = withText "URI" $ \ v ->
    case parseURIReference (unpack v) of
      Nothing -> fail "invalid URI"
      Just x  -> return x

instance ToJSON URI where
  toJSON = toJSON . show
