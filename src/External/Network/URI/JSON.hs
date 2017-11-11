{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : External.Network.URI.JSON
Description : URI FromJSON and ToJSON Instances
Copyright   : (c) Alex Brandt, 2017
License     : MIT

URI Instances for FromJSON and ToJSON
-}
module External.Network.URI.JSON where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import Data.Text (unpack)
import Network.URI (parseURIReference, URI, uriToString)

instance FromJSON URI where
  parseJSON = withText "URI" $ maybe (fail "invalid URI") return . parseURIReference . unpack

instance ToJSON URI where
  toJSON u = toJSON $ uriToString id u ""
