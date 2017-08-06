{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Internal.Network.URI.Tests
Description : URI Arbitrary Instances
Copyright   : (c) Alex Brandt, 2017
License     : MIT

A collection of 'Arbitrary' instances for 'URI'.
-}
module Internal.Network.URI.Tests where

import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
import Network.URI (parseURIReference, URI)
import Test.QuickCheck (Arbitrary (arbitrary), elements)

-- | Not a general implementation.
--
--   This implementation just returns a random URI from the
--   @application/vnd.collection+json@ examples.
instance Arbitrary URI where
  arbitrary = fromJust . parseURIReference <$> elements uris
    where uris = [ "http://example.org/friends/"
                 , "http://example.org/friends/"
                 , "http://example.org/friends/rss"
                 , "http://example.org/friends/jdoe"
                 , "http://examples.org/blogs/jdoe"
                 , "http://examples.org/images/jdoe"
                 , "http://example.org/friends/msmith"
                 , "http://examples.org/blogs/msmith"
                 , "http://examples.org/images/msmith"
                 , "http://example.org/friends/rwilliams"
                 , "http://examples.org/blogs/rwilliams"
                 , "http://examples.org/images/rwilliams"
                 , "http://example.org/friends/search"
                 , "http://example.org/friends/"
                 , "http://example.org/friends/rss"
                 , "http://example.org/friends/?queries"
                 , "http://example.org/friends/?template"
                 , "http://example.org/friends/jdoe"
                 , "http://examples.org/blogs/jdoe"
                 , "http://examples.org/images/jdoe"
                 , "http://example.org/friends/"
                 , "http://example.org/friends/search"
                 , "http://example.org/friends/"
                 , "http://example.org/friends/"
                 ]
