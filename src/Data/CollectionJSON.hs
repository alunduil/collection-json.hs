{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Data.CollectionJSON
Description : Types and Instances for @application/vnd.collection+json@
Copyright   : (c) Alex Brandt, 2017
License     : MIT

A collection of types and instances for @application/vnd.collection+json@.

Full documentation for @application/vnd.collection+json@ can be found at
<http://amundsen.com/media-types/collection/>.
-}
module Data.CollectionJSON where

import Data.Aeson ((.=), (.:?), (.!=), (.:), FromJSON (parseJSON), object, ToJSON (toJSON), withObject)
import Data.Text (Text)
import Network.URI (URI)

import Internal.Network.URI ()

-- * Core Data Types

-- | The top-level object for an @application/vnd.collection+json@ resource.
data Collection = Collection
  { cVersion  :: Text           -- ^ Currently, always "1.0".
  , cHref     :: URI            -- ^ Address used to retrieve the 'Collection'
                                --   and to add new elements.
  , cLinks    :: [Link]
  , cItems    :: [Item]
  , cQueries  :: [Query]
  , cTemplate :: Maybe Template
  , cError    :: Maybe Error
  }

instance FromJSON Collection where
  parseJSON = withObject "Collection" $ \ c -> do
    v <- c .: "collection"

    cVersion  <- v .:? "version"  .!= "1.0"
    cHref     <- v .:  "href"
    cLinks    <- v .:? "links"    .!= []
    cItems    <- v .:? "items"    .!= []
    cQueries  <- v .:? "queries"  .!= []
    cTemplate <- v .:? "template"
    cError    <- v .:? "error"

    return Collection{..}

instance ToJSON Collection where
  toJSON Collection{..} = object
    [ "collection" .= object
      [ "version"  .= cVersion
      , "href"     .= cHref
      , "links"    .= cLinks
      , "items"    .= cItems
      , "queries"  .= cQueries
      , "template" .= cTemplate
      , "error"    .= cError
      ]
    ]

{-|
A link to a related resource (not necessarily an
@application/vnd.collection+json@ resource).
-}
data Link = Link
  { lHref   :: URI        -- ^ Address of the resource.
  , lRel    :: Text       -- ^ Relation---the following contain suggested
                          --   relation values:
                          --
                          --   * [IANA Link Relations](http://www.iana.org/assignments/link-relations/link-relations.xml)
                          --   * [Microformat Existing Rel Values](http://microformats.org/wiki/existing-rel-values)
                          --   * [RFC5988](http://tools.ietf.org/html/rfc5988)
  , lName   :: Maybe Text
  , lRender :: Maybe Text
  , lPrompt :: Maybe Text
  }

instance FromJSON Link where
  parseJSON = withObject "Link" $ \ v -> do
    lHref   <- v .:  "href"
    lRel    <- v .:  "rel"
    lName   <- v .:? "name"
    lRender <- v .:? "render"
    lPrompt <- v .:? "prompt"

    return Link{..}

instance ToJSON Link where
  toJSON Link{..} = object
    [ "href"   .= lHref
    , "rel"    .= lRel
    , "name"   .= lName
    , "render" .= lRender
    , "prompt" .= lPrompt
    ]

-- | An element in the 'Collection'
data Item = Item
  { iHref  :: URI    -- ^ Address of the resource used to retrieve, modify, or
                     --   delete the element.
  , iData  :: [Datum]
  , iLinks :: [Link]
  }

instance FromJSON Item where
  parseJSON = withObject "Item" $ \ v -> do
    iHref  <- v .:  "href"
    iData  <- v .:? "data"  .!= []
    iLinks <- v .:? "links" .!= []

    return Item{..}

instance ToJSON Item where
  toJSON Item{..} = object
    [ "href"  .= iHref
    , "data"  .= iData
    , "links" .= iLinks
    ]

{-|
A template for possible queries related to this 'Collection'.

A query should correspond to an associated HTTP GET request.

The Query:
> Query "http://example.com/search" "search" Nothing (Just "Search:") [Datum "search" "" Nothing]

Corresponds with the following URI for an HTTP GET:
> http://example.com/search?search={search_terms}
-}
data Query = Query
  { qHref   :: URI        -- ^ Address of reqeust's target.
  , qRel    :: Text       -- ^ Relation---the following contain suggested
                          --   relation values:
                          --
                          --   * [IANA Link Relations](http://www.iana.org/assignments/link-relations/link-relations.xml)
                          --   * [Microformat Existing Rel Values](http://microformats.org/wiki/existing-rel-values)
                          --   * [RFC5988](http://tools.ietf.org/html/rfc5988)
  , qName   :: Maybe Text -- ^ Identifier for this 'Query'.
  , qPrompt :: Maybe Text -- ^ Suggested user prompt.
  , qData   :: [Datum]    -- ^ Query parameters for this 'Query'.
  }

instance FromJSON Query where
  parseJSON = withObject "Query" $ \ v -> do
    qHref   <- v .:  "href"
    qRel    <- v .:  "rel"
    qName   <- v .:? "name"
    qPrompt <- v .:? "prompt"
    qData   <- v .:? "data"   .!= []

    return Query{..}

instance ToJSON Query where
  toJSON Query{..} = object
    [ "href"   .= qHref
    , "rel"    .= qRel
    , "name"   .= qName
    , "prompt" .= qPrompt
    , "data"   .= qData
    ]

-- | A fillable template for creation of a new object in the 'Collection'.
newtype Template = Template
  { tData :: [Datum]
  }

instance FromJSON Template where
  parseJSON = withObject "Template" $ \ v -> do
    tData <- v .:? "data" .!= []

    return Template{..}

instance ToJSON Template where
  toJSON Template{..} = object
    [ "data" .= tData
    ]

-- | Information about latest error that occured when responding to a request.
data Error = Error
  { eTitle   :: Maybe Text
  , eCode    :: Maybe Text -- ^ Unique identifier (e.g. session identifier,
                           --   request tracker, etc).
  , eMessage :: Maybe Text
  }

instance FromJSON Error where
  parseJSON = withObject "Error" $ \ v -> do
    eTitle   <- v .:? "title"
    eCode    <- v .:? "code"
    eMessage <- v .:? "message"

    return Error{..}

instance ToJSON Error where
  toJSON Error{..} = object
    [ "title"   .= eTitle
    , "code"    .= eCode
    , "message" .= eMessage
    ]

-- | Contents of a 'Collection' 'Item'.
data Datum = Datum
  { dName   :: Text       -- ^ Identifier for this 'Datum'.
  , dValue  :: Maybe Text
  , dPrompt :: Maybe Text -- ^ Suggested user prompt.
  }

instance FromJSON Datum where
  parseJSON = withObject "Datum" $ \ v -> do
    dName   <- v .:  "name"
    dValue  <- v .:? "value"
    dPrompt <- v .:? "prompt"

    return Datum{..}

instance ToJSON Datum where
  toJSON Datum{..} = object
    [ "name"   .= dName
    , "value"  .= dValue
    , "prompt" .= dPrompt
    ]

-- * Type Conversion

-- | A type that can be converted from 'Collection'.
class FromCollection a where
  fromCollection :: Collection -> a

-- | A type that can be converted to 'Collection'.
class ToCollection a where
  toCollection :: a -> Collection
