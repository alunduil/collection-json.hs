{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Data.CollectionJSON
Description : Types and Instances for @application/vnd.collection+json@
Copyright   : (c) Alex Brandt, 2017
License     : MIT

A collection of types and instances for @application/vnd.collection+json@.

Full documentation for @application/vnd.collection+json@ can be found at
<http://amundsen.com/media-types/collection/>.

Every @href@ ('cHref', 'lHref', 'iHref', and 'qHref') decodes as a URI
reference, so a relative address is valid. It resolves against the
address the document was retrieved from, as described in
[RFC 3986 section 5.1.3](https://www.rfc-editor.org/rfc/rfc3986#section-5.1.3).

A 'Collection' with no @href@ decodes to the empty reference, which
resolves to that same retrieval address.

Decoding accepts every document the format permits and rejects only
what it forbids.
-}
module Data.CollectionJSON (
  -- * Core Data Types
  Collection (..),
  Link (..),
  Render (..),
  Item (..),
  Query (..),
  Template (..),
  Error (..),
  Datum (..),
  DatumValue (..),

  -- * Type Conversion
  FromCollection (..),
  ToCollection (..),
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Bool, Number, String), object, withObject, withText, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.Types (Pair, typeMismatch)
import Data.Maybe (catMaybes)
import Data.Scientific (Scientific)
import Data.Text (Text, unpack)
import Network.URI (URI, parseURIReference)

-- * Core Data Types

-- | The top-level object for an @application/vnd.collection+json@ resource.
data Collection = Collection
  { cVersion :: Text
  {- ^ Version the document declares, or "1.0" when it declares none.
  Checking it is the caller's job.
  -}
  , cHref :: URI
  {- ^ Address used to retrieve the 'Collection'
  and to add new elements.
  -}
  , cLinks :: [Link]
  , cItems :: [Item]
  , cQueries :: [Query]
  , cTemplate :: Maybe Template
  , cError :: Maybe Error
  }
  deriving (Eq, Show)

instance FromJSON Collection where
  parseJSON = withObject "Collection" $ \c -> do
    v <- c .: "collection"

    {- Rejecting a version other than "1.0" would put every other field
    out of reach the day a later version ships.
    -}
    cVersion <- v .:? "version" .!= "1.0"
    cHref <- v .:? "href" .!= "" >>= parseHref
    cLinks <- v .:? "links" .!= []
    cItems <- v .:? "items" .!= []
    cQueries <- v .:? "queries" .!= []
    cTemplate <- v .:? "template"
    cError <- v .:? "error"

    pure Collection{..}

instance ToJSON Collection where
  toJSON Collection{..} =
    object
      [ "collection"
          .= object
            ( catMaybes
                [ Just $ "version" .= cVersion
                , Just $ "href" .= cHref
                , omitEmpty "links" cLinks
                , omitEmpty "items" cItems
                , omitEmpty "queries" cQueries
                , (.=) "template" <$> cTemplate
                , (.=) "error" <$> cError
                ]
            )
      ]

{- |
A link to a related resource (not necessarily an
@application/vnd.collection+json@ resource).
-}
data Link = Link
  { lHref :: URI
  -- ^ Address of the resource.
  , lRel :: Text
  {- ^ Relation---the following contain suggested
  relation values:

  * [IANA Link Relations](http://www.iana.org/assignments/link-relations/link-relations.xml)
  * [Microformat Existing Rel Values](http://microformats.org/wiki/existing-rel-values)
  * [RFC5988](http://tools.ietf.org/html/rfc5988)
  -}
  , lName :: Maybe Text
  , lRender :: Maybe Render
  -- ^ Absent means 'RenderLink'.
  , lPrompt :: Maybe Text
  }
  deriving (Eq, Show)

instance FromJSON Link where
  parseJSON = withObject "Link" $ \v -> do
    lHref <- v .: "href" >>= parseHref
    lRel <- v .: "rel"
    lName <- v .:? "name"
    lRender <- v .:? "render"
    lPrompt <- v .:? "prompt"

    pure Link{..}

instance ToJSON Link where
  toJSON Link{..} =
    object $
      catMaybes
        [ Just $ "href" .= lHref
        , Just $ "rel" .= lRel
        , (.=) "name" <$> lName
        , (.=) "render" <$> lRender
        , (.=) "prompt" <$> lPrompt
        ]

-- | How a user agent should present the resource a 'Link' addresses.
data Render
  = -- | Embed the resource in the display.
    RenderImage
  | -- | Offer the resource as a link to follow.
    RenderLink
  deriving (Eq, Show)

instance FromJSON Render where
  parseJSON = withText "Render" $ \t -> case t of
    "image" -> pure RenderImage
    "link" -> pure RenderLink
    _ -> fail $ "render must be \"image\" or \"link\", not " <> show t

instance ToJSON Render where
  toJSON RenderImage = String "image"
  toJSON RenderLink = String "link"

-- | An element in the 'Collection'
data Item = Item
  { iHref :: Maybe URI
  {- ^ Address of the resource used to retrieve, modify, or
  delete the element. An 'Item' that omits it can only be read
  through its enclosing 'Collection'.
  -}
  , iData :: [Datum]
  , iLinks :: [Link]
  }
  deriving (Eq, Show)

instance FromJSON Item where
  parseJSON = withObject "Item" $ \v -> do
    {- Resolving an absent item href to the empty reference, as
    'Collection' does, would hand back the collection's own address as
    though it were the item's.
    -}
    iHref <- v .:? "href" >>= traverse parseHref
    iData <- v .:? "data" .!= []
    iLinks <- v .:? "links" .!= []

    pure Item{..}

instance ToJSON Item where
  toJSON Item{..} =
    object $
      catMaybes
        [ (.=) "href" <$> iHref
        , omitEmpty "data" iData
        , omitEmpty "links" iLinks
        ]

{- |
A template for possible queries related to this 'Collection'.

A query should correspond to an associated HTTP GET request.

The Query:
> Query "http://example.com/search" "search" Nothing (Just "Search:") [Datum "search" (Just (DatumString "")) Nothing]

Corresponds with the following URI for an HTTP GET:
> http://example.com/search?search={search_terms}
-}
data Query = Query
  { qHref :: URI
  -- ^ Address of reqeust's target.
  , qRel :: Text
  {- ^ Relation---the following contain suggested
  relation values:

  * [IANA Link Relations](http://www.iana.org/assignments/link-relations/link-relations.xml)
  * [Microformat Existing Rel Values](http://microformats.org/wiki/existing-rel-values)
  * [RFC5988](http://tools.ietf.org/html/rfc5988)
  -}
  , qName :: Maybe Text
  -- ^ Identifier for this 'Query'.
  , qPrompt :: Maybe Text
  -- ^ Suggested user prompt.
  , qData :: [Datum]
  -- ^ Query parameters for this 'Query'.
  }
  deriving (Eq, Show)

instance FromJSON Query where
  parseJSON = withObject "Query" $ \v -> do
    qHref <- v .: "href" >>= parseHref
    qRel <- v .: "rel"
    qName <- v .:? "name"
    qPrompt <- v .:? "prompt"
    qData <- v .:? "data" .!= []

    pure Query{..}

instance ToJSON Query where
  toJSON Query{..} =
    object $
      catMaybes
        [ Just $ "href" .= qHref
        , Just $ "rel" .= qRel
        , (.=) "name" <$> qName
        , (.=) "prompt" <$> qPrompt
        , omitEmpty "data" qData
        ]

-- | A fillable template for creation of a new object in the 'Collection'.
newtype Template = Template
  { tData :: [Datum]
  }
  deriving (Eq, Show)

instance FromJSON Template where
  parseJSON = withObject "Template" $ \v -> do
    tData <- v .:? "data" .!= []

    pure Template{..}

instance ToJSON Template where
  toJSON Template{..} =
    object
      [ "data" .= tData
      ]

-- | Information about latest error that occured when responding to a request.
data Error = Error
  { eTitle :: Maybe Text
  , eCode :: Maybe Text
  {- ^ Unique identifier (e.g. session identifier,
  request tracker, etc).
  -}
  , eMessage :: Maybe Text
  }
  deriving (Eq, Show)

instance FromJSON Error where
  parseJSON = withObject "Error" $ \v -> do
    eTitle <- v .:? "title"
    eCode <- v .:? "code"
    eMessage <- v .:? "message"

    pure Error{..}

instance ToJSON Error where
  toJSON Error{..} =
    object $
      catMaybes
        [ (.=) "title" <$> eTitle
        , (.=) "code" <$> eCode
        , (.=) "message" <$> eMessage
        ]

-- | Contents of a 'Collection' 'Item'.
data Datum = Datum
  { dName :: Text
  -- ^ Identifier for this 'Datum'.
  , dValue :: Maybe DatumValue
  {- ^ 'Nothing' for both an absent @value@ and an explicit @null@,
  which the format treats alike.
  -}
  , dPrompt :: Maybe Text
  -- ^ Suggested user prompt.
  }
  deriving (Eq, Show)

instance FromJSON Datum where
  parseJSON = withObject "Datum" $ \v -> do
    dName <- v .: "name"
    dValue <- v .:? "value"
    dPrompt <- v .:? "prompt"

    pure Datum{..}

instance ToJSON Datum where
  toJSON Datum{..} =
    object $
      catMaybes
        [ Just $ "name" .= dName
        , (.=) "value" <$> dValue
        , (.=) "prompt" <$> dPrompt
        ]

{- |
A scalar carried by a 'Datum'. The format admits no nested structure
here, so an object or an array in a @value@ fails to decode.
-}
data DatumValue
  = DatumString Text
  | DatumNumber Scientific
  | DatumBool Bool
  deriving (Eq, Show)

instance FromJSON DatumValue where
  parseJSON (String t) = pure $ DatumString t
  parseJSON (Number n) = pure $ DatumNumber n
  parseJSON (Bool b) = pure $ DatumBool b
  parseJSON v = typeMismatch "DatumValue" v

instance ToJSON DatumValue where
  toJSON (DatumString t) = toJSON t
  toJSON (DatumNumber n) = toJSON n
  toJSON (DatumBool b) = toJSON b

-- * Type Conversion

-- | A type that can be converted from 'Collection'.
class FromCollection a where
  fromCollection :: Collection -> a

instance FromCollection Collection where
  fromCollection = id

-- | A type that can be converted to 'Collection'.
class ToCollection a where
  toCollection :: a -> Collection

instance ToCollection Collection where
  toCollection = id

{- Every array in the format is optional, and an empty one says nothing
a missing one doesn't.
-}
omitEmpty :: ToJSON a => Key -> [a] -> Maybe Pair
omitEmpty k xs = if null xs then Nothing else Just (k .= xs)

{- aeson's @FromJSON URI@ accepts absolute URIs only. Decoding through it
would break round-tripping, because the @href@ fields hold an
unconstrained 'URI' and @ToJSON URI@ encodes relative references.
Enforcing absolute addresses needs a type that cannot hold a relative
one.
-}
parseHref :: MonadFail m => Text -> m URI
parseHref = maybe (fail "invalid href URI") pure . parseURIReference . unpack
