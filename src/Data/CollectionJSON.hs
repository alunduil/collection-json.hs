{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 
module Text.JSON.CollectionJSON.Types where
import Data.Aeson hiding (Error)
import Data.Text (Text)
import Control.Applicative

data Collection = Collection {
    cVersion :: Text
  , cHref :: Text
  , cLinks :: [Link]
  , cItems :: [Item]
  , cQueries :: [Query]
  , cTemplate :: Maybe Template
  , cError :: Maybe Error
  } deriving Show

data Error = Error {
    eTitle :: Text
  , eCode :: Maybe Text
  , eMessage :: Maybe Text
  } deriving Show

{- Each has five possible properties: href (REQUIRED), rel (REQURIED), name (OPTIONAL), render (OPTIONAL), and prompt, (OPTIONAL).
-}

data Link = Link {
    lHref :: Text
  , lRel :: Text
  , lName :: Maybe Text
  , lRender :: Maybe Text
  , lPrompt :: Maybe Text
  } deriving Show
  
{- The queries array SHOULD contain one or more anonymous objects. Each object composed of five possible properties: href (REQUIRED), rel (REQUIRED), name (OPTIONAL), prompt (OPTIONAL), and a data array (OPTIONAL). -}

data Query = Query {
    qHref :: Text
  , qRel :: Text
  , qName :: Maybe Text
  , qPrompt :: Maybe Text
  , qData :: [Data]
  } deriving Show

{- The data array SHOULD contain one or more anonymous objects. Each 
object MAY have any of three possible properties: name (REQUIRED), value
(OPTIONAL), and prompt (OPTIONAL). -}

data Data = Data { 
    dName :: Text
  , dValue :: Maybe Text
  , dPrompt :: Maybe Text
  } deriving Show

{- Each element in the items array SHOULD contain an href property. The
href property MUST contain a URI. This URI MAY be used to retrieve a
Collection+JSON document representing the associated item. It MAY be used
to edit or delete the associated item. See Reading and Writing Data for
details.

The items array MAY have a data array child property.

The items array MAY have a links array child property.
-}

data Item = Item {
    iHref :: Text
  , iData :: [Data]
  , iLinks :: [Link]
  } deriving Show

{- The template object SHOULD have a data array child property. -}


data Template = Template {
    tData :: [Data]
  } deriving Show
 
------------------------------------------------------------------------
-- FromJSON instances

instance FromJSON Template where
    parseJSON (Object v) = Template <$> v .: "data" 

instance FromJSON Item where
    parseJSON (Object v) = Item 
        <$> v .: "href" 
        <*> v .:? "data" .!= [] 
        <*> v .:? "links" .!= []

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .: "name"
        <*> v .:? "value"
        <*> v .:? "prompt"

instance FromJSON Query where
    parseJSON (Object v) = Query
        <$> v .: "href"
        <*> v .: "rel"
        <*> v .:? "name"
        <*> v .:? "prompt"
        <*> v .:? "data" .!= []

instance FromJSON Link where
    parseJSON (Object v) = Link
        <$> v .: "href"
        <*> v .: "rel"
        <*> v .:? "name"
        <*> v .:? "render"
        <*> v .:? "prompt"

instance FromJSON Error where
    parseJSON (Object v) = Error
        <$> v .: "title"
        <*> v .:? "code"
        <*> v .:? "message"

instance FromJSON Collection where
    parseJSON (Object v) = Collection 
        <$> v .: "version" 
        <*> v .: "href" 
        <*> v .:? "links" .!= []
        <*> v .:? "items" .!= []
        <*> v .:? "queries" .!= []
        <*> v .:? "template" 
        <*> v .:? "error" 

------------------------------------------------------------------------

instance ToJSON Template where
    toJSON Template{..} = object [ "data" .= tData ]

instance ToJSON Item where
    toJSON Item{..} = object [ 
        "href" .= iHref
      , "data" .= iData
      , "links" .= iLinks
      ]

instance ToJSON Data where
    toJSON Data{..} = object [ 
        "name" .= dName
      , "data" .= dValue
      , "prompt" .= dPrompt
      ]

instance ToJSON Query where
    toJSON Query{..} = object [ 
        "href" .= qHref
      , "rel" .= qRel
      , "name" .= qName
      , "prompt" .= qPrompt
      , "data" .= qData
      ]

instance ToJSON Link where
    toJSON Link{..} = object [ 
        "href" .= lHref
      , "rel" .= lRel
      , "name" .= lName
      , "render" .= lRender
      , "prompt" .= lPrompt
      ]

instance ToJSON Error where
    toJSON Error{..} = object [ 
        "title" .= eTitle
      , "code" .= eCode
      , "message" .= eMessage
      ]

instance ToJSON Collection where
    toJSON Collection{..} = object [ 
        "version" .= cVersion
      , "href" .= cHref
      , "links" .= cLinks
      , "items" .= cItems
      , "queries" .= cQueries
      , "template" .= cTemplate
      , "error" .= cError
      ]
