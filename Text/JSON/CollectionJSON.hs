{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 
module Text.JSON.CollectionJSON where
import Data.Aeson
import Data.Text (Text)


data Collection = Collection {
    cVersion :: Text
  , cHref :: Text
  , cLinks :: [Link]
  , cItems :: [Item]
  , cQueries :: [Query]
  , cTemplate :: Template
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
 
