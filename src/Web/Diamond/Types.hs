{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Web.Diamond.Types
       where

import Data.Text(Text, pack, unpack)
import Data.Aeson.Types
import GHC.Generics
import Data.Maybe

import Web.HttpApiData

-- dummy for now, not sure if we will ever implement it
data SearchResult =
  SearchResult {
               }
  deriving (Eq, Read, Show, Generic)

-- | successful action response from Confluence. Solid specification of the
-- fields is lacking, so this is done on a by-need basis.
data CfResponse = CfResponse
  { id     :: Text -- ^ object ID in Confluence

    -- , type :: CfType -- very inconvenient name. Not required for now...
  , status :: Text -- ^ status of content (often "current")
  , title  :: Text -- ^ title of content
  , version :: Maybe CfVersion -- ^ current version information (incl. number)
                               -- not present in CfResponseList

--  , extensions :: CfObject -- ^ unclear what this is for
  , _links      :: CfObject -- ^ links related to the returned ID
  , _expandable :: CfObject -- ^ more details (must be requested by
                            -- "?expand=item1,item2,..."), not implemented
    -- expandable things, missing in the CfResponseList for rest/api/content
    --  , space :: Maybe CfObject    -- ^ containing Conf.space
    --  , history :: Maybe CfObject  -- ^ editing metadata (creator, time etc)
-- editing (updating) a page returned additionally 
    --  , ancestors :: [ CfObject ] -- ^ hierarchy (or is it? :-) between things
    --  , container :: CfObject -- ^ the containing space
  , body :: Maybe CfResponseBody -- ^ page body within "body.storage.value"
  } deriving (Eq, Read, Show, Generic)

instance ToJSON CfResponse
instance FromJSON CfResponse

-- | CfObject is a "we don't bother" object to avoid implementing specific
-- fields we won't be interested in. anything important or perceived as
-- reasonably stable in Confluence should become a specific type.
type CfObject = Object

-- | optional page body in a response (need to request "body.storage" to
-- expand it in a cfGet). Similar but not equal to body in a request :-)
data CfResponseBody = CfResponseBody { content :: Text }
     deriving (Eq, Read, Show, Generic)

instance ToJSON CfResponseBody where
  toJSON CfResponseBody{..} =
    object [ "storage" .= object [ "representation" .= string "storage"
                                 , "value" .= content
                                 ]
           ]
    where string = Data.Aeson.Types.String

instance FromJSON CfResponseBody where
  parseJSON (Object o) = CfResponseBody <$>
                         -- could check "representation": "storage"
                         ((.: "value") =<< o .: "storage")
  parseJSON other      = typeMismatch "page body object" other

-- | list (including size and pagination information) of CfResponse items,
-- used in content listing. Some expandable fields not present in this response
data CfResponseList = CfResponseList
  { results :: [ CfResponse ]
  , start   :: Int
  , limit   :: Int
  , size    :: Int
  , _links  :: CfObject
  } deriving (Eq, Read, Show, Generic)

instance ToJSON CfResponseList
instance FromJSON CfResponseList

----------------------------------------
-- | version information for confluence things
data CfVersion =
  CfVersion { by   :: CfPerson
            , when :: CfTime
            , number    :: Int
            , message   :: Maybe Text
            , minorEdit :: Bool
            } deriving (Eq, Read, Show, Generic)

instance FromJSON CfVersion
instance ToJSON   CfVersion

----------------------------------------
-- | models people in Confluence
data CfPerson =
  CfPerson -- ^ a known confluence User, "type": "known"
  { username :: Text
  , userKey  :: Text
  , profilePicture :: CfObject
  , displayName :: Text
  , _links   :: CfObject -- ^ contains one `self` link to the person's page
  }
--   | CfUnknown -- ^ type == ???
  deriving (Eq, Read, Show, Generic)

instance FromJSON CfPerson
instance ToJSON   CfPerson
-- will require distinction according to "type" field when CfUnknown is added

-- | Time representation for Confluence JSON. Example value
-- "2016-08-11T11:35:18.951+10:00"
type CfTime = Text

------------------------------------------------------------
-- Request data

-- | Page creation and update
data CfPageBody =
  CfPageBody { title     :: Text  -- ^ page title, mandatory! also for updates
             , ancestors :: [Int] -- ^ IDs, possibly empty (optional)
             , space     :: Maybe (Either Text Int) -- ^ space, by key or ID
             , body      :: Text      -- ^ will be in storage.value
             , version   :: Maybe Int -- ^ number, needs increment on update
             }
  deriving (Eq, Read, Show, Generic)

instance FromJSON CfPageBody where
  parseJSON (Object o)
    = do title     <- o .: "title"
         ancestors <- maybe (return []) (mapM ( .: "id")) =<< o .:? "ancestors"
         let spaceId (Object space') =
                 do key <- space' .:? "key"
                    iD  <- space' .:? "id"
                    return $ case (key, iD) of
                               (_, Just n) -> Just (Right n) -- prefer iD
                               (Just k, _) -> Just (Left k)
                               other       -> Nothing
         space    <- maybe (return Nothing) spaceId =<< o .:? "space"
         version  <- maybe (return Nothing) (.: "number") =<< o .:? "version"
         -- we should actually check that "representation": "storage" was given
         body     <- (.: "value") =<< (.: "storage") =<< o .: "body"
         return CfPageBody{..}

instance ToJSON CfPageBody where
  toJSON CfPageBody{..}
    = object $
      [ "type"      .= string "page"
      , "title"     .= title
      , "ancestors" .= [ object [ "id" .= n] | n <- ancestors ]
      , "body"      .= object [ "representation" .= string "storage"
                              , "storage" .= body
                              ]
      ]
      ++ catMaybes
      [ "space"     .=? fmap mkSpace space
      , "version"   .=? fmap mkVersion version
      ]
      where mkSpace :: Either Text Int -> Pair
            mkSpace (Left key) = "key" .= key
            mkSpace (Right iD) = "id"  .= iD
            mkVersion :: Int -> Pair
            mkVersion n = "version" .= object ["number" .= n ]
            string = Data.Aeson.Types.String

(.=?) field  = fmap (\mx -> field .= mx)

-- support types

-- | Confluence content type (for query): "page" or "blogpost".
data CfContentType = Page | Blogpost
                   deriving (Eq, Generic, Read, Show)

instance ToJSON CfContentType where
  toJSON = String . showTextData
instance FromJSON CfContentType where
  parseJSON (String s) = either (fail . unpack) return $ readTextData s
  parseJSON other      = typeMismatch "String" other

instance ToHttpApiData CfContentType where
  toUrlPiece    = showTextData
instance FromHttpApiData CfContentType where
  parseUrlPiece = readTextData
