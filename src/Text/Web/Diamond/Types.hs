{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Web.Diamond.Types
       where

import Data.Text(Text)
import Data.Aeson.Types
import GHC.Generics

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
  , space :: CfObject    -- ^ containing Conf.space, stored as JSON Object
  , history :: CfObject  -- ^ editing metadata (original creator + time etc)
  , version :: CfVersion -- ^ current version information (incl. number!)

-- , extensions :: CfObject -- ^ unclear what this is for
  , _links      :: CfObject -- ^ links related to the returned ID
  , _expandable :: CfObject -- ^ more details (must be requested by
                            -- "?expand=item1,item2,..."), not implemented

-- editing (updating) a page returned additionally 
    --  , ancestors :: [ CfObject ] -- ^ "hierarchy" (or is it? :-) between things
    --  , container :: CfObject -- ^ the containing space
    --  , body :: CfObject -- ^ contains "storage" and "_expandable"
  } deriving (Eq, Read, Show, Generic)

instance ToJSON CfResponse
instance FromJSON CfResponse

-- | CfObject is a "we don't bother" object to avoid implementing specific
-- fields we won't be interested in. anything important or perceived as
-- reasonably stable in Confluence should become a specific type.
type CfObject = Object

----------------------------------------
-- | version information for confluence things
data CfVersion =
  CfVersion { by   :: CfPerson
            , when :: CfTime
            , message   :: Text
            , number    :: Int
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
