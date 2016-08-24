{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Web.Diamond.API
       where

import Data.Text(Text)
import Data.Aeson.Types
import GHC.Generics
import Data.Proxy

import Servant.API
import Servant.Client

import Text.Web.Diamond.Types

-- | The Confluence API is very rich, but we only implement a small subset of
-- the content API (for a specific use). Commented code prepares for
-- implementing more.
type ConfluenceAPI =
  "rest" :> "api" :> (
    ContentAPI
    -- :<|> AuditAPI -- auditing changes
    -- :<|> GroupAPI -- user group operations
    -- :<|> longtask -- API for LongTaskService (?)
    -- :<|> SearchAPI   -- only one search method
    -- :<|> SpaceAPI    -- spaces and their key/value property stores 
    -- :<|> TemplateAPI -- content templates
    -- :<|> UserAPI  -- user management
    )
-- matching out a related client
contentClient -- :<|> more APIs
  = client (Proxy :: Proxy ConfluenceAPI)

-- | Content has sub-APIS for child, child/attachment, descendant, label,
-- property, restriction, version, blueprint.
-- We implement a minimal subset geared towards creating and updating pages
-- with a fixed parent page, and listing existing content. Structure is
-- different from the original documentation
type ContentAPI = QueryAPI       -- list and read content
                  :<|> UpdateAPI -- update or create pages

quClient :<|> updClient = contentClient
  
type QueryAPI =
  "content" :> (
                QueryParam "type" CfContentType -- {page, blogpost}
                :> QueryParam "spaceKey" Text
                :> QueryParam "title" Text  -- page title to search for
                                            -- required if type=page
                :> QueryParam "postingDay" Text -- as name suggests yyyy-mm-dd
                                                -- required if type=blogpost
--                :> QueryParam "start" Int -- for pagination
--                :> QueryParam "limit" Int -- for pagination
--                :> QueryParam "expand" [Text] -- details to provide UNUSED
--                :> QueryParam "status" [CfStatus] -- {current, trashed, any}
                :> Get '[JSON] CfResponse
    :<|> 
                Capture "id" Int
                :> QueryParam "version" Int -- latest if not provided
--                :> QueryParam "status" [CfStatus] -- {current, trashed, any}
--                :> QueryParam "expand" [Text] -- details to provide UNUSED
                :> Get '[JSON] CfResponse
               )

cfList :<|> cfGet = quClient

type UpdateAPI =
  "content" :>
  (             -- create a new page
                ReqBody '[JSON] CfPageBody
--                :> QueryParam "status" [CfStatus] -- {current, trashed, any}
--                :> QueryParam "expand" [Text] -- details to provide UNUSED
                :> Post '[JSON] CfResponse
   :<|>         -- update an existing page
                Capture "contentId" Int
                :> ReqBody '[JSON] CfPageBody
                :> Put '[JSON] CfResponse
  )

cfCreate :<|> cfUpdate = updClient
