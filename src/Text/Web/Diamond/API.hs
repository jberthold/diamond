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

import Servant.API
import Servant.Client

import Text.Web.Diamond.Types

type ConfluenceAPI =
  "rest" :> "api" :> (
    -- AuditAPI :<|> -- auditing changes
    ContentAPI :<|> 
    -- GroupAPI :<|> -- user group operations
    -- longtask :<|> -- API for LongTaskService (?)
    SearchAPI :<|>   -- only one search method
    -- SpaceAPI :<|>    -- spaces and their key/value property stores 
    -- TemplateAPI :<|> -- content templates
    UserAPI
    )

-- | Content has sub-APIS for child, child/attachment, descendant, label,
-- property, restriction, version, blueprint
type ContentAPI =
  "this" :> "is" :> "much" :> "stuff" :> Get '[JSON] ()
  -- TODO

-- | The search interface uses the "Confluence Query Language" cql
type SearchAPI =
  "search" :> QueryParam "cql" Text :>
              QueryParam "cqlcontext" Text :>
              QueryParam "excerpt" Text :>
              QueryParam "expand" Text :>
              QueryParam "start" Int :>
              QueryParam "limit" Int :>
              QueryParam "includeArchivedSpace" Bool :>
              Get '[JSON] [SearchResult]


type UserAPI = ContentAPI
  
