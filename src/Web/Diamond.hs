{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Diamond
  where

import Web.Diamond.API
import Web.Diamond.Types

import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Servant.Client
import Servant.API

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)


-- | Confluence Interaction type
type Confluence a = (BasicAuthData, BaseUrl) -> ExceptT ServantError IO a

-- TODO convenience functionality

-- | list of pages for a space (optional), returning titles and IDs (as Int)
list    :: Maybe Text -> Confluence [(Text, Int)]
list space = \(auth, baseUrl) -> do
  undefined

-- | reading a page (returns the page title and body)
getPage :: Int -> Confluence (Text, Text)
getPage iD =  \(auth, baseUrl) -> do
  undefined

-- | create a page, optionally as a child of another page. Returns newly
-- created page ID as Int
createPage :: Text -> Text -> Maybe Int -> Confluence Int
createPage title body parent =  \(auth, baseUrl) -> do
  undefined

-- | update a page: may update title, parent, and/or body
-- To implement this, the page will be read and then updated, using previous
-- information unless updated.
updatePage :: Int -> Maybe Text -> Maybe Int -> Maybe Text -> Confluence ()
updatePage iD newTitle newAncestors newContent = \(auth, baseUrl) -> do
  undefined-- GET iD >>= PUT . updateCfPageBody..


