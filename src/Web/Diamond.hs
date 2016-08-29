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
import qualified Data.ByteString.Char8 as B

import Data.Aeson
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS(tlsManagerSettings)

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class

import Servant.Client
import Servant.API

import System.IO
import Control.Exception



-- | Confluence Interaction type
type Confluence a = (BasicAuthData, BaseUrl) -> ExceptT ServantError IO a
 -- TODO use ReaderT?

-- TODO convenience functionality

-- | list of pages for a space (optional), returning titles and IDs (as Int)
list    :: Maybe Text -> Confluence [(Text, Int)]
list space = \(auth, baseUrl) -> do
  mgr   <- liftIO $ newManager tlsManagerSettings
  -- (auth, baseUrl) <- ask -- TODO use ReaderT?
  answer <- cfList auth (Just Page) space Nothing Nothing mgr baseUrl
  return [ (title, read (T.unpack id)) | CfResponse{..} <- results answer ]
  
-- | reading a page (returns the page title and body)
getPage :: Int -> Confluence (Text, Text)
getPage iD =  \(auth, baseUrl) -> do
  mgr   <- liftIO $ newManager tlsManagerSettings
  CfResponse{..} <- cfGet auth iD Nothing ["body.storage"] mgr baseUrl
  return (title, maybe (error "no body received") content body)

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


mkAuth :: IO BasicAuthData
mkAuth = do putStr "User: "
            basicAuthUsername <- B.getLine
            if (B.null basicAuthUsername)
              then fail "empty user"
              else do putStr "Password: "
                      basicAuthPassword <- withEcho False B.getLine
                      putChar '\n'
                      return BasicAuthData{..}

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
