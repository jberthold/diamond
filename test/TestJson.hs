{-# LANGUAGE ScopedTypeVariables #-}

module TestJson where

import Distribution.TestSuite.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- import           Data.Text(Text)
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as T

import Control.Monad.IO.Class
import Data.Either

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import          System.FilePath

import Web.Diamond.Types

tests :: IO [Test]
tests = return [ wildJsonParsing ]

wildJsonParsing :: Test
wildJsonParsing = testGroup "parsing JSON collected in the wild"
    [ testProperty "parse request bodies" $
      resource "requestbodies.json" `canParseAs` (undefined :: CfPageBody)
    , testProperty "parse responses" $
      resource "responses.json" `canParseAs` (undefined :: CfResponse)
    , testProperty "parse response lists" $
      resource "responselists.json" `canParseAs` (undefined :: CfResponseList)
    ]

resource :: FilePath -> FilePath
resource = ("test" </>) . ("resources" </>)

-- Using a lame type argument trick: we won't actually use the thing
canParseAs :: forall a . A.FromJSON a => FilePath -> a -> Property
file `canParseAs` thing = monadicIO $
          do json <- liftIO $ B.readFile file
             let content :: Either String [a]
                 content = A.eitherDecode json
             assert (isRight content)
