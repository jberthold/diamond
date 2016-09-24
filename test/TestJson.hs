{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

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
      canParseAs @CfPageBody     $ resource "requestbodies.json"
    , testProperty "parse responses" $
      canParseAs @CfResponse     $ resource "responses.json"
    , testProperty "parse response lists again" $
      canParseAs @CfResponseList $ resource "responselists.json"
    ]

resource :: FilePath -> FilePath
resource = ("test" </>) . ("resources" </>)

-- using a type argument
canParseAs :: forall a . A.FromJSON a => FilePath -> Property
canParseAs file = monadicIO $
          do json <- liftIO $ B.readFile file
             let content :: Either String [a]
                 content = A.eitherDecode json
             assert (isRight content)

roundTripJson :: forall a . (A.ToJSON a, A.FromJSON a, Eq a) => a -> Bool
roundTripJson dat = isRight answer && dat == dat'
  where answer     = (A.eitherDecode . A.encode) dat
        Right dat' = answer 
       
