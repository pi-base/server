{-# LANGUAGE TemplateHaskell #-}
module Build
  ( Info(..)
  , info
  ) where

import Protolude

import           Data.Aeson (ToJSON)
import qualified Data.Text  as T

import Util.TH (buildEnv)

data Info = Info
  { number :: Text
  , sha    :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Info

info :: Maybe Info
info = do
  number <- $(buildEnv "CIRCLE_BUILD_NUM")
  sha    <- $(buildEnv "CIRCLE_SHA1")
  return $ Info (T.pack number) (T.pack sha)
