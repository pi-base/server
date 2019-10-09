{-# LANGUAGE TemplateHaskell #-}
module Server.Status
  ( Status(..)
  , build
  , mkStatus
  ) where

import Core
import Util.TH (buildEnv) -- TODO: gitHash

data Status = Status
  { started   :: UTCTime
  , buildInfo :: Maybe String
  } deriving (Generic, Eq, Show)

mkStatus :: UTCTime -> Status
mkStatus started = Status
  { started
  , buildInfo = $(buildEnv "BUILD")
  }

build :: MonadIO m => m Status
build = liftIO $ fmap mkStatus getCurrentTime
