{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Settings where

import Import

import Control.Lens                (makeLenses)
import Control.Monad.Logger        (LogLevel(..))
import Data.Aeson                  (ToJSON(..), Value(..), (.=), object)
import Data.Time                   (UTCTime)
import Database.Persist.Postgresql (PostgresConf(..))
import Database.Persist.Sql        (ConnectionPool)

import qualified Build
import qualified Data.Store.Types       as Store
import qualified Graph.Types            as Graph
import qualified Logger.Types           as Logger
import qualified Services.Github.Types  as Github
import qualified Services.Rollbar.Types as Rollbar

data Settings = Settings
  { _frontendUrl    :: Text
  , _logLevel       :: LogLevel
  , _port           :: Int
  , _testMode       :: Bool

  , _databaseSettings :: PostgresConf
  , _githubSettings   :: Github.Settings
  , _graphSettings    :: Graph.Settings
  , _repoSettings     :: Store.Settings
  , _rollbarSettings  :: Rollbar.Settings
  } deriving (Show, Generic)

makeLenses ''Settings

instance ToJSON LogLevel where
  toJSON LevelError     = String "error"
  toJSON LevelWarn      = String "warn"
  toJSON LevelInfo      = String "info"
  toJSON LevelDebug     = String "debug"
  toJSON (LevelOther l) = String l

instance ToJSON PostgresConf where
  toJSON (PostgresConf str pool) = object
    [ "connStr"  .= decodeUtf8 str
    , "poolSize" .= pool
    ]

instance ToJSON Settings

data Foundation = Foundation
  { _appConnPool :: ConnectionPool
  , _appLogger   :: Logger.Logger
  , _appQueries  :: Maybe Graph.QueryCache
  , _appStore    :: Store.Store
  }

makeLenses ''Foundation

data Env = Env
  { _envBootTime   :: UTCTime
  , _envBuildInfo  :: Maybe Build.Info
  , _envFoundation :: Foundation
  , _envSettings   :: Settings
  }

makeLenses ''Env
