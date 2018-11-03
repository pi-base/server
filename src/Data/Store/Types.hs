{-# LANGUAGE TemplateHaskell #-}
module Data.Store.Types where

import Protolude

import Control.Lens      (makeLenses)
import Data.Aeson        (ToJSON)
import Data.Loader.Types
import Git.Libgit2       (LgRepo)
import Types

type RemoteURL = Text

data Settings = Settings
  { _repoPath      :: FilePath
  , _defaultBranch :: BranchName
  , _autoPush      :: Bool
  , _upstream      :: RemoteURL
  } deriving (Generic, Eq, Show)

makeLenses ''Settings

instance ToJSON Settings

data Store = Store
  { _storeSettings  :: Settings
  , _storeRepo      :: LgRepo
  , _storeLoader    :: MVar Loader
  , _storeWriteLock :: MVar ()
  }

makeLenses ''Store
