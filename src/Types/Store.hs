module Types.Store where

import Import.NoFoundation

import Git.Libgit2  (LgRepo)
import Types
import Types.Loader

type RemoteURL = Text

data Store = Store
  { storeRepo       :: LgRepo
  , storeRepoPath   :: FilePath
  , storeBaseBranch :: BranchName
  , storeLoader     :: MVar Loader
  , storeAutoSync   :: Bool
  , storeUpstream   :: RemoteURL
  , storeDownstream :: RemoteURL
  }
