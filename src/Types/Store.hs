module Types.Store where

import Import.NoFoundation

import Git.Libgit2  (LgRepo)
import Types
import Types.Loader

data Store = Store
  { storeRepo       :: LgRepo
  , storeRepoPath   :: FilePath
  , storeBaseBranch :: BranchName
  , storeLoader     :: MVar Loader
  , storeAutoSync   :: Bool
  }
