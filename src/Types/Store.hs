module Types.Store where

import Import.NoFoundation

import Git          (MonadGit(..), Commit)
import Git.Libgit2  (LgRepo)
import Types        hiding (Loader)
import Types.Loader

data Store = Store
  { storeRepo    :: LgRepo
  , storeBaseRef :: Ref
  , storeLoader  :: MVar Loader
  }
