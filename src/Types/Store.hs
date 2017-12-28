module Types.Store where

import Import.NoFoundation

import Git.Libgit2  (LgRepo)
import Types
import Types.Loader

data Store = Store
  { storeRepo    :: LgRepo
  , storeBaseRef :: Ref
  , storeLoader  :: MVar Loader
  }
