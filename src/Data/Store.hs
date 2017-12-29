module Data.Store
  ( Store
  , mkStore
  , storeBaseRef
  , storeLoader
  , storeRepo
  ) where

import Import.NoFoundation

import Data.Loader
import Git         (Commit)
import Git.Libgit2 (LgRepo)
import Types
import Types.Store

mkStore :: MonadBase IO m => LgRepo -> Ref -> Commit LgRepo -> m Store
mkStore storeRepo storeBaseRef commit = do
  loader      <- mkLoader commit
  storeLoader <- newMVar loader
  return Store{..}