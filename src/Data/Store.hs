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

-- storeCached :: MonadStore m
--             => m (Either a View)
--             -> m (Either a View)
-- storeCached f = do
--   Store{..} <- getStore
--   modifyMVar storeCache $ \mev -> case mev of
--     Just viewer -> return $ (Just viewer, Right viewer)
--     _ -> f >>= \case
--       Left err     -> return $ (Nothing, Left err)
--       Right viewer -> return $ (Just viewer, Right viewer)
