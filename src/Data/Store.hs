module Data.Store
  ( Store
  , mkStore
  ) where

-- TODO: enforce that only one thread gets to write to a branch at a time?
data Store = Store
  { storeRepo    :: LgRepo
  , storeCache   :: MVar (Maybe View)
  , storeBaseRef :: Ref
  }
