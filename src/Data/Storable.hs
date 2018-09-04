module Data.Storable
  ( Storable(..)
  ) where

import Protolude hiding (Storable, handle)
import Core      hiding (Key)

import Data.Git (writePages)

import qualified Data.Branch as Branch
import           Data.Loader
import qualified Page

import qualified Page.Property
import qualified Page.Space
import qualified Page.Theorem
import qualified Page.Trait

class Storable a where
  type Key a

  load :: MonadStore m => Loader -> Key a -> m a
  loadAll :: MonadStore m => Loader -> m [a]
  page :: Page a

  all :: (MonadStore m, MonadLogger m) => Branch -> m [a]
  all = Branch.loader >=> loadAll

  fetch :: (MonadStore m, MonadLogger m) => Branch -> Key a -> m a
  fetch b k = do
    loader <- Branch.loader b
    load loader k

  find :: (MonadStore m, MonadLogger m) => Branch -> Key a -> m (Maybe a)
  find b k = handle fail $ do
    loaded <- fetch b k
    return $ Just loaded
    where
      fail :: Monad m => NotFoundError -> m (Maybe b)
      fail _ = return Nothing

  write :: (MonadStore m, MonadLogger m) => a -> TreeT LgRepo m ()
  write a = writePages [Page.write page a]

  put :: (MonadStore m, MonadLogger m)
      => Branch -> User -> Text -> a -> m (a, Sha)
  put branch user message obj =
    Branch.update branch user message $ \_ -> do
      write obj
      return obj

instance Storable Space where
  type Key Space = SpaceId
  load    = Data.Loader.space
  loadAll = Data.Loader.spaces
  page    = Page.Space.page

instance Storable Property where
  type Key Property = PropertyId
  load    = Data.Loader.property
  loadAll = Data.Loader.properties
  page    = Page.Property.page

instance Storable (Theorem PropertyId) where
  type Key (Theorem PropertyId) = TheoremId
  load    = Data.Loader.theorem
  loadAll = Data.Loader.theorems
  page    = Page.Theorem.page

instance Storable (Trait SpaceId PropertyId) where
  type Key (Trait SpaceId PropertyId) = (SpaceId, PropertyId)
  load loader (sid, pid) = Data.Loader.trait loader sid pid
  loadAll                = Data.Loader.traits
  page                   = Page.Trait.page