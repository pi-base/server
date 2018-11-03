module Data.Loader.Types where

import Import

import Data.Map.Strict (Map)
import Git             (Commit)
import Git.Libgit2     (LgRepo)
import Types

data Field x a = Field
  { indexer :: (a -> x)
  , ref     :: MVar (Map x a)
  }

data Loader = Loader
  { commit     :: Commit LgRepo
  , spaces     :: Field SpaceId Space
  , properties :: Field PropertyId Property
  , theorems   :: Field TheoremId (Theorem PropertyId)
  , traits     :: Field (SpaceId, PropertyId) (Trait SpaceId PropertyId)
  }

mkField :: (MonadIO m, Ord x) => (a -> x) -> m (Field x a)
mkField indexer = Field
  <$> pure indexer
  <*> newMVar mempty

mkLoader :: MonadIO m => Commit LgRepo -> m Loader
mkLoader commit = Loader
  <$> pure commit
  <*> mkField spaceId
  <*> mkField propertyId
  <*> mkField theoremId
  <*> mkField (\t -> (_traitSpace t, _traitProperty t))
