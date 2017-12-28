module Types.Loader where

import Import.NoFoundation hiding (Field)

import Data.Map.Strict (Map)
import Git             (Commit)
import Git.Libgit2     (LgRepo)
import Types

data Field x a = Field
  { name    :: Text
  , indexer :: (a -> x)
  , ref     :: MVar (Map x a)
  }

data Loader = Loader
  { commit     :: Commit LgRepo
  , spaces     :: Field SpaceId Space
  , properties :: Field PropertyId Property
  , theorems   :: Field TheoremId (Theorem PropertyId)
  , traits     :: Field (SpaceId, PropertyId) (Trait SpaceId PropertyId)
  }

mkField :: (Ord x, MonadBase IO m) => Text -> (a -> x) -> m (Field x a)
mkField name indexer = Field
  <$> pure name
  <*> pure indexer
  <*> newMVar mempty

mkLoader :: MonadBase IO m => Commit LgRepo -> m Loader
mkLoader commit = Loader
  <$> pure commit
  <*> mkField "spaces" spaceId
  <*> mkField "properties" propertyId
  <*> mkField "theorems" theoremId
  <*> mkField "traits" (\t -> (_traitSpace t, _traitProperty t))
