module Graph.Loader
  ( Loader
  , mkLoader
  , allProperties
  , allSpaces
  , allTheorems
  , spaceTraits
  , version
  ) where

import Core hiding (Loader) -- FIXME
import Graph.Import

import           Data.Git      (commitVersion)
import qualified Data.Parse as Parse

data Loader = Loader
  { lCommit     :: Commit LgRepo
  , lSpaces     :: IORef [Space]
  , lProperties :: IORef [Property]
  , lTheorems   :: IORef [Theorem PropertyId]
  }

mkLoader :: MonadBase IO m => Commit LgRepo -> m Loader
mkLoader commit = Loader
  <$> pure commit
  <*> newIORef mempty
  <*> newIORef mempty
  <*> newIORef mempty

version :: Loader -> Version
version = commitVersion . lCommit

allProperties :: MonadStore m => Loader -> m [Property]
allProperties Loader{..} = storeLoad lProperties $ Parse.properties lCommit

allSpaces :: MonadStore m => Loader -> m [Space]
allSpaces Loader{..} = storeLoad lSpaces $ Parse.spaces lCommit

allTheorems :: MonadStore m => Loader -> m [Theorem PropertyId]
allTheorems Loader{..} = storeLoad lTheorems $ Parse.theorems lCommit

-- Idea:
-- * each loader tracks what has been loaded (All | Ids [...])
-- * hold a reference to a loader for the master branch
spaceTraits :: MonadStore m => Loader -> Space -> m [Trait Space Property]
spaceTraits l@Loader{..} space = do
  props <- allProperties l
  sourceToList $ Parse.traits lCommit [space] props .| Parse.discardLeftC

storeLoad :: (MonadIO m, MonadBase IO m)
          => IORef [v]
          -> ConduitM () (Either Error v) m ()
          -> m [v]
storeLoad ref source = do
  vs <- readIORef ref
  if null vs
    then do
      vs' <- sourceToList $ source .| Parse.discardLeftC
      modifyIORef' ref $ const vs'
      return vs'
    else return vs

