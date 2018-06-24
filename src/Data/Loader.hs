{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Loader
  ( Loader
  , Loader.mkLoader
  , property
  , space
  , theorem
  , trait
  , Loader.commit
  , version
  , spaceIds
  , Data.Loader.spaces
  , Data.Loader.properties
  , Data.Loader.theorems
  , spaceTraits
  , implications
  ) where

import Protolude hiding (modifyMVar)

import           Core
import           Conduit         (sourceToList)
import qualified Data.Parse      as Parse
import           Data.Git        (commitSha)
import qualified Data.Map.Strict as M
import           Types.Loader    (Loader, Field(..))
import qualified Types.Loader    as Loader
import           UnliftIO        (MonadUnliftIO)
import           UnliftIO.MVar   (modifyMVar)

version :: Loader -> Version
version = Version . commitSha . Loader.commit

cache :: (MonadUnliftIO m, Ord x)
      => Field x a -> x -> m a -> m a
cache field key action = modifyMVar (ref field) $ \index ->
  case M.lookup key index of
    Just record -> return (index, record)
    Nothing -> do
      record <- action
      let ix = indexer field record
          updated = M.insert ix record index
      return (updated, record)

fetch :: (MonadStore m, Ord k)
      => (Loader -> Field k a)
      -> (Commit LgRepo -> k -> m a)
      -> Loader
      -> k
      -> m a
fetch field parser loader _id =
    cache  (field loader)         _id 
  $ parser (Loader.commit loader) _id

space :: MonadStore m => Loader -> SpaceId -> m Space
space = fetch Loader.spaces Parse.space

property :: MonadStore m => Loader -> PropertyId -> m Property
property = fetch Loader.properties Parse.property

theorem :: MonadStore m => Loader -> TheoremId -> m (Theorem PropertyId)
theorem = fetch Loader.theorems Parse.theorem

trait :: MonadStore m
      => Loader
      -> SpaceId
      -> PropertyId
      -> m (Trait SpaceId PropertyId)
trait loader sid pid = fetch Loader.traits parser loader (sid, pid)
  where
    parser :: MonadStore m
           => Commit LgRepo 
           -> (SpaceId, PropertyId) 
           -> m (Trait SpaceId PropertyId)
    parser c (s, p) = Parse.trait c s p

spaceIds :: MonadStore m => Loader -> m [SpaceId]
spaceIds = sourceToList . Parse.spaceIds . Loader.commit

propertyIds :: MonadStore m => Loader -> m [PropertyId]
propertyIds = sourceToList . Parse.propertyIds . Loader.commit

theoremIds :: MonadStore m => Loader -> m [TheoremId]
theoremIds = sourceToList . Parse.theoremIds . Loader.commit

spaceTraitIds :: MonadStore m => Loader -> SpaceId -> m [PropertyId]
spaceTraitIds loader _id = sourceToList $ Parse.spaceTraitIds _id $ Loader.commit loader

implications :: MonadStore m => Loader -> m [(TheoremId, Implication PropertyId)]
implications loader = do
  ts <- theorems loader
  return $ map (\t -> (theoremId t, theoremImplication t)) ts

spaceTraits :: MonadStore m => Loader -> SpaceId -> m (Map PropertyId TVal)
spaceTraits loader sid = do
  traits <- loadAll (\l -> spaceTraitIds l sid) (\l -> trait l sid) loader
  return $ M.fromList $ map (\t -> (_traitProperty t, _traitValue t)) traits

loadAll :: Monad m
        => (Loader -> m [a])
        -> (Loader -> a -> m b)
        -> Loader
        -> m [b]
loadAll getIds getOne loader = do
  ids <- getIds loader
  foldM f [] ids
  where
    f acc _id = do
      record <- getOne loader _id
      return $! record : acc

spaces :: MonadStore m => Loader -> m [Space]
spaces = loadAll spaceIds space

properties :: MonadStore m => Loader -> m [Property]
properties = loadAll propertyIds property

theorems :: MonadStore m => Loader -> m [Theorem PropertyId]
theorems = loadAll theoremIds theorem