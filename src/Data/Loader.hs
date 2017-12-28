module Data.Loader
  ( Loader
  , mkLoader
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

import           Core
import           Conduit         (sourceToList)
import qualified Data.Parse      as Parse
import           Data.Git        (commitSha)
import qualified Data.Map.Strict as M
import           Types.Loader    as Loader

instance Show Loader where
  show Loader{..} = "<Loader(" ++ show (commitSha commit) ++ ")>"

version :: Loader -> Version
version = Version . commitSha . Loader.commit

cache :: (MonadBase IO m, MonadBaseControl IO m, Ord x)
      => Field x a -> x -> m (Either e a) -> m (Either e a)
cache Field{..} key action = modifyMVar ref $ \hash ->
  case M.lookup key hash of
    Just record -> return (hash, Right record)
    Nothing -> action >>= \case
      Left     err -> return (hash, Left err)
      Right record -> return (M.insert (indexer record) record hash, Right record)

ethrow :: MonadStore m => m (Either Error a) -> m a
ethrow action = action >>= either throwM return

space :: MonadStore m => Loader -> SpaceId -> m Space
space Loader{..} _id = ethrow $ cache spaces _id $ Parse.space commit _id

property :: MonadStore m => Loader -> PropertyId -> m Property
property Loader{..} _id = ethrow $ cache properties _id $ Parse.property commit _id

theorem :: MonadStore m => Loader -> TheoremId -> m (Theorem PropertyId)
theorem Loader{..} _id = ethrow $ cache theorems _id $ Parse.theorem commit _id

trait :: MonadStore m
      => Loader
      -> SpaceId
      -> PropertyId
      -> m (Trait SpaceId PropertyId)
trait = error "trait"

spaceIds :: MonadStore m => Loader -> m [SpaceId]
spaceIds Loader{..} = sourceToList $ Parse.spaceIds commit

propertyIds :: MonadStore m => Loader -> m [PropertyId]
propertyIds Loader{..} = sourceToList $ Parse.propertyIds commit

theoremIds :: MonadStore m => Loader -> m [TheoremId]
theoremIds Loader{..} = sourceToList $ Parse.theoremIds commit

implications :: MonadStore m => Loader -> m [(TheoremId, Implication PropertyId)]
implications = error "implications"

spaceTraits :: MonadStore m => Loader -> SpaceId -> m (Map PropertyId TVal)
spaceTraits Loader{..} _id = error "traits"

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
