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

import           Core
import           Conduit         (sourceToList)
import qualified Data.Parse      as Parse
import           Data.Git        (commitSha)
import qualified Data.Map.Strict as M
import           Types.Loader    (Loader(Loader), Field(..))
import qualified Types.Loader    as Loader

instance Show Loader where
  show Loader{..} = "<Loader(" ++ show (commitSha commit) ++ ")>"

version :: Loader -> Version
version = Version . commitSha . Loader.commit

cache :: (MonadBase IO m, MonadBaseControl IO m, Ord x)
      => Field x a -> x -> m (Either e a) -> m (Either e a)
cache Field{..} key action = modifyMVar ref $ \index ->
  case M.lookup key index of
    Just record -> return (index, Right record)
    Nothing -> action >>= \case
      Left     err -> return (index, Left err)
      Right record -> return (M.insert (indexer record) record index, Right record)

ethrow :: MonadStore m => m (Either Error a) -> m a
ethrow action = action >>= either throwM return

fetch :: MonadStore m
      => (Loader -> Field (Id a) a)
      -> (Commit LgRepo -> Id a -> m (Either Error a))
      -> Loader
      -> Id a 
      -> m a
fetch field parser loader _id = ethrow 
  $ cache  (field loader)         _id 
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
