module Graph.Loader
  ( Loader
  , Graph.Loader.mkLoader
  , allProperties
  , loadProperty
  , allSpaces
  , allTheorems
  , loadTrait
  , spaceTraits
  , version
  ) where

import qualified Data.Map as M

import Core hiding (Loader) -- FIXME

import           Conduit
import           Data.Git     (commitVersion)
import           Data.Loader  as L
import qualified Data.Parse   as Parse
import           Types        hiding (Loader)
import           Types.Loader as L
import           Types.Store
import           Util         (insertNested)

mkLoader :: MonadStore m => Commit LgRepo -> m Loader
mkLoader commit = do
  loaderVar    <- storeLoader <$> getStore
  cachedLoader <- readMVar loaderVar
  if (commitVersion commit) == (commitVersion $ L.commit cachedLoader)
    then return cachedLoader
    else L.mkLoader commit

allProperties :: MonadStore m => Loader -> m [Property]
allProperties Loader{..} = storeLoad properties $ Parse.properties commit

loadProperty :: MonadStore m => Loader -> PropertyId -> m Property
loadProperty loader pid = do
  properties <- allProperties loader
  case find (\p -> propertyId p == pid) properties of
    Just found -> return found
    Nothing -> Core.throwM . Types.NotFound $ unId pid

loadTrait :: MonadStore m => Loader -> SpaceId -> PropertyId -> m (Trait SpaceId PropertyId)
loadTrait loader sid pid = do
  mtrait <- Parse.trait (commit loader) sid pid
  case mtrait of
    Left    err -> Core.throwM err
    Right trait -> return trait

allSpaces :: MonadStore m => Loader -> m [Space]
allSpaces Loader{..} = storeLoad spaces $ Parse.spaces commit

allTheorems :: MonadStore m => Loader -> m [Theorem PropertyId]
allTheorems Loader{..} = storeLoad theorems $ Parse.theorems commit

allTVals :: MonadStore m => Loader -> m (Map SpaceId (Map PropertyId TVal))
allTVals Loader{..} = cache traits $ do
  tuples <- sourceToList $ Parse.allTraits commit
  return $ foldr acc mempty tuples
  where
    acc :: Trait SpaceId PropertyId -> Map SpaceId (Map PropertyId TVal) -> Map SpaceId (Map PropertyId TVal)
    acc t = insertNested (_traitSpace t) (_traitProperty t) (_traitValue t)

spaceTraits :: MonadStore m => Loader -> Space -> m (Map PropertyId TVal)
spaceTraits loader space = do
  tvals <- allTVals loader
  return $ M.findWithDefault mempty (spaceId space) tvals

storeLoad :: (MonadIO m, MonadBase IO m, MonadBaseControl IO m, Eq v, Show v)
          => Field [v]
          -> ConduitM () (Either Error v) m ()
          -> m [v]
storeLoad ref source = cache ref $
  sourceToList $ source .| Parse.discardLeftC
