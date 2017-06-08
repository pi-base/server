{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Graph.Query where

import Graph.Import

import qualified Data.Aeson
import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import           Database.Persist.Types (Entity(..))

import           Core hiding (Handler)
import           Data
import           Formula (Formula)
import qualified Graph.Types as G
import           Handler.Helpers (requireToken)
import           Model (User(..))

spaceR :: MonadStore m
       => Space
       -> M.Map PropertyId (Trait SpaceId PropertyId)
       -> M.Map PropertyId Property
       -> Handler m G.Space
spaceR s@Space{..} traitMap propMap = pure $ pure "Space"
  :<> pure (unSpaceId spaceId)
  :<> pure spaceSlug
  :<> pure spaceName
  :<> getSpaceDescription s
  :<> pure spaceTopology
  :<> traitsR (M.elems traitMap) propMap


traitsR :: MonadStore m
        => [Trait s PropertyId]
        -> M.Map PropertyId Property
        -> Handler m (List G.Trait)
traitsR traits props = pure $ map render traits
  where
    render Trait{..} = pure $ pure "Trait"
      :<> (findKey props traitProperty >>= propertyR)
      :<> pure traitValue

theoremsR ::  MonadStore m
          => [Theorem p] -> (p -> m Property) -> Handler m (List G.Theorem)
theoremsR ts f = pure $ map (theoremR f) ts

theoremR :: MonadStore m => (p -> m Property) -> Theorem p -> Handler m G.Theorem
theoremR f t@Theorem{..} = pure $ pure "Theorem"
  :<> pure (unTheoremId theoremId)
  :<> (encodeFormula f $ theoremIf t)
  :<> (encodeFormula f $ theoremThen t)
  :<> getTheoremDescription t

spacesR :: MonadStore m
        => [Space]
        -> M.Map SpaceId (M.Map PropertyId (Trait SpaceId PropertyId))
        -> M.Map PropertyId Property
        -> Handler m (List G.Space)
spacesR ss traitMap propMap = pure $ map space ss
  where space s = spaceR s (M.findWithDefault mempty (spaceId s) traitMap) propMap

propertyR :: MonadStore m => Property -> Handler m G.Property
propertyR p@Property{..} = pure $ pure "Property"
  :<> pure (unPropertyId propertyId)
  :<> pure propertySlug
  :<> pure propertyName
  :<> getPropertyDescription p

propertiesR :: MonadStore m => [Property] -> Handler m (List G.Property)
propertiesR ps = pure $ map propertyR ps

user :: G G.User
user = do
  (Entity _id User{..}) <- requireToken
  return $ pure "User" :<> pure userName

viewer :: (MonadStore m, MonadHandler m) => Maybe Text -> Handler m G.Viewer
viewer mver = do
  eviewer <- case mver of
    (Just ver) -> parseViewer $ Sha ver
    _ -> storeMaster
  case eviewer of
    Left errs -> halt $ show errs
    Right v   -> viewR v

viewR :: MonadStore m => View -> Handler m G.Viewer
viewR View{..} = pure $ pure "Viewer"
  :<> pure (maybe "" unVersion _viewVersion)
  :<> spacesR     (M.elems _viewSpaces    ) _viewTraits _viewProperties
  :<> propertiesR (M.elems _viewProperties)
  :<> theoremsR   (M.elems _viewTheorems  ) (findKey _viewProperties)

encodeFormula :: MonadThrow m => (p -> m Property) -> Formula p -> m Text
encodeFormula f formula =
  mapM f formula >>=
    return . TL.toStrict . decodeUtf8 . Data.Aeson.encode . map (unPropertyId . propertyId)

findKey :: (MonadThrow m, Ord k, Show k) => M.Map k v -> k -> m v
findKey props p = case M.lookup p props of
  Nothing   -> throwM . Core.NotFound $ tshow p
  Just prop -> return prop
