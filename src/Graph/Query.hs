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

import qualified Data.Property as Property
import qualified Data.Space    as Space
import qualified Data.Theorem  as Theorem

spaceR :: MonadStore m
       => Maybe Committish
       -> Space
       -> M.Map PropertyId (Trait SpaceId PropertyId)
       -> M.Map PropertyId Property
       -> Handler m G.Space
spaceR mc s@Space{..} traitMap propMap = pure $ pure "Space"
  :<> pure (unSpaceId spaceId)
  :<> pure spaceSlug
  :<> pure spaceName
  :<> Space.describe mc s
  :<> pure spaceTopology
  :<> traitsR mc (M.elems traitMap) propMap


traitsR :: MonadStore m
        => Maybe Committish
        -> [Trait s PropertyId]
        -> M.Map PropertyId Property
        -> Handler m (List G.Trait)
traitsR mc traits props = pure $ map render traits
  where
    render Trait{..} = pure $ pure "Trait"
      :<> (findKey props _traitProperty >>= propertyR mc)
      :<> pure _traitValue

theoremsR ::  MonadStore m
          => Maybe Committish -> [Theorem p] -> (p -> m Property) -> Handler m (List G.Theorem)
theoremsR mc ts f = pure $ map (theoremR mc f) ts

theoremR :: MonadStore m => Maybe Committish -> (p -> m Property) -> Theorem p -> Handler m G.Theorem
theoremR mc f t@Theorem{..} = pure $ pure "Theorem"
  :<> pure (unTheoremId theoremId)
  :<> (encodeFormula f $ theoremIf t)
  :<> (encodeFormula f $ theoremThen t)
  :<> pure theoremDescription

spacesR :: MonadStore m
        => Maybe Committish
        -> [Space]
        -> M.Map SpaceId (M.Map PropertyId (Trait SpaceId PropertyId))
        -> M.Map PropertyId Property
        -> Handler m (List G.Space)
spacesR mc ss traitMap propMap = pure $ map space ss
  where space s = spaceR mc s (M.findWithDefault mempty (spaceId s) traitMap) propMap

propertyR :: MonadStore m => Maybe Committish -> Property -> Handler m G.Property
propertyR mc p@Property{..} = pure $ pure "Property"
  :<> pure (unPropertyId propertyId)
  :<> pure propertySlug
  :<> pure propertyName
  :<> pure propertyDescription

propertiesR :: MonadStore m => Maybe Committish -> [Property] -> Handler m (List G.Property)
propertiesR mc ps = pure $ map (propertyR mc) ps

user :: G G.User
user = do
  (Entity _id User{..}) <- requireToken
  return $ pure "User" :<> pure userName

viewer :: (MonadStore m, MonadHandler m) => Maybe Text -> Handler m G.Viewer
viewer mver = do
  eviewer <- case mver of
    (Just ver) -> parseViewer $ CommitSha ver
    _ -> storeMaster
  case eviewer of
    Left errs -> halt errs
    Right v   -> viewR v

viewR :: MonadStore m => View -> Handler m G.Viewer
viewR View{..} = pure $ pure "Viewer"
  :<> pure (maybe "" unVersion _viewVersion)
  :<> spacesR     sha (M.elems _viewSpaces    ) _viewTraits _viewProperties
  :<> propertiesR sha (M.elems _viewProperties)
  :<> theoremsR   sha (M.elems _viewTheorems  ) (findKey _viewProperties)
  where
    sha = CommitSha . unVersion <$> _viewVersion

encodeFormula :: MonadThrow m => (p -> m Property) -> Formula p -> m Text
encodeFormula f formula =
  mapM f formula >>=
    return . TL.toStrict . decodeUtf8 . Data.Aeson.encode . map (unPropertyId . propertyId)

findKey :: (MonadThrow m, Ord k, Show k) => M.Map k v -> k -> m v
findKey props p = case M.lookup p props of
  Nothing   -> throwM . Core.NotFound $ tshow p
  Just prop -> return prop
