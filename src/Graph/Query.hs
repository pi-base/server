{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Graph.Query where

import Graph.Import

import qualified Data.Aeson
import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import           Database.Persist.Types (Entity(..))
import           Yesod.Auth (requireAuth)

import           Core hiding (Handler)
import           Data
import           Formula (Formula)
import qualified Graph.Types as G
import           Model (User(..))
import qualified Util
import           Viewer (Viewer(..))

spaceR :: MonadStore m
       => M.Map SpaceId [Trait Space Property]
       -> Space
       -> Handler m G.Space
spaceR traitMap s@Space{..} = pure $ pure "Space"
  :<> pure (unSpaceId spaceId)
  :<> pure spaceSlug
  :<> pure spaceName
  :<> getSpaceDescription s
  :<> pure spaceTopology
  :<> traitsR traits
  where
    traits :: [Trait Space Property]
    traits = maybe [] id $ M.lookup spaceId traitMap


traitsR :: MonadStore m
        => [Trait Space Property]
        -> Handler m (List G.Trait)
traitsR = pure . map render
  where
    render Trait{..} = pure $ pure "Trait"
      :<> propertyR traitProperty
      :<> pure traitValue

theoremsR ::  MonadStore m => Viewer -> Handler m (List G.Theorem)
theoremsR Viewer{..} = pure $ map theoremR viewerTheorems

theoremR :: MonadStore m => Theorem Property -> Handler m G.Theorem
theoremR t@Theorem{..} = pure $ pure "Theorem"
  :<> pure (unTheoremId theoremId)
  :<> pure (encodeFormula theoremIf)
  :<> pure (encodeFormula theoremThen)
  :<> getTheoremDescription t

spacesR :: MonadStore m => Viewer -> Handler m (List G.Space)
spacesR Viewer{..} = pure $ map (spaceR traitMap) viewerSpaces
  where
    traitMap :: M.Map SpaceId [Trait Space Property]
    traitMap = Util.groupBy (\Trait{..} -> spaceId traitSpace) viewerTraits

propertyR :: MonadStore m => Property -> Handler m G.Property
propertyR p@Property{..} = pure $ pure "Property"
  :<> pure (unPropertyId propertyId)
  :<> pure propertySlug
  :<> pure propertyName
  :<> getPropertyDescription p

propertiesR :: MonadStore m => Viewer -> Handler m (List G.Property)
propertiesR Viewer{..} = pure $ map propertyR viewerProperties

user :: G G.User
user = do
  (Entity _id User{..}) <- requireAuth
  return $ pure "User" :<> pure userName

viewer :: MonadStore m => Maybe Version -> Handler m G.Viewer
viewer mver = do
  store <- getStore
  eviewer <- case mver of
    (Just ver) -> parseViewer store $ Sha ver
    _ -> storeMaster
  case eviewer of
    Left errs -> error $ show errs -- TODO: ViewerOrError
    Right v -> pure $ pure "Viewer"
      :<> spacesR v
      :<> propertiesR v
      :<> theoremsR v

encodeFormula :: Formula Property -> Text
encodeFormula = TL.toStrict . decodeUtf8 . Data.Aeson.encode . map (unPropertyId . propertyId)
