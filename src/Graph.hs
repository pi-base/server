{-# LANGUAGE TypeApplications #-}
module Graph
  ( query
  ) where

import Import hiding (Handler)
import qualified Import (Handler)

import GraphQL.API (List)
import GraphQL.Resolver
import GraphQL.Value.ToValue (toValue)

import qualified Data.Aeson as Aeson
import qualified Data.Map as M
import Database.Persist.Types (Entity(..))
import Yesod.Auth (requireAuthPair, requireAuth)

import qualified Graph.Query as Q
import qualified Graph.Types as G

import Core hiding (Handler)
import Data
import qualified Util
import Viewer (Viewer(..))

type G a = Handler Import.Handler a

spaceR :: MonadStore m => Store -> M.Map SpaceId [Trait Space Property] -> Space -> Handler m G.Space
spaceR store traitMap s@Space{..} =
  let
    (SpaceId _id) = spaceId

    traits :: [Trait Space Property]
    traits = maybe [] id $ M.lookup spaceId traitMap

  in pure $ pure "Space"
        :<> pure _id
        :<> pure spaceSlug
        :<> pure spaceName
        :<> getSpaceDescription store s
        :<> pure spaceTopology
        :<> traitsR store traits

traitsR :: MonadStore m => Store -> [Trait Space Property] -> Handler m (List G.Trait)
traitsR store = pure . map render
  where
    render Trait{..} = pure $ pure "Trait"
      :<> propertyR store traitProperty
      :<> pure traitValue

theoremsR ::  MonadStore m => Store -> Viewer -> Handler m (List G.Theorem)
theoremsR store Viewer{..} = pure $ map (theoremR store) viewerTheorems

theoremR :: MonadStore m => Store -> Theorem Property -> Handler m G.Theorem
theoremR store t@Theorem{..} =
  let (TheoremId _id) = theoremId
  in pure $ pure "Theorem"
      :<> pure _id
      :<> pure "FIXME: if"
      :<> pure "FIXME: then"
      :<> getTheoremDescription store t

spacesR :: MonadStore m => Store -> Viewer -> Handler m (List G.Space)
spacesR store Viewer{..} = pure $ map (spaceR store traitMap) viewerSpaces
  where
    traitMap :: M.Map SpaceId [Trait Space Property]
    traitMap = Util.groupBy (\Trait{..} -> spaceId traitSpace) viewerTraits

propertyR :: MonadStore m => Store -> Property -> Handler m G.Property
propertyR store p@Property{..} =
  let (PropertyId _id) = propertyId
  in pure $ pure "Property"
        :<> pure _id
        :<> pure propertySlug
        :<> pure propertyName
        :<> getPropertyDescription store p

propertiesR :: MonadStore m => Store -> Viewer -> Handler m (List G.Property)
propertiesR store Viewer{..} = pure $ map (propertyR store) viewerProperties

failure msg = unionValue @G.Error (pure $ pure "Error" :<> pure msg)

updateSpace :: Store -> Text -> Text -> G G.SpaceOrError
updateSpace store _id description = do
  (_userId, user) <- requireAuthPair
  ms <- findSpace store _id
  case ms of
    Nothing -> failure "Could not find space"
    Just s -> do
      updated <- Data.updateSpace store user s description
      case updated of
        -- TODO: don't allow querying for traits here
        Just us -> unionValue @G.Space (spaceR store mempty us)
        Nothing -> failure "Update failed"

userR :: G G.User
userR = do
  (Entity _id User{..}) <- requireAuth
  return $ pure "User" :<> pure userName

updateProperty :: Store -> Text -> Text -> G G.PropertyOrError
updateProperty store _id description = do
  (_userId, user) <- requireAuthPair
  ms <- findProperty store _id
  case ms of
    Nothing -> failure "Could not find property"
    Just p -> do
      updated <- Data.updateProperty store user p description
      case updated of
        Nothing -> failure "Update failed"
        Just up -> unionValue @G.Property (propertyR store up)

viewerR :: MonadStore m => Store -> Maybe Version -> Handler m G.Viewer
viewerR store mver = viewerAtVersion mver >>= \case
  Left errs -> error $ show errs -- TODO: ViewerOrError
  Right viewer -> pure
    $ pure "Viewer"
    :<> spacesR store viewer
    :<> propertiesR store viewer
    :<> theoremsR store viewer
  where
    viewerAtVersion (Just ver) = parseViewer store $ Sha ver
    viewerAtVersion _ = storeMaster store

queryRoot :: Store -> G G.QueryRoot
queryRoot store = pure
  $ pure "Query"
  :<> viewerR store
  :<> userR
  :<> Graph.updateSpace store
  :<> Graph.updateProperty store

query :: Store -> Q.GQuery -> Import.Handler Aeson.Value
query store q = do
  response <- Q.query (queryRoot store) q
  return . Aeson.toJSON $ toValue response
