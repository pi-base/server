{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
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

spaceR :: MonadStore m
       => M.Map SpaceId [Trait Space Property]
       -> Space
       -> Handler m G.Space
spaceR traitMap s@Space{..} =
  let
    (SpaceId _id) = spaceId

    traits :: [Trait Space Property]
    traits = maybe [] id $ M.lookup spaceId traitMap

  in pure $ pure "Space"
        :<> pure _id
        :<> pure spaceSlug
        :<> pure spaceName
        :<> getSpaceDescription s
        :<> pure spaceTopology
        :<> traitsR traits

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
theoremR t@Theorem{..} =
  let (TheoremId _id) = theoremId
  in pure $ pure "Theorem"
      :<> pure _id
      :<> pure "FIXME: if"
      :<> pure "FIXME: then"
      :<> getTheoremDescription t

spacesR :: MonadStore m => Viewer -> Handler m (List G.Space)
spacesR Viewer{..} = pure $ map (spaceR traitMap) viewerSpaces
  where
    traitMap :: M.Map SpaceId [Trait Space Property]
    traitMap = Util.groupBy (\Trait{..} -> spaceId traitSpace) viewerTraits

propertyR :: MonadStore m => Property -> Handler m G.Property
propertyR p@Property{..} =
  let (PropertyId _id) = propertyId
  in pure $ pure "Property"
        :<> pure _id
        :<> pure propertySlug
        :<> pure propertyName
        :<> getPropertyDescription p

propertiesR :: MonadStore m => Viewer -> Handler m (List G.Property)
propertiesR Viewer{..} = pure $ map propertyR viewerProperties

failure msg = unionValue @G.Error (pure $ pure "Error" :<> pure msg)

updateSpace :: Text -> Text -> G G.SpaceOrError
updateSpace _id description = do
  store <- getStore
  (_userId, user) <- requireAuthPair
  ms <- findSpace store _id
  case ms of
    Nothing -> failure "Could not find space"
    Just s -> do
      updated <- Data.updateSpace store user s description
      case updated of
        -- TODO: don't allow querying for traits here
        Just us -> unionValue @G.Space (spaceR mempty us)
        Nothing -> failure "Update failed"

userR :: G G.User
userR = do
  (Entity _id User{..}) <- requireAuth
  return $ pure "User" :<> pure userName

updateProperty :: Text -> Text -> G G.PropertyOrError
updateProperty _id description = do
  store <- getStore
  (_userId, user) <- requireAuthPair
  ms <- findProperty store _id
  case ms of
    Nothing -> failure "Could not find property"
    Just p -> do
      updated <- Data.updateProperty store user p description
      case updated of
        Nothing -> failure "Update failed"
        Just up -> unionValue @G.Property (propertyR up)

viewerR :: MonadStore m => Maybe Version -> Handler m G.Viewer
viewerR mver = do
  store <- getStore
  eviewer <- case mver of
    (Just ver) -> parseViewer store $ Sha ver
    _ -> storeMaster store
  case eviewer of
    Left errs -> error $ show errs -- TODO: ViewerOrError
    Right viewer -> pure
      $ pure "Viewer"
      :<> spacesR viewer
      :<> propertiesR viewer
      :<> theoremsR viewer

queryRoot :: G G.QueryRoot
queryRoot = pure
  $ pure "Query"
  :<> viewerR
  :<> userR
  :<> Graph.updateSpace
  :<> Graph.updateProperty

query :: Q.GQuery -> Import.Handler Aeson.Value
query q = do
  response <- Q.query queryRoot q
  return . Aeson.toJSON $ toValue response
