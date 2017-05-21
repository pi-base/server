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

space :: Monad m => M.Map SpaceId [Trait Space Property] -> Space -> Handler m G.Space
space traitMap Space{..} =
  let
    (SpaceId _id) = spaceId

    traits :: [Trait Space Property]
    traits = maybe [] id $ M.lookup spaceId traitMap

  in pure $ pure "Space"
        :<> pure _id
        :<> pure spaceSlug
        :<> pure spaceName
        :<> pure spaceDescription
        :<> pure spaceTopology
        :<> traitsR traits

traitsR :: Monad m => [Trait Space Property] -> Handler m (List G.Trait)
traitsR traits = pure $ map trait traits
  where
    trait Trait{..} = pure
      $ pure "Trait"
      :<> property traitProperty
      :<> pure traitValue

spacesR :: MonadStore m => Viewer -> Handler m (List G.Space)
spacesR Viewer{..} = pure $ map (space traitMap) viewerSpaces
  where
    traitMap :: M.Map SpaceId [Trait Space Property]
    traitMap = Util.groupBy (\Trait{..} -> spaceId traitSpace) viewerTraits

property :: Monad m => Property -> Handler m G.Property
property Property{..} =
  let (PropertyId _id) = propertyId
  in pure $ pure "Property"
        :<> pure _id
        :<> pure propertySlug
        :<> pure propertyName
        :<> pure propertyDescription

propertiesR :: MonadStore m => Viewer -> Handler m (List G.Property)
propertiesR Viewer{..} = pure $ map property viewerProperties

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
        Just us -> unionValue @G.Space (space mempty us)
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
        Just up -> unionValue @G.Property (property up)

viewerR :: MonadStore m => Store -> Maybe Version -> Handler m G.Viewer
viewerR store mver = viewerAtVersion mver >>= \case
  Left errs -> error $ show errs -- TODO: ViewerOrError
  Right viewer -> pure
    $ pure "Viewer"
    :<> spacesR viewer
    :<> propertiesR viewer
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
