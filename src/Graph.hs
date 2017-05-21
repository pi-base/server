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
import Database.Persist.Types (Entity(..))
import Yesod.Auth (requireAuthPair, requireAuth)

import qualified Graph.Query as Q
import qualified Graph.Types as G

import Core hiding (Handler)
import Data
import Viewer (Viewer(..))

type G a = Handler Import.Handler a

space :: Monad m => Space -> Handler m G.Space
space Space{..} =
  let (SpaceId _id) = spaceId
  in pure $ pure "Space"
        :<> pure _id
        :<> pure spaceSlug
        :<> pure spaceName
        :<> pure spaceDescription
        :<> pure spaceTopology

spacesR :: Monad m => Viewer -> Handler m (List G.Space)
spacesR Viewer{..} = pure $ map space viewerSpaces

property :: Monad m => Property -> Handler m G.Property
property Property{..} =
  let (PropertyId _id) = propertyId
  in pure $ pure "Property"
        :<> pure _id
        :<> pure propertySlug
        :<> pure propertyName
        :<> pure propertyDescription

propertiesR :: Monad m => Viewer -> Handler m (List G.Property)
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
        Nothing -> failure "Update failed"
        Just us -> unionValue @G.Space (space us)

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

queryRoot :: Store -> Viewer -> G G.QueryRoot
queryRoot store viewer = pure
  $   spacesR viewer
  :<> propertiesR viewer
  :<> userR
  :<> Graph.updateSpace store
  :<> Graph.updateProperty store

query :: Store -> Viewer -> Q.GQuery -> Import.Handler Aeson.Value
query store viewer q = do
  response <- Q.query (queryRoot store viewer) q
  return . Aeson.toJSON $ toValue response
