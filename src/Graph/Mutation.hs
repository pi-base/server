{-# LANGUAGE TypeApplications #-}
module Graph.Mutation
  ( Graph.Mutation.updateSpace
  , Graph.Mutation.updateProperty
  ) where

import Graph.Import

import Import hiding (Handler)
import qualified Import (Handler)

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
        Just us -> unionValue @G.Space (Q.spaceR mempty us)
        Nothing -> failure "Update failed"

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
        Just up -> unionValue @G.Property (Q.propertyR up)

failure msg = unionValue @G.Error (pure $ pure "Error" :<> pure msg)
