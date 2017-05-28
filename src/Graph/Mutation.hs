{-# LANGUAGE TypeApplications #-}
module Graph.Mutation
  ( Graph.Mutation.updateSpace
  , Graph.Mutation.updateProperty
  ) where

import Graph.Import

import Yesod.Auth (requireAuthPair)

import qualified Graph.Query as Q
import qualified Graph.Types as G

import Data

updateSpace :: Text -> Text -> G G.Space
updateSpace _id description = do
  store <- getStore
  (_userId, user) <- requireAuthPair
  ms <- findSpace store _id
  case ms of
    Nothing -> halt "Could not find space"
    Just s -> do
      updated <- Data.updateSpace store user s description
      case updated of
        -- TODO: don't allow querying for traits here
        Just us -> Q.spaceR mempty us
        Nothing -> halt "Update failed"

updateProperty :: Text -> Text -> G G.Property
updateProperty _id description = do
  store <- getStore
  (_userId, user) <- requireAuthPair
  ms <- findProperty store _id
  case ms of
    Nothing -> halt "Could not find property"
    Just p -> do
      updated <- Data.updateProperty store user p description
      case updated of
        Nothing -> halt "Update failed"
        Just up -> Q.propertyR up

