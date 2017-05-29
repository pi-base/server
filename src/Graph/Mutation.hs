{-# LANGUAGE TypeApplications #-}
module Graph.Mutation
  ( Graph.Mutation.updateProperty
  , module X
  ) where

import Graph.Import
import Graph.Mutations.UpdateSpace as X

import Yesod.Auth (requireAuthPair)

import qualified Graph.Query as Q
import qualified Graph.Types as G

import Data

data PropertyInput = PropertyInput { uid :: Text, description :: Text }

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

