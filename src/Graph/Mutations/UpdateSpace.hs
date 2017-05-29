{-# LANGUAGE DeriveGeneric #-}
module Graph.Mutations.UpdateSpace
  ( SpaceInput
  , updateSpace
  ) where

import Graph.Import

import Yesod.Auth (requireAuthPair)

import qualified Data        as D
import qualified Graph.Types as G
import qualified Graph.Query as G

data SpaceInput = SpaceInput { uid :: Text, description :: Text }
  deriving (Show, Generic)

instance FromValue SpaceInput
instance HasAnnotatedInputType SpaceInput
instance Defaultable SpaceInput where
  defaultFor _ = error "No default for SpaceInput"

updateSpace :: SpaceInput -> G G.Space
updateSpace SpaceInput{..} = do
  store <- D.getStore
  (_userId, user) <- requireAuthPair
  ms <- D.findSpace store uid
  case ms of
    Nothing -> halt "Could not find space"
    Just s -> do
      updated <- D.updateSpace store user s description
      case updated of
        -- TODO: don't allow querying for traits here
        Just us -> G.spaceR mempty us
        Nothing -> halt "Update failed"
