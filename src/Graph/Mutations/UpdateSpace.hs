{-# LANGUAGE DeriveGeneric #-}
module Graph.Mutations.UpdateSpace
  ( UpdateSpaceInput
  , updateSpace
  ) where

import Graph.Import

import           Core        (SpaceId(..))
import qualified Data        as D
import qualified Graph.Types as G
import qualified Graph.Query as G

data UpdateSpaceInput = UpdateSpaceInput { uid :: Text, description :: Text }
  deriving (Show, Generic)

instance FromValue UpdateSpaceInput
instance HasAnnotatedInputType UpdateSpaceInput
instance Defaultable UpdateSpaceInput where
  defaultFor _ = error "No default for UpdateSpaceInput"

updateSpace :: UpdateSpaceInput -> G G.Space
updateSpace UpdateSpaceInput{..} = do
  (Entity _id user) <- requireToken
  ms <- D.findSpace $ SpaceId uid
  case ms of
    Nothing -> halt "Could not find space"
    Just s ->
      D.updateSpace user s description >>= \case
        -- TODO: don't allow querying for traits here
        Just us -> G.spaceR mempty us
        Nothing -> halt "Update failed"
