{-# LANGUAGE DeriveGeneric #-}
module Graph.Mutations.UpdateTheorem
  ( UpdateTheoremInput
  , updateTheorem
  ) where

import Graph.Import

import           Core        (TheoremId(..), fixme)
import qualified Data        as D
import qualified Graph.Types as G
import qualified Graph.Query as G

data UpdateTheoremInput = UpdateTheoremInput { uid :: Text, description :: Text }
  deriving (Show, Generic)

instance FromValue UpdateTheoremInput
instance HasAnnotatedInputType UpdateTheoremInput
instance Defaultable UpdateTheoremInput where
  defaultFor _ = error "No default for UpdateTheoremInput"

updateTheorem :: UpdateTheoremInput -> G G.Theorem
updateTheorem UpdateTheoremInput{..} = do
  (Entity _id user) <- requireToken
  ms <- D.findTheorem $ TheoremId uid
  case ms of
    Nothing -> halt "Could not find theorem"
    Just t ->
      D.updateTheorem user (fixme "updateTheorem") description >>= \case
        Just up -> G.theoremR (fixme "updateTheorem") up
        Nothing -> halt "Update failed"
