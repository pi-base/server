{-# LANGUAGE DeriveGeneric #-}
module Graph.Mutations.UpdateProperty
  ( UpdatePropertyInput
  , updateProperty
  ) where

import Graph.Import

import           Core        (PropertyId(..))
import qualified Data        as D
import qualified Graph.Types as G
import qualified Graph.Query as G

data UpdatePropertyInput = UpdatePropertyInput { uid :: Text, description :: Text }
  deriving (Show, Generic)

instance FromValue UpdatePropertyInput
instance HasAnnotatedInputType UpdatePropertyInput
instance Defaultable UpdatePropertyInput where
  defaultFor _ = error "No default for UpdatePropertyInput"

updateProperty :: UpdatePropertyInput -> G G.Property
updateProperty UpdatePropertyInput{..} = do
  (_userId, user) <- requireAuthPair
  ms <- D.findProperty $ PropertyId uid
  case ms of
    Nothing -> halt "Could not find property"
    Just p ->
      D.updateProperty user p description >>= \case
        Just up -> G.propertyR up
        Nothing -> halt "Update failed"
