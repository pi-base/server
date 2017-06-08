{-# LANGUAGE DeriveGeneric #-}
module Graph.Mutations.CreateProperty
  ( CreatePropertyInput
  , createProperty
  ) where

import Graph.Import

import qualified Data        as D
import qualified Graph.Types as G
import qualified Graph.Query as G

data CreatePropertyInput = CreatePropertyInput { name :: Text, description :: Text }
  deriving (Show, Generic)

instance FromValue CreatePropertyInput
instance HasAnnotatedInputType CreatePropertyInput
instance Defaultable CreatePropertyInput where
  defaultFor _ = error "No default for CreatePropertyInput"

createProperty :: CreatePropertyInput -> G G.Property
createProperty CreatePropertyInput{..} = do
  (Entity _id user) <- requireToken
  property <- D.createProperty user name description
  G.propertyR property
