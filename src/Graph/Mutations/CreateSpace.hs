{-# LANGUAGE DeriveGeneric #-}
module Graph.Mutations.CreateSpace
  ( CreateSpaceInput
  , createSpace
  ) where

import Graph.Import

import qualified Data        as D
import qualified Graph.Types as G
import qualified Graph.Query as G

data CreateSpaceInput = CreateSpaceInput { name :: Text, description :: Text }
  deriving (Show, Generic)

instance FromValue CreateSpaceInput
instance HasAnnotatedInputType CreateSpaceInput
instance Defaultable CreateSpaceInput where
  defaultFor _ = error "No default for CreateSpaceInput"

createSpace :: CreateSpaceInput -> G G.Space
createSpace CreateSpaceInput{..} = do
  (Entity _ user) <- requireToken
  space <- D.createSpace user name description
  G.spaceR space mempty mempty
