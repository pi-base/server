{-# LANGUAGE DeriveGeneric #-}
module Graph.Mutations.AssertTrait
  ( AssertTraitInput
  , assertTrait
  ) where

import Graph.Import

import           Core        (SpaceId(..))
import qualified Data        as D
import qualified Graph.Types as G
import qualified Graph.Query as G

data AssertTraitInput = AssertTraitInput
  { spaceId     :: Text
  , propertyId  :: Text
  -- , value       :: Text
  -- , description :: Text
  } deriving (Show, Generic)

instance FromValue AssertTraitInput
instance HasAnnotatedInputType AssertTraitInput
instance Defaultable AssertTraitInput where
  defaultFor _ = error "No default for AssertTraitInput"

assertTrait :: AssertTraitInput -> G (List G.Trait)
assertTrait AssertTraitInput{..} = error "assertTrait"
