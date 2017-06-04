{-# LANGUAGE DeriveGeneric #-}
module Graph.Mutations.AssertTrait
  ( AssertTraitInput
  , assertTrait
  ) where

import Graph.Import

import           Core        (SpaceId(..), PropertyId(..))
import qualified Data        as D
import qualified Graph.Types as G
import qualified Graph.Query as G

data AssertTraitInput = AssertTraitInput
  { spaceId     :: Text
  , propertyId  :: Text
  , value       :: Bool
  } deriving (Show, Generic)

instance FromValue AssertTraitInput
instance HasAnnotatedInputType AssertTraitInput
instance Defaultable AssertTraitInput where
  defaultFor _ = error "No default for AssertTraitInput"

assertTrait :: AssertTraitInput -> G G.Viewer
assertTrait AssertTraitInput{..} = do
  (Entity _ user) <- requireToken
  updates <- t user (SpaceId spaceId) (PropertyId propertyId) value ""
  G.viewerR updates

  where
    t = error "Implement assertTrait"

