{-# LANGUAGE DeriveGeneric #-}
module Graph.Mutations.AssertTrait
  ( AssertTraitInput
  , assertTrait
  ) where

import Graph.Import

import           Core
import qualified Data.Trait    as T
import qualified Graph.Types   as G
import qualified Graph.Query   as G

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

  let trait = Trait
        { _traitSpace       = SpaceId spaceId
        , _traitProperty    = PropertyId propertyId
        , _traitValue       = value
        , _traitDescription = "FIXME"
        }

      commit = CommitMeta user $ "Add " <> tshow trait

  view <- T.put (userBranch user) commit trait
  either halt G.viewR view
