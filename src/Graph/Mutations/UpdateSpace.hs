{-# LANGUAGE DeriveGeneric #-}
module Graph.Mutations.UpdateSpace
  ( UpdateSpaceInput
  , updateSpace
  ) where

import Graph.Import

import           Core
import qualified Data.Space  as S
import qualified Graph.Types as G
import qualified Graph.Query as G
import qualified View        as V

data UpdateSpaceInput = UpdateSpaceInput { uid :: Text, description :: Text }
  deriving (Show, Generic)

instance FromValue UpdateSpaceInput
instance HasAnnotatedInputType UpdateSpaceInput
instance Defaultable UpdateSpaceInput where
  defaultFor _ = error "No default for UpdateSpaceInput"

updateSpace :: UpdateSpaceInput -> G G.Viewer
updateSpace UpdateSpaceInput{..} = do
  (Entity _id user) <- requireToken
  let ref = userBranch user
  old <- S.fetch (CommitRef ref) $ SpaceId uid
  let updated = old { spaceDescription = description }
      commit  = CommitMeta user $ "Update " <> spaceName updated
  (version, s) <- S.put ref commit updated
  G.viewR $ V.build [s] [] [] [] version
