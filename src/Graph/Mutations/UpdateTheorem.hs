{-# LANGUAGE DeriveGeneric #-}
module Graph.Mutations.UpdateTheorem
  ( UpdateTheoremInput
  , updateTheorem
  ) where

import Graph.Import

import           Core
import qualified Data.Theorem as T
import qualified Graph.Types  as G
import qualified Graph.Query  as G
import qualified View         as V

data UpdateTheoremInput = UpdateTheoremInput { uid :: Text, description :: Text }
  deriving (Show, Generic)

instance FromValue UpdateTheoremInput
instance HasAnnotatedInputType UpdateTheoremInput
instance Defaultable UpdateTheoremInput where
  defaultFor _ = error "No default for UpdateTheoremInput"

updateTheorem :: UpdateTheoremInput -> G G.Viewer
updateTheorem UpdateTheoremInput{..} = do
  (Entity _id user) <- requireToken
  let ref = userBranch user
  old <- T.fetch (CommitRef ref) $ TheoremId uid
  let updated = old { theoremDescription = description }
      commit  = CommitMeta user $ "Update " <> theoremName updated
  (version, t) <- T.put ref commit updated
  G.viewR $ V.build [] [] [] [t] version
