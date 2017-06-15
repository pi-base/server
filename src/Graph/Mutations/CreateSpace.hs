{-# LANGUAGE DeriveGeneric #-}
module Graph.Mutations.CreateSpace
  ( CreateSpaceInput
  , createSpace
  ) where

import Graph.Import

import           Core
import qualified Data.Space  as S
import qualified Graph.Types as G
import qualified Graph.Query as G
import qualified View        as V

data CreateSpaceInput = CreateSpaceInput { name :: Text, description :: Text }
  deriving (Show, Generic)

instance FromValue CreateSpaceInput
instance HasAnnotatedInputType CreateSpaceInput
instance Defaultable CreateSpaceInput where
  defaultFor _ = error "No default for CreateSpaceInput"

createSpace :: CreateSpaceInput -> G G.Viewer
createSpace CreateSpaceInput{..} = do
  putStrLn "createSpace"
  (Entity _ user) <- requireToken
  let space = Space
        { spaceId          = S.pending
        , spaceName        = name
        , spaceDescription = description
        , spaceSlug        = slugify name
        , spaceTopology    = Nothing
        }
      commit = CommitMeta user $ "Add " <> name
  (version, s) <- S.put (userBranch user) commit space
  G.viewR $ V.build [s] [] [] [] version
