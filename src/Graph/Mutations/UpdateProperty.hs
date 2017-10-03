{-# LANGUAGE DeriveGeneric #-}
module Graph.Mutations.UpdateProperty
  ( UpdatePropertyInput
  , updateProperty
  ) where

import Graph.Import

import           Core
import qualified Data.Property as P
import qualified Graph.Types   as G
import qualified Graph.Query   as G
import qualified View          as V

data UpdatePropertyInput = UpdatePropertyInput { uid :: Text, description :: Text }
  deriving (Show, Generic)

instance FromValue UpdatePropertyInput
instance HasAnnotatedInputType UpdatePropertyInput
instance Defaultable UpdatePropertyInput where
  defaultFor _ = error "No default for UpdatePropertyInput"

updateProperty :: UpdatePropertyInput -> G G.Viewer
updateProperty UpdatePropertyInput{..} = do
  (Entity _id user) <- requireToken
  let ref = userBranch user
  old <- P.fetch (CommitRef ref) $ PropertyId uid
  let updated = old { propertyDescription = description }
      commit  = CommitMeta user $ "Update " <> propertyName updated
  (version, p) <- P.put ref commit updated
  G.presentView $ V.build [] [p] [] [] version
