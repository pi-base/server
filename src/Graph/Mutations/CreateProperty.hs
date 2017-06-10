{-# LANGUAGE DeriveGeneric #-}
module Graph.Mutations.CreateProperty
  ( CreatePropertyInput
  , createProperty
  ) where

import Graph.Import

import           Core
import qualified Data.Property as P
import qualified Graph.Types   as G
import qualified Graph.Query   as G
import qualified View          as V

data CreatePropertyInput = CreatePropertyInput { name :: Text, description :: Text }
  deriving (Show, Generic)

instance FromValue CreatePropertyInput
instance HasAnnotatedInputType CreatePropertyInput
instance Defaultable CreatePropertyInput where
  defaultFor _ = error "No default for CreatePropertyInput"

createProperty :: CreatePropertyInput -> G G.Viewer
createProperty CreatePropertyInput{..} = do
  (Entity _id user) <- requireToken
  let property = Property
        { propertyId          = P.pending
        , propertyName        = name
        , propertyDescription = description
        , propertySlug        = slugify name
        , propertyAliases     = Just []
        }
      commit = CommitMeta user $ "Add " <> name
  (version, p) <- P.put (userBranch user) commit property
  G.viewR $ V.build [] [p] [] [] version
