{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , DuplicateRecordFields
  , OverloadedStrings
  , PatternSynonyms
  , TypeOperators
  , ViewPatterns
#-}
module Graph.Types where

import Prelude

import qualified Data.Aeson              as Aeson
import           GraphQL.API
import           GraphQL.Internal.Validation (QueryDocument, VariableValue)
import           GraphQL.Resolver        (Defaultable(..))
import           GraphQL.Value           (Name, pattern ValueEnum)
import           GraphQL.Value.ToValue   (ToValue(..))
import           GraphQL.Value.FromValue (FromValue(..))

import Core (BranchAccess, Generic, Text)

instance GraphQLEnum BranchAccess

instance ToValue BranchAccess where
  toValue = ValueEnum . enumToValue

data Operation = Named Name | Anonymous deriving Show

type Query = QueryDocument VariableValue

data QueryData = QueryData
  { operation :: Operation
  , query     :: Text
  , variables :: Maybe Aeson.Object
  } deriving Show

-- Graph Types

type Space = Object "Space" '[]
  '[ Field "__typename"      Text
   , Field "uid"             Text
   , Field "slug"            Text
   , Field "name"            Text
   , Field "aliases"         (List Text)
   , Field "description"     Text
   , Field "proofOfTopology" (Maybe Text)
   , Field "traits"          (List Trait)
   ]

type Property = Object "Property" '[]
  '[ Field "__typename"      Text
   , Field "uid"             Text
   , Field "slug"            Text
   , Field "name"            Text
   , Field "aliases"         (List Text)
   , Field "description"     Text
   ]

type Trait = Object "Trait" '[]
  '[ Field "__typename"  Text
   , Field "property"    Property
   , Field "value"       Bool
   , Field "description" Text
   ]

type Theorem = Object "Theorem" '[]
  '[ Field "__typename"  Text
   , Field "uid"         Text
   , Field "if"          Text
   , Field "then"        Text
   , Field "description" Text
   ]

type Branch = Object "Branch" '[]
  '[ Field "__typename" Text
   , Field "name"       Text
   , Field "access"     Text -- FIXME
   , Field "sha"        Text
   ]

type User = Object "User" '[]
  '[ Field "__typename" Text
   , Field "name"       Text
   , Field "branches"   (List Branch)
   ]

type Error = Object "Error" '[]
  '[ Field "__typename" Text
   , Field "message"    Text
   ]

type Viewer = Object "Viewer" '[]
  '[ Field "__typename" Text
   , Field "version"    Text
   , Field "spaces"     (List Space)
   , Field "properties" (List Property)
   , Field "theorems"   (List Theorem)
   ]

type ResetBranchResponse = Object "ResetBranchResponse" '[]
  '[ Field "__typename" Text
   , Field "branch"     Text
   , Field "sha"        Text
   ]

type TestResetResponse = Object "TestResetResponse" '[]
  '[ Field "__typename" Text
   , Field "version"    Text
   , Field "token"      (Maybe Text)
   ]

type PatchMutation = Object "PatchMutation" '[]
  '[ Field "__typename" Text
   , Argument "input" CreateSpaceInput    :> Field "createSpace"    Viewer
   , Argument "input" CreatePropertyInput :> Field "createProperty" Viewer
   , Argument "input" UpdateSpaceInput    :> Field "updateSpace"    Viewer
   , Argument "input" UpdatePropertyInput :> Field "updateProperty" Viewer
   , Argument "input" UpdateTheoremInput  :> Field "updateTheorem"  Viewer
   , Argument "input" AssertTraitInput    :> Field "assertTrait"    Viewer
   , Argument "input" AssertTheoremInput  :> Field "assertTheorem"  Viewer
   ]

type Root = Object "QueryRoot" '[]
  '[ Field "__typename" Text
   , Argument "version" (Maybe Text) :> Field "viewer" Viewer
   , Field "me" User
   -- Mutations
   , Argument "input" PatchInput       :> Field "patch"       PatchMutation
   , Argument "input" ResetBranchInput :> Field "resetBranch" ResetBranchResponse
   , Argument "input" TestResetInput   :> Field "testReset"   TestResetResponse
   ]

-- Inputs

data CreateSpaceInput = CreateSpaceInput
  { name        :: Text
  , description :: Text
  } deriving (Show, Generic)

instance FromValue CreateSpaceInput
instance HasAnnotatedInputType CreateSpaceInput
instance Defaultable CreateSpaceInput where
  defaultFor _ = error "No default for CreateSpaceInput"

data CreatePropertyInput = CreatePropertyInput
  { name        :: Text
  , description :: Text
  } deriving (Show, Generic)

instance FromValue CreatePropertyInput
instance HasAnnotatedInputType CreatePropertyInput
instance Defaultable CreatePropertyInput where
  defaultFor _ = error "No default for CreatePropertyInput"

data AssertTraitInput = AssertTraitInput
  { spaceId     :: Text
  , propertyId  :: Text
  , value       :: Bool
  } deriving (Show, Generic)

instance FromValue AssertTraitInput
instance HasAnnotatedInputType AssertTraitInput
instance Defaultable AssertTraitInput where
  defaultFor _ = error "No default for AssertTraitInput"

data AssertTheoremInput = AssertTheoremInput
  { antecedent  :: Text
  , consequent  :: Text
  , description :: Text
  } deriving (Show, Generic)

instance FromValue AssertTheoremInput
instance HasAnnotatedInputType AssertTheoremInput
instance Defaultable AssertTheoremInput where
  defaultFor _ = error "No default for AssertTheoremInput"

data TestResetInput = TestResetInput
  { token :: Maybe Text
  , ref :: Text
  } deriving (Show, Generic)

instance FromValue TestResetInput
instance HasAnnotatedInputType TestResetInput
instance Defaultable TestResetInput where
  defaultFor _ = error "No default for TestResetInput"

data ResetBranchInput = ResetBranchInput
  { branch :: Text
  , to     :: Text -- ref or sha
  } deriving (Show, Generic)

instance FromValue ResetBranchInput
instance HasAnnotatedInputType ResetBranchInput
instance Defaultable ResetBranchInput where
  defaultFor _ = error "No default for ResetBranchInput"

data UpdateSpaceInput = UpdateSpaceInput
  { uid :: Text
  , description :: Text
  } deriving (Show, Generic)

instance FromValue UpdateSpaceInput
instance HasAnnotatedInputType UpdateSpaceInput
instance Defaultable UpdateSpaceInput where
  defaultFor _ = error "No default for UpdateSpaceInput"

data UpdatePropertyInput = UpdatePropertyInput
  { uid :: Text
  , description :: Text
  } deriving (Show, Generic)

instance FromValue UpdatePropertyInput
instance HasAnnotatedInputType UpdatePropertyInput
instance Defaultable UpdatePropertyInput where
  defaultFor _ = error "No default for UpdatePropertyInput"

data UpdateTheoremInput = UpdateTheoremInput
  { uid :: Text
  , description :: Text
  } deriving (Show, Generic)

instance FromValue UpdateTheoremInput
instance HasAnnotatedInputType UpdateTheoremInput
instance Defaultable UpdateTheoremInput where
  defaultFor _ = error "No default for UpdateTheoremInput"

data PatchInput = PatchInput
  { branch :: Text
  , sha    :: Text
  } deriving (Show, Generic)

instance FromValue PatchInput
instance HasAnnotatedInputType PatchInput
instance Defaultable PatchInput where
  defaultFor _ = error "No default for CreateSpaceInput"

