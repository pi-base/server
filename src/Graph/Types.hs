{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , DuplicateRecordFields
  , OverloadedStrings
  , PatternSynonyms
  , TypeOperators
#-}
module Graph.Types where

import Prelude

import GraphQL                     (VariableValues)
import GraphQL.API
import GraphQL.Internal.Validation (QueryDocument, VariableValue)
import GraphQL.Value               (Name)

import Core (Generic, Text)

type Query = QueryDocument VariableValue

-- Define a newtypes so we can manage the FromJSON instances
newtype Operation = Operation { unOp :: Maybe Name }
  deriving (Eq, Ord, Show, Generic)
newtype Variables = Variables { unVar :: VariableValues }
  deriving (Eq, Show, Generic)

data QueryData = QueryData
  { operation :: Operation
  , query     :: Text
  , variables :: Variables
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

type Root = Object "QueryRoot" '[]
  '[ Field "__typename" Text
   , Argument "version" (Maybe Text) :> Field "viewer" Viewer
   , Field "me" User
   -- Mutations
   , Argument "patch" PatchInput 
     :> Argument "space" CreateSpaceInput
     :> Field "createSpace" Viewer
   , Argument "patch" PatchInput 
     :> Argument "property" CreatePropertyInput 
     :> Field "createProperty" Viewer
   , Argument "patch" PatchInput 
     :> Argument "space" UpdateSpaceInput
     :> Field "updateSpace" Viewer
   , Argument "patch" PatchInput 
     :> Argument "property" UpdatePropertyInput 
     :> Field "updateProperty" Viewer
   , Argument "patch" PatchInput 
     :> Argument "theorem" UpdateTheoremInput  
     :> Field "updateTheoem"  Viewer
   , Argument "patch" PatchInput 
     :> Argument "trait" AssertTraitInput
     :> Field "assertTrait" Viewer
   , Argument "patch" PatchInput 
     :> Argument "theorem" AssertTheoremInput
     :> Field "assertTheorem" Viewer
   , Argument "input" ResetBranchInput :> Field "resetBranch" ResetBranchResponse
   ]

-- Inputs

data CreateSpaceInput = CreateSpaceInput
  { name        :: Text
  , description :: Text
  } deriving (Show, Generic)

data CreatePropertyInput = CreatePropertyInput
  { name        :: Text
  , description :: Text
  } deriving (Show, Generic)

data AssertTraitInput = AssertTraitInput
  { spaceId     :: Text
  , propertyId  :: Text
  , value       :: Bool
  } deriving (Show, Generic)

data AssertTheoremInput = AssertTheoremInput
  { antecedent  :: Text
  , consequent  :: Text
  , description :: Text
  } deriving (Show, Generic)

data ResetBranchInput = ResetBranchInput
  { branch :: Text
  , to     :: Text -- ref or sha
  } deriving (Show, Generic)

data UpdateSpaceInput = UpdateSpaceInput
  { uid :: Text
  , description :: Text
  } deriving (Show, Generic)

data UpdatePropertyInput = UpdatePropertyInput
  { uid :: Text
  , description :: Text
  } deriving (Show, Generic)

data UpdateTheoremInput = UpdateTheoremInput
  { uid :: Text
  , description :: Text
  } deriving (Show, Generic)

data PatchInput = PatchInput
  { branch :: Text
  , sha    :: Text
  } deriving (Show, Generic)