{-# LANGUAGE DataKinds, DeriveGeneric, DuplicateRecordFields, OverloadedStrings, PatternSynonyms, TypeOperators #-}
module Graph.Schema where

import Protolude

import GraphQL                     (VariableValues)
import GraphQL.API
import GraphQL.Internal.Validation (QueryDocument, VariableValue)
import GraphQL.Value               (Name)

import Core (Generic, SpaceId, PropertyId, TheoremId, Formula)
import qualified Core

type Query = QueryDocument VariableValue

-- Define newtypes so we can manage the FromJSON instances
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
   , Field "references"      (List Citation)
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
   , Field "references"      (List Citation)
   , Field "description"     Text
   ]

type Trait = Object "Trait" '[]
  '[ Field "__typename"  Text
   , Field "property"    Property
   , Field "value"       Bool
   , Field "references"  (List Citation)
   , Field "description" Text
   ]

type Theorem = Object "Theorem" '[]
  '[ Field "__typename"  Text
   , Field "uid"         Text
   , Field "if"          Text
   , Field "then"        Text
   , Field "references"  (List Citation)
   , Field "description" Text
   ]

type Citation = Object "Citation" '[]
  '[ Field "__typename" Text
   , Field "type"       Text -- doi | mr | wikipedia
   , Field "ref"        Text
   , Field "name"       Text
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

type SubmitBranchResponse = Object "SubmitBranchResponse" '[]
  '[ Field "__typename" Text
   , Field "branch"     Text
   , Field "url"        Text
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
     :> Field "updateTheorem"  Viewer

   , Argument "patch" PatchInput
     :> Argument "trait" UpdateTraitInput
     :> Field "updateTrait" Viewer

   , Argument "patch" PatchInput 
     :> Argument "trait" AssertTraitInput
     :> Field "assertTrait" Viewer

   , Argument "patch" PatchInput 
     :> Argument "theorem" AssertTheoremInput
     :> Field "assertTheorem" Viewer

   , Argument "input" ResetBranchInput :> Field "resetBranch" ResetBranchResponse
   , Argument "input" SubmitBranchInput :> Field "submitBranch" SubmitBranchResponse
   ]

-- Inputs

data CreateSpaceInput = CreateSpaceInput
  { uid         :: SpaceId
  , name        :: Text
  , description :: Text
  , references  :: Maybe [Core.Citation]
  } deriving (Show, Generic)

data CreatePropertyInput = CreatePropertyInput
  { uid         :: PropertyId
  , name        :: Text
  , description :: Text
  , references  :: Maybe [Core.Citation]
  } deriving (Show, Generic)

data AssertTraitInput = AssertTraitInput
  { spaceId     :: SpaceId
  , propertyId  :: PropertyId
  , value       :: Bool
  , description :: Text
  , references  :: Maybe [Core.Citation]
  } deriving (Show, Generic)

data AssertTheoremInput = AssertTheoremInput
  { uid :: TheoremId
  , antecedent  :: Formula PropertyId
  , consequent  :: Formula PropertyId
  , description :: Text
  , references  :: Maybe [Core.Citation]
  } deriving (Show, Generic)

data ResetBranchInput = ResetBranchInput
  { branch :: Text
  , to     :: Text -- ref or sha
  } deriving (Show, Generic)

data SubmitBranchInput = SubmitBranchInput
  { branch :: Text
  } deriving (Show, Generic)

data UpdateSpaceInput = UpdateSpaceInput
  { uid         :: SpaceId
  , description :: Maybe Text
  , references  :: Maybe [Core.Citation]
  } deriving (Show, Generic)

data UpdatePropertyInput = UpdatePropertyInput
  { uid         :: PropertyId
  , description :: Maybe Text
  , references  :: Maybe [Core.Citation]
  } deriving (Show, Generic)

data UpdateTheoremInput = UpdateTheoremInput
  { uid         :: TheoremId
  , description :: Maybe Text
  , references  :: Maybe [Core.Citation]
  } deriving (Show, Generic)

data UpdateTraitInput = UpdateTraitInput
  { spaceId     :: SpaceId
  , propertyId  :: PropertyId
  , description :: Maybe Text
  , references  :: Maybe [Core.Citation]
  } deriving (Show, Generic)

data PatchInput = PatchInput
  { branch :: Text
  , sha    :: Text
  } deriving (Show, Generic)