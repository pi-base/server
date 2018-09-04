{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
module Graph.Schema where

import Protolude

import GraphQL                         (SchemaRoot, VariableValues)
import GraphQL.API
import GraphQL.Internal.Validation     (QueryDocument, VariableValue)
import GraphQL.Value                   (Name)
import qualified GraphQL.Introspection as Introspection

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
  '[ Field "uid"             Text
   , Field "name"            Text
   , Field "aliases"         (List Text)
   , Field "references"      (List Citation)
   , Field "description"     Text
   , Field "proofOfTopology" (Maybe Text)
   , Field "traits"          (List Trait)
   ]

type Property = Object "Property" '[]
  '[ Field "uid"             Text
   , Field "name"            Text
   , Field "aliases"         (List Text)
   , Field "references"      (List Citation)
   , Field "description"     Text
   ]

type Trait = Object "Trait" '[]
  '[ Field "property"    Property
   , Field "value"       Bool
   , Field "references"  (List Citation)
   , Field "description" Text
   , Field "deduced"     Bool
   ]

type Theorem = Object "Theorem" '[]
  '[ Field "uid"         Text
   , Field "if"          Text
   , Field "then"        Text
   , Field "references"  (List Citation)
   , Field "description" Text
   ]

type Citation = Object "Citation" '[]
  '[ Field "type"       Text -- doi | mr | wikipedia
   , Field "ref"        Text
   , Field "name"       Text
   ]

type Branch = Object "Branch" '[]
  '[ Field "name"       Text
   , Field "access"     Text -- TODO: enum
   , Field "sha"        Text
   ]

type User = Object "User" '[]
  '[ Field "name"       Text
   , Field "branches"   (List Branch)
   ]

type Error = Object "Error" '[]
  '[ Field "message"    Text
   ]

type Viewer = Object "Viewer" '[]
  '[ Field "version"    Text
   , Field "spaces"     (List Space)
   , Field "properties" (List Property)
   , Field "theorems"   (List Theorem)
   ]

type SubmitBranchResponse = Object "SubmitBranchResponse" '[]
  '[ Field "branch"     Text
   , Field "url"        Text
   ]

type QueryRoot = Object "Query" '[]
  '[ Introspection.SchemaField
   , Introspection.TypeField
   , Argument "version" (Maybe Text) :> Field "viewer" Viewer
   , Field "me" User
   ]

type MutationRoot = Object "Mutation" '[]
  '[ Argument "patch" PatchInput
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

   , Argument "input" ResetBranchInput :> Field "resetBranch" Viewer
   , Argument "input" BranchInput :> Field "submitBranch" SubmitBranchResponse
   , Argument "input" BranchInput :> Field "approveBranch" Viewer
   ]

type Root m = SchemaRoot m QueryRoot MutationRoot

-- Inputs

data CitationInput = CitationInput
  { name         :: Text
  , citationType :: Core.CitationType
  , ref          :: Text
  } deriving (Show, Generic)

data CreateSpaceInput = CreateSpaceInput
  { name        :: Text
  , description :: Maybe Text
  , references  :: Maybe [CitationInput]
  } deriving (Show, Generic)

data CreatePropertyInput = CreatePropertyInput
  { name        :: Text
  , description :: Maybe Text
  , references  :: Maybe [CitationInput]
  } deriving (Show, Generic)

data AssertTraitInput = AssertTraitInput
  { spaceId     :: SpaceId
  , propertyId  :: PropertyId
  , value       :: Bool
  , description :: Maybe Text
  , references  :: Maybe [CitationInput]
  } deriving (Show, Generic)

data AssertTheoremInput = AssertTheoremInput
  { antecedent  :: Formula PropertyId
  , consequent  :: Formula PropertyId
  , description :: Maybe Text
  , references  :: Maybe [CitationInput]
  } deriving (Show, Generic)

data ResetBranchInput = ResetBranchInput
  { branch :: Text
  , to     :: Text -- ref or sha
  } deriving (Show, Generic)

data BranchInput = BranchInput
  { branch :: Text
  } deriving (Show, Generic)

data UpdateSpaceInput = UpdateSpaceInput
  { uid         :: SpaceId
  , description :: Maybe Text
  , references  :: Maybe [CitationInput]
  } deriving (Show, Generic)

data UpdatePropertyInput = UpdatePropertyInput
  { uid         :: PropertyId
  , description :: Maybe Text
  , references  :: Maybe [CitationInput]
  } deriving (Show, Generic)

data UpdateTheoremInput = UpdateTheoremInput
  { uid         :: TheoremId
  , description :: Maybe Text
  , references  :: Maybe [CitationInput]
  } deriving (Show, Generic)

data UpdateTraitInput = UpdateTraitInput
  { spaceId     :: SpaceId
  , propertyId  :: PropertyId
  , description :: Maybe Text
  , references  :: Maybe [CitationInput]
  } deriving (Show, Generic)

data PatchInput = PatchInput
  { branch :: Text
  , sha    :: Text
  } deriving (Show, Generic)