{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Graph.Types where

import Core (Version, Bool, Maybe, Text)

import GraphQL.API

type Space = Object "Space" '[]
  '[ Field "__typename"      Text
   , Field "uid"             Text
   , Field "slug"            Text
   , Field "name"            Text
   , Field "description"     Text
   , Field "proofOfTopology" (Maybe Text)
   , Field "traits"          (List Trait)
   ]

type Property = Object "Property" '[]
  '[ Field "__typename"      Text
   , Field "uid"             Text
   , Field "slug"            Text
   , Field "name"            Text
   , Field "description"     Text
   ]

type Trait = Object "Trait" '[]
  '[ Field "__typename" Text
   , Field "property"   Property
   , Field "value"      Bool
   ]

type Theorem = Object "Theorem" '[]
  '[ Field "__typename"  Text
   , Field "uid"         Text
   , Field "if"          Text
   , Field "then"        Text
   , Field "description" Text
   ]

type User = Object "User" '[]
  '[ Field "__typename" Text
   , Field "name"       Text
   ]

type Error = Object "Error" '[]
  '[ Field "__typename" Text
   , Field "message"    Text
   ]

type SpaceOrError = Union "SpaceOrError" '[Space, Error]
type PropertyOrError = Union "PropertyOrError" '[Property, Error]

type Viewer = Object "Viewer" '[]
  '[ Field "__typename" Text
   , Field "spaces"     (List Space)
   , Field "properties" (List Property)
   , Field "theorems"   (List Theorem)
   ]

type QueryRoot = Object "QueryRoot" '[]
  '[ Field "__typename" Text
   , Argument "version" (Maybe Version) :> Field "viewer" Viewer
   , Field "me" User
   -- Mutations
   , Argument "uid" Text :> Argument "description" Text :> Field "updateSpace" Space
   , Argument "uid" Text :> Argument "description" Text :> Field "updateProperty" Property
   ]
