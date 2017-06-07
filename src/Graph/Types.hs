{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Graph.Types where

import Core (Bool, Maybe, Text)

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

type Viewer = Object "Viewer" '[]
  '[ Field "__typename" Text
   , Field "version"    Text
   , Field "spaces"     (List Space)
   , Field "properties" (List Property)
   , Field "theorems"   (List Theorem)
   ]

