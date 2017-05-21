{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Graph.Types where

import Data.Maybe (Maybe)
import Data.Text  (Text)

import GraphQL.API

type Space = Object "Space" '[]
  '[ Field "__typename"      Text
   , Field "uid"             Text
   , Field "slug"            Text
   , Field "name"            Text
   , Field "description"     Text
   , Field "proofOfTopology" (Maybe Text)
   ]

type Property = Object "Property" '[]
  '[ Field "__typename"      Text
   , Field "uid"             Text
   , Field "slug"            Text
   , Field "name"            Text
   , Field "description"     Text
   ]

type User = Object "User" '[]
  '[ Field "__typename" Text
   , Field "name"       Text
   ]

type Error = Object "Error" '[]
  '[ Field "__typename" Text
   , Field "message" Text
   ]

type SpaceOrError = Union "SpaceOrError" '[Space, Error]
type PropertyOrError = Union "PropertyOrError" '[Property, Error]

type QueryRoot = Object "QueryRoot" '[]
  '[ Field "spaces"     (List Space)
   , Field "properties" (List Property)
   , Field "me"         User
   -- Mutations
   , (Argument "uid" Text :> Argument "description" Text :> Field "updateSpace" SpaceOrError)
   , (Argument "uid" Text :> Argument "description" Text :> Field "updateProperty" PropertyOrError)
   ]
