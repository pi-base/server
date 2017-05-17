{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Handler.Graph where

import Import hiding (Handler, Field, Value, Query, User)
import qualified Import (Handler)

import qualified Data.Aeson as Aeson
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.List as CL
import qualified Data.Map as Map

import GraphQL
import GraphQL.API
import GraphQL.Resolver
import GraphQL.Value.ToValue

type User  = Object "User" '[] '[Field "name" Text, Field "number" Text]
type Query = Object "Query" '[] '[Field "me" User, Field "greet" Hello]

type Hello = Object "Hello" '[]
  '[ Argument "who" Text :> Field "greeting" Text ]

hello :: Handler IO Hello
hello = pure greeting
  where
    greeting who = pure $ "Hello " <> who

user :: Handler IO User
user = pure (name :<> number)
  where
    name = pure "Mort"
    number = pure "42"

query :: Handler IO Query
query = pure (user :<> hello)

run :: Text -> IO GraphQL.Response
run text = interpretQuery @Query query text Nothing Map.empty

postGraphR :: Import.Handler Aeson.Value
postGraphR = do
  body <- rawRequestBody $$ CT.decode CT.utf8 =$ CL.consume
  let q = intercalate "\n" body
  response <- liftIO . run $ q
  $(logInfo) $ "Query: " <> q
  -- $(logInfo) $ "Response: " <> (Aeson.encode $ toValue $ response)
  return . Aeson.toJSON $ toValue response
