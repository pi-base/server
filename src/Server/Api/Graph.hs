{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module Server.Api.Graph
  ( API
  , server
  ) where

import Server.Import

import qualified Graph
import qualified Server.Middleware.Auth as Auth

type API =
  ( Vault :> ReqBody '[JSON] Graph.QueryData :> Post '[JSON] Graph.Response
  :<|> "schema" :> Get '[JSON] Text
  )

server :: App m => ServerT API m
server = handler
    :<|> schema

handler :: App m => Vault -> Graph.QueryData -> m Graph.Response
handler vault req@Graph.QueryData{..} = do
  $(logInfo)  $ "[GraphQL] " <> (fromMaybe query $ Graph.opName operation)
  $(logDebug) $ "[GraphQL] " <> pj variables

  env <- getEnv
  response <- Graph.run env (Auth.vaultUser vault) req

  $(logDebug) $ "[GraphQL] " <> pj response

  return response

schema :: App m => m Text
schema = return Graph.schema
