{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Server.Api
  ( API
  , server
  ) where

import Server.Import

import Data.Aeson

import qualified Build
import           Data.Store       as Store (getBaseVersion)
import qualified Server.Api.Auth  as Auth
import qualified Server.Api.Graph as Graph
import           Server.Util      (redirectToFrontend)

type API =
  ( Get '[JSON] Root
  :<|> "app"     :> Get '[JSON] ()
  :<|> "auth"    :> Auth.API
  :<|> "graphql" :> Graph.API
  )

-- FIXME: strip out "bearer " prefix
server :: App m => ServerT API m
server = root
  :<|> redirectToFrontend Nothing ""
  :<|> Auth.server
  :<|> Graph.server

instance ToJSON Root where
  toJSON Root{..} = object
    [ "startedAt" .= rootStart
    , "build" .= rootBuild
    , "data" .= object
      [ "head" .= rootVersion
      ]
    ]

data Root = Root
  { rootVersion :: Version
  , rootStart   :: UTCTime
  , rootBuild   :: Maybe Build.Info
  }

root :: App m => m Root
root = do
  env     <- getEnv
  version <- Store.getBaseVersion
  return Root
    { rootVersion = version
    , rootStart   = env ^. envBootTime
    , rootBuild   = env ^. envBuildInfo
    }
