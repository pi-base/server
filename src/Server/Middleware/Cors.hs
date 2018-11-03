module Server.Middleware.Cors
  ( middleware
  ) where

import Server.Import

import Network.Wai                 (Middleware)
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors, simpleCorsResourcePolicy)

middleware :: Middleware
middleware = cors $ const $ Just $ simpleCorsResourcePolicy
  { corsMethods = ["GET" , "HEAD" , "POST", "OPTIONS"]
  , corsRequestHeaders = ["content-type", "authorization"]
  }
