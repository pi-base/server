module Server.Middleware.Logging
  ( middleware
  ) where

import Server.Import

import Network.Wai
import Network.HTTP.Types

import qualified Logger

middleware :: Env -> Middleware
middleware env app req next = do
  let logger = env ^. envFoundation . appLogger

  Logger.log logger LevelDebug $ mconcat
    [ "Started "
    , decodeUtf8 (requestMethod req)
    , " "
    , decodeUtf8 (rawPathInfo req)
    , decodeUtf8 (rawQueryString req)
    ]

  app req $ \res -> do
    -- TODO: timing
    Logger.log logger LevelInfo $ mconcat
      [ show (statusCode $ responseStatus res)
      , " "
      , decodeUtf8 (requestMethod req)
      , " "
      , decodeUtf8 (rawPathInfo req)
      ]

    next res
