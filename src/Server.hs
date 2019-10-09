module Server
  ( Config(..)
  , build
  , start
  ) where

import Server.Import

import qualified Interpreter
import           Network.Wai                          as W
import qualified Network.Wai.Handler.Warp             as W
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Server.Api                           (Server, API, context, server)
import           Server.Class                         ()

data Config = Config
  { port        :: Int
  , dev         :: Bool
  , interpreter :: Interpreter.Config
  } deriving (Generic, Show, Eq)

start :: Config -> IO ()
start config = do
  (app, middleware, settings) <- build config
  W.runSettings settings $ middleware app

build :: Config -> IO (Application, Middleware, W.Settings)
build config = do
  env <- Interpreter.boot $ interpreter config

  let
    nt :: MonadIO m => Server a -> m a
    nt = Interpreter.serve . Interpreter.run env

    settings
      = W.setPort (port config)
      $ W.setOnException (\r e -> nt $ handleError r e)
      $ W.defaultSettings

    application
      = serveWithContext api (context nt)
      $ hoistServerWithContext api ctx nt server

    middleware = if dev config
      then logStdoutDev
      else identity

  return (application, middleware, settings)

handleError :: Maybe W.Request -> SomeException -> Server ()
handleError _ _ = do
  -- TODO:
  -- * setup and use a consistent logger -- http://hackage.haskell.org/package/co-log-polysemy ?
  -- * check config for rollbar access and report
  -- traceM (show e :: Text)
  return ()

api :: Proxy API
api = Proxy

ctx :: Proxy '[AuthHandler Request (Maybe User)]
ctx = Proxy
