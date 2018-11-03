{-# LANGUAGE TemplateHaskell #-}
module Server
  ( Server.build
  , start
  ) where

import Server.Import

import Network.Wai.Handler.Warp (run)

import Config.Boot       (boot)
import Server.Api        (API, server)
import Server.Middleware as Middleware

start :: Settings -> IO ()
start settings = do
  env <- boot settings
  let
    app = Server.build (Middleware.def env) (runApp env)
    appPort = env ^. envSettings . port
  runApp env $ $(logInfo) $ "Application starting on port " <> show appPort
  run appPort app

build :: App m
      => Middleware.Stack
      -> (forall a. m a -> Handler a)
      -> Application
build stack runner = Middleware.build stack $ application $ runner

application :: App m => (forall a. m a -> Handler a) -> Application
application runner = serve api $ hoistServer api runner server
  where
    api :: Proxy API
    api = Proxy
