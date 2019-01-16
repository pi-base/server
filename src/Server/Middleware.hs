module Server.Middleware
  ( Stack(..)
  , build
  , def
  ) where

import Server.Import

import Network.Wai

import qualified Server.Middleware.Auth    as Auth
import qualified Server.Middleware.Cors    as Cors
import qualified Server.Middleware.Logging as Logging
import qualified Server.Middleware.Errors  as Errors
import qualified Server.Middleware.Rollbar as Rollbar
import qualified Server.Middleware.Session as Session

data Stack = Stack
  { cors    :: Middleware
  , auth    :: Middleware
  , logger  :: Middleware
  , rollbar :: Middleware
  , errors  :: Middleware
  , session :: Middleware
  }

def :: Env -> IO Stack
def env = do
  session <- Session.makeMiddleware
  return Stack
    { cors    = Cors.middleware
    , auth    = Auth.middleware env
    , logger  = Logging.middleware env
    , rollbar = Rollbar.middleware env
    , errors  = Errors.middleware env
    , session = session
    }

build :: Stack -> Middleware
build Stack{..} = foldr (.) identity
  [ cors
  , auth
  , logger
  , rollbar
  , session
  , errors
  ]
