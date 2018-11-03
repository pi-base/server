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

data Stack = Stack
  { cors    :: Middleware
  , auth    :: Middleware
  , logger  :: Middleware
  , rollbar :: Middleware
  , errors  :: Middleware
  }

def :: Env -> Stack
def env = Stack
  { cors    = Cors.middleware
  , auth    = Auth.middleware env
  , logger  = Logging.middleware env
  , rollbar = Rollbar.middleware env
  , errors  = Errors.middleware env
  }

build :: Stack -> Middleware
build Stack{..} = foldr (.) identity
  [ cors
  , auth
  , logger
  , rollbar
  , errors
  ]
