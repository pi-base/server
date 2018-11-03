module Server.Middleware.Rollbar
  ( middleware
  ) where

import Server.Import

import           Data.Aeson                ((.=))
import qualified Data.Text                 as T
import           Network.HTTP.Types.Status (status500)
import           Network.Wai

import qualified Server.Middleware.Auth   as Auth
import           Server.Middleware.Errors (errorResponse)
import           Services.Rollbar         as Rollbar

middleware :: Env -> Middleware
middleware env app req next = catch
  (app req next)
  (handleFallback env req next)

handleFallback :: Env
               -> Request
               -> (Response -> IO ResponseReceived)
               -> SomeException
               -> IO ResponseReceived
handleFallback env req next err = do
  let
    settings = env ^. envSettings . rollbarSettings
    logger   = env ^. envFoundation . appLogger
    user     = Auth.requestUser req

  runApp env $ send settings logger $ Report
    { message = T.pack $ displayException err
    , context = Just "Uncaught error"
    , level   = LevelError
    , user    = user
    , custom  = Nothing
    , request = Just req
    }

  next $ errorResponse status500
           [ "message" .= (show err :: Text)
           ]
