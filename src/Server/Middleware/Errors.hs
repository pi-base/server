module Server.Middleware.Errors
  ( middleware
  , errorResponse
  ) where

import Core

import Data.Aeson                         ((.=), encode, object)
import Data.Aeson.Types                   (Pair)
import Network.HTTP.Types.Status
import Network.Wai
import Servant.Server.Internal.ServantErr (responseServantErr)
import UnliftIO.Exception

middleware :: Env
           -> Application
           -> Request
           -> (Response -> IO ResponseReceived)
           -> IO ResponseReceived
middleware _env app req next =
  catches (app req next)
    [ rescued responseServantErr
    , rescued notAuthenticated
    , rescued notFoundError
    , rescued permissionError
    , rescued validationError
    ]
  where
    rescued :: Exception e => (e -> Response) -> Handler IO ResponseReceived
    rescued formatter = Handler $ next . formatter

notAuthenticated :: NotAuthenticated -> Response
notAuthenticated _ = errorResponse status401 []

notFoundError :: NotFoundError -> Response
notFoundError NotFoundError{..} = errorResponse status404
  [ "resource" .= nfResource
  , "id" .= nfIdentifier
  ]

permissionError :: PermissionError -> Response
permissionError BranchPermissionRequired{..} = errorResponse status403
  [ "branch"   .= branchName branch
  , "required" .= format required
  , "actual"   .= fmap format actual
  ]
  where
    format :: BranchAccess -> Text
    format BranchRead  = "read"
    format BranchWrite = "write"
    format BranchAdmin = "admin"

validationError :: ValidationError -> Response
validationError (ValidationMessage msg) = errorResponse status422
  [ "message" .= msg
  ]

errorResponse :: Status -> [Pair] -> Response
errorResponse status pairs = responseLBS status headers body
  where
    headers = [("Content-Type", "application/json")]
    body = encode $ object $
      [ "error" .= True
      , "message" .= (decodeUtf8 $ statusMessage status)
      ] ++ pairs
