{-# LANGUAGE TemplateHaskell #-}
module Handler.Graph (postGraphR) where

import Import
import Control.Lens hiding ((.=))
import Data.Aeson.Lens
import Graph.Root       (asJSON, interpret)
import Util             (pj)

import qualified Core

postGraphR :: Handler Value
postGraphR = flip catch handleError $ do
  body <- requireJsonBody
  $(logInfo) $ "[GraphQL] " <> (body ^. key "operationName" . _String)
  $(logDebug) $ "[GraphQL] " <> pj (body ^. key "variables" . _Object)
  response <- asJSON interpret body
  $(logDebug) $ "[GraphQL] " <> pj response
  return response

handleError :: Core.Error -> Handler Value
handleError e = do
  let status = errorStatus e
  when (status == status500) $ rollbarH e
  sendStatusJSON status $ object [ "error" .= e ]

errorStatus :: Core.Error -> Status
errorStatus (Core.PermissionError _) = status403
errorStatus (Core.NotFound _) = status404
errorStatus (Core.UnknownGitRef _) = status404
errorStatus (Core.ConflictError _) = status409
errorStatus _ = status500