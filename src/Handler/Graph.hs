{-# LANGUAGE TemplateHaskell #-}
module Handler.Graph (postGraphR) where

import Import
import Control.Lens              hiding ((.=))
import Data.Aeson.Lens
import Graph.Root                (asJSON, interpreted)
import Network.HTTP.Types.Status (mkStatus)
import Util                      (pj)

import qualified Core

postGraphR :: Handler Value
postGraphR = do
  settings <- appSettings <$> getYesod
  body <- requireJsonBody
  $(logInfo) $ "[GraphQL] " <> (body ^. key "operationName" . _String)
  $(logDebug) $ "[GraphQL] " <> pj (body ^. key "variables" . _Object)
  asJSON (interpreted settings) body >>= \case
    Left e -> do
      let status = errorStatus e
      if (status == status500)
        then do
          rollbarH e
          $(logError) $ "[GraphQL] " <> pj e
        else
          $(logDebug) $ "[GraphQL] " <> pj e
      sendStatusJSON status $ object [ "error" .= e ]
    Right response -> do
      $(logDebug) $ "[GraphQL] " <> pj response
      return response

status422 :: Status
status422 = mkStatus 422 "Unprocessable Entity"

errorStatus :: Core.Error -> Status
errorStatus (Core.GraphError (Core.ExecutionErrors _)) = status400
errorStatus (Core.PermissionError _) = status403
errorStatus (Core.NotFound _) = status404
errorStatus (Core.UnknownGitRef _) = status404
errorStatus (Core.ConflictError _) = status409
errorStatus (Core.ValidationError _) = status422
errorStatus _ = status500