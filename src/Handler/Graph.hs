{-# LANGUAGE TemplateHaskell #-}
module Handler.Graph (postGraphR) where

import Import
import Control.Lens              hiding ((.=))
import Data.Aeson.Lens
import Graph.Root                (asJSON, interpreted)
import Network.HTTP.Types.Status (mkStatus)
import Util                      (pj)

import qualified Data.HashMap.Strict as HM

import qualified Core
import qualified Services.Rollbar as Rollbar

postGraphR :: Handler Value
postGraphR = do
  settings <- appSettings <$> getYesod
  body <- requireJsonBody
  $(logInfo) $ "[GraphQL] " <> (body ^. key "operationName" . _String)
  $(logDebug) $ "[GraphQL] " <> pj (body ^. key "variables" . _Object)
  -- FIXME: compiled and cached queries only in production
  asJSON (interpreted settings) body >>= \case
    Left e -> do
      let status = errorStatus e
      if (status == status500)
        then do
          rollbar $ Rollbar.report
            { Rollbar.message = "Graph error"
            , Rollbar.level   = Rollbar.Error
            , Rollbar.custom  = Just $ HM.fromList 
              [ "body"  .= body 
              , "error" .= e
              ]
            }
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
errorStatus (Core.PermissionError _) = status403
errorStatus (Core.NotFound _) = status404
errorStatus (Core.UnknownGitRef _) = status404
errorStatus (Core.ConflictError _) = status409
errorStatus (Core.ValidationError _) = status422
errorStatus _ = status500