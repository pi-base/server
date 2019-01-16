module Server.Util
  ( redirectTo
  , redirectToFrontend
  ) where

import Server.Import

import Control.Lens (view)

redirectTo :: MonadError ServantErr m => Text -> m a
redirectTo location = throwError $ err303
  { errHeaders = [("Location", encodeUtf8 location)] }

redirectToFrontend :: App m => Maybe Text -> Text -> m ()
redirectToFrontend viewerUrl path = do
  url <- maybe getViewerUrlFromEnv return viewerUrl
  redirectTo $ url <> path
  where
    getViewerUrlFromEnv = view (envSettings . frontendUrl) <$> getEnv
