module Server.Util
  ( redirectTo
  , redirectToFrontend
  ) where

import Server.Import

import Control.Lens (view)

redirectTo :: MonadError ServantErr m => Text -> m a
redirectTo location = throwError $ err303
  { errHeaders = [("Location", encodeUtf8 location)] }

redirectToFrontend :: App m => Text -> m ()
redirectToFrontend path = do
  url <- view (envSettings . frontendUrl) <$> getEnv
  redirectTo $ url <> path
