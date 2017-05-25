module Handler.Graph (postGraphR) where

import Import
import qualified Graph

postGraphR :: Handler Value
postGraphR = do
  value <- requireJsonBody >>= Graph.query
  -- $(logDebug) $ "[GraphQL] " <> (toStrict . decodeUtf8 $ encode value)
  return value
